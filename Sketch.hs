module Sketch where

import qualified Data.Set as Set
import Data.Monoid

import Control.Monad.Reader
import Control.Monad.State

import Language.Java.Parser
import Language.Java.Syntax
import Language.KURE
import Language.KURE.Utilities
import Language.KURE.Injection

import Kure
import KureCong
import Mutate hiding (not)

data SketchState = SketchState {
                          sketchVars :: Set.Set String
                   }

findMethod' :: String -> TranslateJ MemberDecl MemberDecl
findMethod' n = translate $ \_ d -> case d of
                                        MethodDecl _ _ _ (Ident n') _ _ _ | n == n' -> return d
                                        _                                           -> fail "method not found"

findMethod :: String -> TranslateJ GenericJava MemberDecl
findMethod n = onetdT $ promoteT (findMethod' n)

getMethod :: String -> CompilationUnit -> MemberDecl
getMethod interest prog = runKureM id (error "did not find method") (apply (findMethod interest) initialContext (inject prog))

makeSketchExp :: MemberDecl -> (SketchState, Exp)
makeSketchExp _ = (SketchState {sketchVars = Set.singleton "sketch"},
                     ExpName $ Name [Ident "sketch"])

replaceExp' :: Int -> Exp -> Rewrite Context (ReaderT TypeMap (StateT Int KureM)) Exp
replaceExp' n f = translate $ \_ e -> do l <- lift nextLabel
                                         if l /= n
                                          then
                                           return e
                                          else
                                           do tm <- ask
                                              let goodMutations = filter (\m -> applicable m e tm) allMutations
                                              if not (is_int e tm)
                                               then
                                                 return e
                                               else
                                                return f
                                       

replaceExp :: Int -> Exp -> Rewrite Context (ReaderT TypeMap (StateT Int KureM)) GenericJava
replaceExp n e = anybuR $ promoteR $ replaceExp' n e

doReplaceExp :: MemberDecl -> Int -> Exp -> MemberDecl
doReplaceExp d i e = let tm = runKureM id (error "type map failed") (apply getTypeMap initialContext (inject d))
                         t = runReaderT (apply (replaceExp i e) initialContext (inject d)) tm
                         t' = evalState t 0 in
                     runKureM (\(GMemberDecl c) -> c) (error "memberdecl proj failed") t'

genSketches :: String -> String -> (SketchState, [MemberDecl])
genSketches src interest = case parser compilationUnit src of
                              Left _ -> error "Parse error"
                              Right tree -> let m = getMethod interest tree
                                                (skst, sexp) = makeSketchExp m
                                                nExp = getSum $ runKureM id (error "count exp failed") (apply countExp initialContext (inject m))
                                                sketches = [doReplaceExp m i sexp | i <- [0..(nExp-1)]] in
                                              (skst, sketches)
