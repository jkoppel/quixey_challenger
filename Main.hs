{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, IncoherentInstances #-}

module Main where

import qualified Data.Map as Map

import Control.Applicative ( Applicative )
import Control.Monad ( liftM )
import Control.Monad.Error ( MonadError, throwError, catchError )
import Control.Monad.Reader ( MonadReader, runReaderT, ask, runReader )
import Control.Monad.Random ( MonadRandom )
import Control.Monad.State ( MonadState, get, modify )
import Data.Maybe ( fromJust )
import Data.Monoid ( mconcat, Sum (Sum))
import Debug.Trace

import System.Environment ( getArgs )
import System.Random ( getStdGen )

import Language.Java.Syntax
import Language.Java.Parser ( compilationUnit, parser )
import Language.Java.Pretty ( pretty )

import Language.KURE ( MonadCatch, catchM, translate, crushbuT, (<+), Translate, Rewrite, apply, constT, onebuR )
import Language.KURE.Injection ( promoteT, inject, promoteR )
import Language.KURE.Utilities ( KureMonad, runKureMonad )

import Kure
import KureCong ( Context, initialContext )

instance MonadError String KureMonad where
  throwError = fail
  catchError = catchM

instance MonadError String m => MonadCatch m where
  catchM = catchError


data JavaType = Base Type
              | Top
              deriving (Show, Eq)

type TypeMap = Map.Map Ident JavaType

nextLabel :: (Num a, MonadState a m) => m a
nextLabel = do n <- get
               modify (+1)
               return n

convertVarDeclId :: JavaType -> VarDeclId -> (Ident, JavaType)
convertVarDeclId t (VarId id) = (id, t)
--convertVarDeclId t (VarDeclArray v) = convertVarDeclId (Array t) v

getVarDeclId :: VarDecl -> VarDeclId
getVarDeclId (VarDecl v _) = v

localVarDecls :: TranslateJ BlockStmt TypeMap
localVarDecls = translate $ \_ v -> case v of
                     LocalVars _ t decs -> return $ mconcat $ map (uncurry Map.singleton . convertVarDeclId (Base t) . getVarDeclId) decs
                     _ -> return Map.empty

argDecls :: TranslateJ FormalParam TypeMap
argDecls = translate $ \_ a -> case a of
                    FormalParam _ t _ v -> return $ uncurry Map.singleton $ convertVarDeclId (Base t) v

getTypeMap :: TranslateJ GenericJava TypeMap
getTypeMap = crushbuT $ (promoteT localVarDecls) <+ (promoteT argDecls)

inferLit :: Literal -> JavaType
inferLit (Int _) = Base $ PrimType IntT
inferLit (Float _) = Base $ PrimType FloatT
inferLit (Double _) = Base $ PrimType DoubleT
inferLit (Boolean _) = Base $ PrimType BooleanT
inferLit (Char _) = Base $ PrimType CharT
inferLit (String _) = Base $ RefType $ ClassRefType $ ClassType [(Ident "String", [])]
inferLit Null = Top

inferOp :: JavaType -> Op -> JavaType
inferOp t o = case o of
                    Mult -> Base $ PrimType IntT
                    Div -> Base $ PrimType IntT
                    Rem -> Base $ PrimType IntT
                    Sub -> Base $ PrimType IntT
                    LShift -> Base $ PrimType IntT
                    RShift -> Base $ PrimType IntT
                    RRShift -> Base $ PrimType IntT
                    And -> Base $ PrimType IntT
                    Or -> Base $ PrimType IntT
                    Xor -> Base $ PrimType IntT
                    LThan -> Base $ PrimType BooleanT
                    GThan -> Base $ PrimType BooleanT
                    LThanE -> Base $ PrimType BooleanT
                    GThanE -> Base $ PrimType BooleanT
                    Equal -> Base $ PrimType BooleanT
                    NotEq -> Base $ PrimType BooleanT
                    CAnd -> Base $ PrimType BooleanT
                    COr -> Base $ PrimType BooleanT
                    Add -> case t of
                                Base (RefType _) -> t
                                t'               -> t'


inferExp :: MonadReader TypeMap m => Exp -> m JavaType
inferExp (Lit l) = return $ inferLit l
inferExp (ArrayAccess (ArrayIndex e _)) = do t <- inferExp e
                                             case t of
                                                  Base (RefType (ArrayType t')) -> return $ Base t'
                                                  _        -> fail "not an array"
inferExp (PostIncrement e) = inferExp e
inferExp (PreIncrement e) = inferExp e
inferExp (PostDecrement e) = inferExp e
inferExp (PreDecrement e) = inferExp e
inferExp (PrePlus e) = inferExp e
inferExp (PrePlus e) = inferExp e
inferExp (PreBitCompl e) = inferExp e
inferExp (PreNot e) = return $ Base $ PrimType BooleanT
inferExp (BinOp e o _) = do t <- inferExp e
                            return $ inferOp t o
inferExp (Cond _ e _) = inferExp e
inferExp (Assign _ _ e) = inferExp e
inferExp (ExpName (Name n)) = do bindings <- ask
                                 return $ fromJust $ Map.lookup (last n) bindings
inferExp _ = fail "unimplemented"

showExpTypes' :: MonadReader TypeMap m => Translate Context m Exp String
showExpTypes' = translate $ \_ e -> do t <- inferExp e
                                       return $ (show $ pretty e) ++ "   ::    " ++ (show t) ++ "\n\n"

showExpTypes :: (MonadReader TypeMap m, MonadCatch m, Applicative m) => Translate Context m GenericJava String
showExpTypes = crushbuT $ promoteT showExpTypes'

countExp' :: MonadCatch m => Translate Context m Exp (Sum Int)
countExp' = constT $ return $ Sum 1

countExp :: (MonadCatch m, Applicative m) => Translate Context m GenericJava (Sum Int)
countExp = crushbuT $ promoteT countExp'

data Mutation =  Mutation { applicable :: Exp -> TypeMap -> Bool,
                            mutate :: Exp -> Exp }

typ e m = runReader (inferExp e) m

is_int :: Exp -> TypeMap -> Bool
is_int e m = (typ e m) == (Base $ PrimType IntT)
is_bool e m = (typ e m) == (Base $ PrimType IntT)

plusOne = Mutation { applicable = is_int,
                     mutate = \e -> BinOp e Add (Lit $ Int 1) }
subOne = Mutation { applicable = is_int,
                     mutate = \e -> BinOp e Sub (Lit $ Int 1) }
zero = Mutation { applicable = is_int,
                  mutate = \e -> Lit $ Int 0 }
not = Mutation { applicable = is_bool,
                 mutate = \e -> PreNot }
left_proj = Mutation { applicable (BinOp e1 o e2) = True
                       applicable _ = False,
                       mutate (BinOp e1 o e2) = e1 }
right_proj = Mutation { applicable (BinOp e1 o e2) = True
                       applicable _ = False,
                       mutate (BinOp e1 o e2) = e2 }
strip_unop = Mutation { applicable (PostIncrement _) = True
                        applicable (PostDecrement _) = True
                        applicable (PreIncrement _) = True
                        applicable (PreDecrement _) = True
                        applicable (PrePlus _) = True
                        applicable (PreMinus _) = True
                        applicable (PreBitCompl _) = True
                        applicable (PreNot _) = True
                        applicable _ = False,

                        mutate (PostIncrement e) = e
                        mutate (PostDecrement e) = e
                        mutate (PreIncrement e) = e
                        mutate (PreDecrement e) = e
                        mutate (PrePlus e) = e
                        mutate (PreMinus e) = e
                        mutate (PreBitCompl e) = e
                        mutate (PreNot e) = e }

guardMutate :: (MonadRandom m, MonadReader Int m, MonadState Int m, MonadCatch m)  => Int -> Rewrite Context m Exp
guardMutate n = translate $ \_ e -> do l <- nextLabel
                                       if l /= n
                                        then
                                          fail ""
                                        else
                                         return e
                                       

--mutateGeneric :: (MonadRandom m, MonadReader m) => Int -> Rewrite Context m GenericJava
--mutateGeneric n = onebuR $ promoteR $ mutateExp' n

main :: IO ()
main = do fil <- liftM last getArgs
          str <- readFile fil
          g <- getStdGen
          case parser compilationUnit str of
               Left _ -> error "Parse error"
               Right tree -> do let tm = runKureMonad id (error "type map failed") (apply getTypeMap initialContext (inject tree))
                                putStrLn (runKureMonad id (error "showExpTypes failed") (runReaderT (apply showExpTypes initialContext (inject tree)) tm))
                                return ()
