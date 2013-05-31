    {-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, IncoherentInstances #-}

-- binary search and use explicit imports

module Mutate where

import qualified Data.Map as Map

import Control.Applicative ( Applicative )
import Control.Monad ( liftM )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Error ( MonadError, throwError, catchError )
import Control.Monad.Reader ( MonadReader, runReaderT, ask, runReader, ReaderT )
import Control.Monad.Random ( MonadRandom, runRandT, evalRandT, getRandomR, RandT, getSplit )
import Control.Monad.State ( MonadState, get, modify, evalStateT, StateT )
import Data.Maybe ( fromJust )
import Data.Monoid ( mconcat, Sum (Sum), getSum)
import Debug.Trace

import System.Environment ( getArgs )
import System.Random ( getStdGen, randomRIO, RandomGen, randomR )

import Language.Java.Syntax
import Language.Java.Parser ( compilationUnit, parser )
import Language.Java.Pretty ( pretty )

import Language.KURE ( MonadCatch, catchM, translate, crushbuT, (<+), Translate, Rewrite, apply, constT, anybuR )
import Language.KURE.Injection ( promoteT, inject, promoteR )
import Language.KURE.Utilities ( KureM, runKureM )

import Kure
import KureCong ( Context, initialContext )

instance MonadError String KureM where
  throwError = fail
  catchError = catchM

instance MonadError String m => MonadCatch m where
  catchM = catchError


instance (MonadError e m, RandomGen g) => MonadError e (RandT g m) where
  throwError = lift . throwError

  m `catchError` f = do g <- getSplit
                        g' <- getSplit
                        let f' e = evalRandT (f e) g'
                        lift $ evalRandT m g `catchError` f'


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
                     _ -> fail "localVarDecls"

argDecls :: TranslateJ FormalParam TypeMap
argDecls = translate $ \_ a -> case a of
                    FormalParam _ t _ v -> return $ uncurry Map.singleton $ convertVarDeclId (Base t) v

methDecs :: TranslateJ MemberDecl TypeMap
methDecs = translate $ \_ m -> case m of
                     MethodDecl _ _ (Just t) n _ _ _ -> return $ Map.singleton n (Base t)
                     _                               -> fail "methDecs"

getTypeMap :: TranslateJ GenericJava TypeMap
getTypeMap = crushbuT $ (promoteT localVarDecls) <+ (promoteT argDecls) <+ (promoteT methDecs)


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
inferExp (PreMinus e) = inferExp e
inferExp (PreBitCompl e) = inferExp e
inferExp (PreNot e) = return $ Base $ PrimType BooleanT
inferExp (BinOp e o _) = do t <- inferExp e
                            return $ inferOp t o
inferExp (Cond _ e _) = inferExp e
inferExp (Assign _ _ e) = inferExp e
inferExp (ExpName (Name [_,_])) = return $ Base $ PrimType $ IntT
inferExp (ExpName (Name n)) = do bindings <- ask
                                 return $ maybe (error ("did not find " ++ (show n))) id $ Map.lookup (last n) bindings
inferExp (MethodInv (MethodCall (Name n) _)) = do bindings <- ask
                                                  return $ maybe Top id $ Map.lookup (last n) bindings
inferExp (InstanceCreation _ t _ _) = return $ Base $ RefType $ ClassRefType t
inferExp (ArrayCreate _ _ _) = return Top
inferExp _ = fail "unimplemented"

countExp' :: MonadCatch m => Translate Context m Exp (Sum Int)
countExp' = constT $ return $ Sum 1

countExp :: (MonadCatch m, Applicative m) => Translate Context m GenericJava (Sum Int)
countExp = crushbuT $ promoteT countExp'

typ e m = runReader (inferExp e) m


is_int :: Exp -> TypeMap -> Bool
is_int e m = (typ e m) == (Base $ PrimType IntT)
is_bool e m = (typ e m) == (Base $ PrimType IntT)


constantFold' :: Map.Map String Int -> Rewrite Context KureM Exp
constantFold' m = translate $ \_ e -> return $ fold_it e m

fold_it :: Exp -> Map.Map String Int -> Exp
fold_it e m = case e of
        ExpName (Name [Ident v]) ->
                  case Map.lookup v m of
                       Just n -> Lit $ Int (fromIntegral n)
                       Nothing -> e
        BinOp e1 Equal e2 -> case (e1', e2') of
            (Lit (Boolean b1), Lit (Boolean b2)) -> Lit $ Boolean $ b1==b2
            (Lit (Int n1), Lit (Int n2)) -> Lit $ Boolean $ n1==n2
            _ -> BinOp e1' Equal e2'
            where
            e1' = fold_it e1 m
            e2' = fold_it e2 m
        Cond e1 e2 e3 -> case e1' of
            Lit (Boolean True) -> e2'
            Lit (Boolean False) -> e3'
            _ -> Cond e1' e2' e3'
            where
            [e1', e2', e3'] = map (\x -> fold_it x m) [e1, e2, e3]
        _ -> e

constantFold'' :: Map.Map String Int -> Rewrite Context KureM GenericJava
constantFold'' m = anybuR $ promoteR (constantFold' m)

constantFold :: Map.Map String Int -> MemberDecl -> MemberDecl
constantFold m d = runKureM (\(GMemberDecl x) -> x) (error "constant folding failed") (apply (constantFold'' m) initialContext (inject d))
