module Symbolic where

import Control.Monad
import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Java.Syntax hiding (Assert)

data ZType = ZInt | ZBool
             deriving (Show, Eq)

data Z3 = Assert Z3
        | DeclareConst String ZType
        | ZVar String
        | BV32 Int
        | ZBinOp String Z3 Z3
        | CheckSat
        | GetModel
        deriving (Show, Eq)
          

data SymbState = SymbState { counter :: Int,
                             z3 :: [Z3],
                             pathGuard :: Z3,
                             retVar :: String,
                             varLab :: Map.Map String Int,
                             sketchVars :: Set.Set String
                             } deriving (Show)

class Pretty a where
      pretty :: a -> String

instance Pretty ZType where
  pretty ZInt = "(_ BitVec 32)"
  pretty ZBool = "Bool"

instance Pretty Z3 where
  pretty (Assert z) = "\n(assert " ++ (pretty z) ++ ")"
  pretty (DeclareConst v t) = "\n(declare-const " ++ v ++ " " ++ (pretty t) ++ ")"
  pretty (ZVar s) = s
  pretty (BV32 n) = "(_ bv" ++ (show n) ++ " 32)"
  pretty (ZBinOp s z1 z2) = "(" ++ s ++ " " ++ (pretty z1) ++ " " ++ (pretty z2) ++ ")"
  pretty CheckSat = "(check-sat)"
  pretty GetModel = "(get-model)"

startState :: SymbState
startState = SymbState {counter = 0,
                        z3 = [],
                        pathGuard = ZVar "true",
                        retVar = "",
                        varLab = Map.empty,
                        sketchVars :: Set.empty}

type Symb = State SymbState

addZ3 :: Z3 -> Symb ()
addZ3 e = do z <- gets z3
             let z' = z ++ [e]
             modify (\s -> s {z3 = z'})
             return ()

tempVar :: ZType -> Symb String
tempVar t = do n <- gets counter
               modify (\s -> s {counter=n+1})
               let v = "var" ++ (show n)
               addZ3 $ DeclareConst v t
               return v

getVar :: String -> Symb String
getVar n = do m <- gets varLab
              case Map.lookup n m of
                   Nothing -> error "Looking up undeclared variable"
                   Just k -> return $ n ++ "_" ++ (show k)

overwriteVar :: String -> Symb ()
overwriteVar n = do m <- gets varLab
                    let k' = case Map.lookup n m of
                               Nothing -> 0
                               Just k -> k+1
                    modify (\s -> s {varLab = Map.insert n k' m})
                    addZ3 $ DeclareConst (n ++ "_" ++ (show k')) ZInt
                    return ()

zAssert :: Z3 -> Symb ()
zAssert e = addZ3 $ Assert e

symbMethodDecl :: MemberDecl -> Symb ()
symbMethodDecl (MethodDecl _ _ _ _ args _ (MethodBody (Just b))) = do mapM_ symbFormalParam args
                                                                      symbBlock b
                                                                      return ()

symbFormalParam :: FormalParam -> Symb ()
symbFormalParam (FormalParam _ t _ (VarId (Ident n))) = do addZ3 $ DeclareConst n (symbType t)
                                                           return ()

symbBlock :: Block -> Symb ()
symbBlock (Block stms) = mapM_ symbBlockStmt stms

symbBlockStmt :: BlockStmt -> Symb ()
symbBlockStmt (LocalVars _ t decs) = mapM_ (symbVarDecl t) decs
symbBlockStmt (BlockStmt s) = symbStmt s

symbStmt :: Stmt -> Symb ()
symbStmt (StmtBlock b) = symbBlock b
symbStmt Empty = return ()
symbStmt (ExpStmt e) = do symbExp e
                          return ()
symbStmt (Return (Just e)) = do v <- symbExp e
                                g <- gets pathGuard
                                r <- gets retVar
                                zAssert $ ZBinOp "==>" g (ZBinOp "=" (ZVar r) (ZVar v))
                                return ()

symbVarDecl :: Type -> VarDecl -> Symb ()
symbVarDecl t (VarDecl (VarId (Ident n)) vinit) = do addZ3 $ DeclareConst n (symbType t)
                                                     case vinit of
                                                         Nothing -> return ()
                                                         Just (InitExp e) -> do v <- symbExp e
                                                                                overwriteVar n
                                                                                n' <- getVar n
                                                                                zAssert $ ZBinOp "=" (ZVar n) (ZVar v)
                                                                                return ()

symbExp :: Exp -> Symb String
symbExp (Lit l) = do v <- tempVar ZInt
                     zAssert $ ZBinOp "=" (ZVar v) (symbLit l)
                     return v
symbExp (BinOp e1 o e2) = do v1 <- symbExp e1
                             v2 <- symbExp e2
                             v <- tempVar ZInt
                             zAssert $ ZBinOp "=" (ZVar v) (ZBinOp (opName o) (ZVar v1) (ZVar v2))
                             return v
symbExp (ExpName (Name [Ident n])) = getVar n
                             
symbAssign :: String -> Exp -> Symb ()
symbAssign n e = do overwriteVar n
                    v <- getVar n
                    ev <- symbExp e
                    zAssert $ ZBinOp "=" (ZVar v) (ZVar ev)
                    return ()

opName :: Op -> String
opName Mult = "bvmul"
opName Add = "bvadd"
opName Sub = "bvsub"

symbType :: Type -> ZType
symbType (PrimType IntT) = ZInt
symbType (PrimType BooleanT) = ZBool

symbLit :: Literal -> Z3
symbLit (Int n) = BV32 $ fromInteger n
symbLit (Boolean True) = ZVar "true"
symbLit (Boolean False) = ZVar "false"

symbTest :: MemberDecl -> [Int] -> Int -> Symb ()
symbTest (MethodDecl _ _ _ _ args _ (MethodBody (Just b))) inputs output = do overwriteVar "retVar"
                                                                              r <- getVar "retVar"
                                                                              modify (\s -> s {retVar = r})
                                                                              mapM_ (uncurry symbAssign) (zip argNames expInputs)
                                                                              symbBlock b
                                                                              zAssert $ ZBinOp "=" (ZVar r) (BV32 output)
                                                                              return ()
  where
    argNames = map (\(FormalParam _ _ _ (VarId (Ident n))) -> n) args
    expInputs = map (Lit . Int . toInteger) inputs


myMeth = MethodDecl [] [] Nothing (Ident "foo") [FormalParam [] (PrimType IntT) False (VarId $ Ident "x")] [] $ MethodBody $ Just $ Block [BlockStmt $ Return $ Just $ BinOp (ExpName (Name [Ident "x"])) Mult (Lit (Int 3))]
myTest = do symbTest myMeth [1] 2
            symbTest myMeth [2] 4
            return ()
runTest = runState myTest startState