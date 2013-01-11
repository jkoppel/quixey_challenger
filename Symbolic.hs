module Symbolic where

import Control.Monad
import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import Language.Java.Syntax hiding (Assert)
import Language.Java.Pretty (prettyPrint)

import Sketch

import Debug.Trace

data ZType = ZInt | ZBool | ZArray ZType ZType
             deriving (Show, Eq)

data Z3 = Assert Z3
        | DeclareConst String ZType
        | ZIte Z3 Z3 Z3
        | ZVar String
        | BV32 Int
        | ZBinOp String Z3 Z3
        | ZSelect Z3 Z3
        | ZStore Z3 Z3 Z3
        | ZNot Z3
        | ConstArray Int
        | CheckSat
        | GetModel
        deriving (Show, Eq)
          

data SymbState = SymbState { counter :: Int,
                             z3 :: [Z3],
                             pathGuard :: [Z3],
                             retVar :: String,
                             varLab :: Map.Map String Int,
                             sketchState :: SketchState
                             } deriving (Show)

class Pretty a where
      pretty :: a -> String

instance Pretty ZType where
  pretty ZInt = "(_ BitVec 32)"
  pretty ZBool = "Bool"
  pretty (ZArray t1 t2) = "(Array " ++ (pretty t1) ++ " " ++ (pretty t2) ++ ")"

instance Pretty Z3 where
  pretty (Assert z) = "\n(assert " ++ (pretty z) ++ ")"
  pretty (DeclareConst v t) = "\n(declare-const " ++ v ++ " " ++ (pretty t) ++ ")"
  pretty (ZVar s) = s
  pretty (BV32 n) = if n >= 0
                      then
                        "(_ bv" ++ (show n) ++ " 32)"
                      else
                        "(bvneg " ++ (pretty (BV32 (-n))) ++ ")"
  pretty (ZSelect arr n) = "(select " ++ (pretty arr) ++ " " ++ (pretty n) ++ ")"                      
  pretty (ZStore arr n e) = "(store " ++ (pretty arr) ++ " " ++ (pretty n) ++ (pretty e) ++ ")"
  pretty (ZBinOp s z1 z2) = "(" ++ s ++ " " ++ (pretty z1) ++ " " ++ (pretty z2) ++ ")"
  pretty (ZNot z) = "(not " ++ (pretty z) ++ ")"
  pretty (ZIte z1 z2 z3) = "(ite " ++ (pretty z1) ++ " " ++ (pretty z2) ++ " " ++ (pretty z3) ++ ")"
  pretty (ConstArray n) = "((as const (Array (_ BitVec 32) (_ BitVec 32))) " ++ (show n) ++ ")"
  --(assert (= all1 ((as const (Array Int Int)) 1)))
  pretty CheckSat = "(check-sat)"
  pretty GetModel = "(get-model)"

startState :: SketchState -> SymbState
startState skst = SymbState {counter = 0,
                             z3 = [],
                             pathGuard = [],
                             retVar = "",
                             varLab = Map.insert "A" 0 Map.empty,
                             sketchState = skst}

type Symb = State SymbState

pushGuard :: Z3 -> Symb ()
pushGuard z = do g <- gets pathGuard
                 modify (\s -> s {pathGuard = z : g})
                 return ()

popGuard :: Symb ()
popGuard = do g <- gets pathGuard
              modify (\s -> s {pathGuard = tail g})
              return ()

getGuard :: Symb Z3
getGuard = do g <- gets pathGuard
              return $ foldr (ZBinOp "and") (ZVar "true") g
     
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

isSketchVar :: String -> Symb Bool
isSketchVar n = do vs <- gets (sketchVars . sketchState)
                   return $ Set.member n vs

vName v k = v ++ "_" ++ (show k)

getVar :: String -> Symb String
getVar var = do 
    m <- gets varLab
    b <- isSketchVar var
    if b
     then return var
     else case Map.lookup var m of
              Nothing -> error ("Looking up undeclared variable: " ++ var)
              Just k -> return $ vName var k

overwriteVar :: String -> ZType -> Symb ()
overwriteVar n t = do 
    m <- gets varLab
    let k' = case Map.lookup n m of
               Nothing -> 0
               Just k -> k+1
    modify (\s -> s {varLab = Map.insert n k' m})
    addZ3 $ DeclareConst (n ++ "_" ++ (show k')) t
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
                                g <- getGuard
                                r <- gets retVar
                                zAssert $ ZBinOp "=>" g (ZBinOp "=" (ZVar r) (ZVar v))
                                return ()
symbStmt (IfThenElse e s1 s2) = do
    v1 <- symbExp e
    m1 <- gets varLab
    pushGuard (ZVar v1)
    v2 <- symbStmt s1
    popGuard
    m2 <- gets varLab
    pushGuard $ ZNot (ZVar v1)
    v3 <- symbStmt s2
    popGuard
    m3 <- gets varLab
    g <- getGuard
    let g1 = ZBinOp "and" g (ZVar v1)
    let g2 = ZBinOp "and" g (ZNot (ZVar v1))
    mapM ((flip overwriteVar) ZInt . fst) (Map.toList m1)
    m4 <- gets varLab
    mapM (\(x,n) -> zAssert $ ZBinOp "=>" g1 (ZBinOp "=" (mVar x m4) (mVar x m2)))
         (Map.toList m1)
    mapM (\(x,n) -> zAssert $ ZBinOp "=>" g2 (ZBinOp "=" (mVar x m4) (branch2Var x m1 m2 m3)))
         (Map.toList m1)
    return ()
  where
   lookup x m = fromJust $ Map.lookup x m

   mVar x m = ZVar $ vName x (lookup x m)
   branch2Var x m1 m2 m3 = if lookup x m3 > lookup x m2
                            then
                              mVar x m3
                            else
                              mVar x m1


symbVarDecl :: Type -> VarDecl -> Symb ()
symbVarDecl t (VarDecl (VarId (Ident n)) vinit) = do 
    overwriteVar n ZInt
    n' <- getVar n
    case vinit of
         Nothing -> return ()
         Just (InitExp e) -> do 
            v <- symbExp e
            zAssert $ ZBinOp "=" (ZVar n') (ZVar v)

symbExp :: Exp -> Symb String
{-symbExp (ArrayCreate _ [_] 0) = do
    v <- tempVar $ ZArray ZInt ZInt
    return v
-}
symbExp (ArrayAccess (ArrayIndex arr n)) = do
    arr' <- symbExp arr
    n' <- symbExp n
    v <- tempVar ZInt
    upper_bound <- getVar "length"
    zAssert $ ZBinOp "=" (ZVar v) (select arr' n' upper_bound)
    return v

    where
    select :: String -> String -> String -> Z3
    select arr n upper =
        ZIte (ZBinOp "and" (ZBinOp "bvsge" (ZVar n) (symbLit $ Int 0)) (ZBinOp "bvslt" (ZVar n) (ZVar upper)))
             (ZSelect (ZVar arr) (ZVar n))
             (symbLit $ Int 0)
{-

ArrayCreate Type [Exp] Int
ArrayAccess (ArrayIndex arr n)


(declare-const a3 (Array Int Int))
(assert (= (select a1 x) x))
(assert (= (store a1 x y) a1))
-}

{-symbExp (Assign (ArrayLhs (ArrayIndex arr n)) EqualA e) = do
    arr' <- symbExp arr
    n' <- symbExp n
    overwriteVar arr'
    v <- getVar arr'
    e' <- symbExp e
    zAssert $ ZBinOp "=" (ZVar v) (ZStore (ZVar arr') (ZVar n') (ZVar e'))
    return v
-}    

symbExp (Assign (NameLhs (Name [Ident v])) EqualA e) = symbAssign v e
symbExp (Lit l) = do v <- tempVar ZInt
                     zAssert $ ZBinOp "=" (ZVar v) (symbLit l)
                     return v
symbExp (Cond e1 e2 e3) = do
    v1 <- symbExp e1
    v2 <- symbExp e2
    v3 <- symbExp e3
    v <- tempVar ZInt
    zAssert $ ZBinOp "=" (ZVar v) (ZIte (ZVar v1) (ZVar v2) (ZVar v3))
    return v
symbExp (BinOp e1 o e2) | opType o == ZBool = do
    v1 <- symbExp e1
    v2 <- symbExp e2
    v <- tempVar ZBool
    zAssert $ ZBinOp "=" (ZVar v) (ZBinOp (opName o) (ZVar v1) (ZVar v2))
    return v
symbExp (BinOp e1 o e2) = do v1 <- symbExp e1
                             v2 <- symbExp e2
                             v <- tempVar ZInt
                             zAssert $ ZBinOp "=" (ZVar v) (ZBinOp (opName o) (ZVar v1) (ZVar v2))
                             return v
symbExp (ExpName (Name [Ident n])) = getVar n
symbExp e = fail (prettyPrint e)
                             
symbAssign :: String -> Exp -> Symb String
symbAssign n e = do overwriteVar n ZInt
                    v <- getVar n
                    ev <- symbExp e
                    zAssert $ ZBinOp "=" (ZVar v) (ZVar ev)
                    return v

opName :: Op -> String
opName Mult = "bvmul"
opName Add = "bvadd"
opName Sub = "bvsub"
opName Div = "bvsdiv"
opName Equal = "="
opName LThan = "bvslt"
opName GThan = "bvsgt"
opName CAnd = "and"
opName COr = "or"

opType :: Op -> ZType
opType Mult = ZInt
opType Add = ZInt
opType Sub = ZInt
opType Div = ZInt
opType Equal = ZBool
opType LThan = ZBool
opType GThan = ZBool
opType CAnd = ZBool
opType COr = ZBool

symbType :: Type -> ZType
symbType (PrimType IntT) = ZInt
symbType (PrimType BooleanT) = ZBool
symbType (RefType (ArrayType t)) = ZArray ZInt (symbType t)

symbLit :: Literal -> Z3
symbLit (Int n) = BV32 $ fromInteger n
symbLit (Boolean True) = ZVar "true"
symbLit (Boolean False) = ZVar "false"

symbTest :: MemberDecl -> [Int] -> Int -> Symb ()
symbTest (MethodDecl _ _ _ _ args _ (MethodBody (Just b))) inputs output = do
  overwriteVar "retVar" ZInt
  r <- getVar "retVar"
  zAssert $ ZBinOp "=" (ZVar r) (BV32 output)

  overwriteVar "A" (ZArray ZInt ZInt)
  arr <- getVar "A"

  overwriteVar "length" ZInt
  len <- getVar "length"
  zAssert $ ZBinOp "=" (ZVar len) (BV32 $ head $ inputs)

  modify (\s -> s {retVar = r})
  mapM_ (\(e,i) -> do
    v <- symbExp e
    zAssert $ ZBinOp "=" (ZVar v) (ZSelect (ZVar arr) (symbLit $ Int i))) (zip expInputs [0..])
  symbBlock b  
  where
    expInputs = map (Lit . Int . toInteger) (tail inputs)
    

  {-mapM_ (uncurry symbAssign) (zip argNames expInputs)
  symbBlock b
  return ()
  where
    argNames = map (\(FormalParam _ _ _ (VarId (Ident n))) -> n) args
    -}

declare :: String -> Symb ()
declare n = do addZ3 $ DeclareConst n ZInt
               return ()

declareSketchVars :: Symb ()
declareSketchVars = do skvs <- gets (sketchVars . sketchState)
                       mapM_ declare (Set.toList skvs)
                       return ()

evalSketch :: MemberDecl -> SketchState -> [([Int], Int)] -> String
evalSketch dec skst tests = concat $ map pretty $ z3 $ execState runTests (startState skst)
 where
    runTests = do declareSketchVars
                  mapM_ (uncurry $ symbTest dec) tests
                  addZ3 CheckSat
                  addZ3 GetModel
                  return ()



{-
myMeth = MethodDecl [] [] Nothing (Ident "foo") [FormalParam [] (PrimType IntT) False (VarId $ Ident "x")] [] $ MethodBody $ Just $ Block [BlockStmt $ IfThenElse (Lit $ Boolean False) (ExpStmt $ Assign (NameLhs (Name [Ident "x"])) EqualA (Lit $ Int 1)) Empty]--,BlockStmt $ Return $ Just $ BinOp (ExpName (Name [Ident "x"])) Mult (Lit (Int 3))]
myTest = do symbTest myMeth [10] 20
            return ()
runTest = runState myTest (startState startSketchState)
-}
