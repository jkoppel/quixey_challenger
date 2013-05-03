module Symbolic where

import Control.Monad
import Control.Monad.State
import Control.Lens ( makeLenses, (^.), (.=), use, (+=), (-=) )

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import Language.Java.Syntax hiding (Assert)
import Language.Java.Pretty (prettyPrint)
import SMTLib2

import Debug.Trace

import Sketch

data ZType = ZInt | ZBool | ZArray ZType ZType
             deriving (Show, Eq)
            -- *tInt | *tBool | *tArray x y (takes two Type parameters) | tVar?

data Z3 = Assert Z3 -- *Command, CmdAssert
        | DeclareConst String ZType -- *Command, need to convert to DeclareFun
        | ZIte Z3 Z3 Z3 -- *ite, Expr
        | ZVar String -- identifier? Name?
        | BV32 Int -- bv :: Integer -> Integer -> Expr, bv num w, need a function to check if use bvneg
        | ZBinOp String Z3 Z3 -- * lots of stuff, bv or not?
        | ZSelect Z3 Z3 -- *select :: Expr -> Expr -> Expr
        | ZStore Z3 Z3 Z3 -- *store :: Expr -> Expr -> Expr -> Expr
        | ZNot Z3 -- *not (I think, or bvnot?)
        | ConstArray Int -- Array indextype valuetype, so how do you get a constant array?
        | CheckSat -- *Command (needs produce-models option to be true, before set-logic)
        | GetModel -- Probably Command. (get-value (x y z blah))
        deriving (Show, Eq)

-- looks like it's Type and Expr (Function and Name, but functions are Expr)
-- We should just work at the Commmand level?

data SymbState = SymbState { _counter :: Int,
                             _smt :: Script,
                             _pathGuard :: Exprs
                             _retVar :: String,
                             _varLab :: Map.Map String Int, -- SSA; Single Static Assignment
                             _sketchState :: SketchState,
                             _unrollDepth :: Int,
                             _maxUnrollDepth :: Int
                             } deriving (Show)

makeLenses ''SymbState

startState :: SketchState -> Int -> SymbState
startState skst maxunroll = SymbState {_counter = 0,
                                       _smt = [], -- this should start with logic and options
                                       _pathGuard = [],
                                       _retVar = "",
                                       _varLab = Map.insert "A" 0 Map.empty,
                                       _sketchState = skst,
                                       _unrollDepth = 0,
                                       _maxUnrollDepth = maxunroll}

type Symb = State SymbState

class Symbolic t v | t -> v where
  symb :: t -> Symb v

-- write a bv32 function... is this right?
bv32 :: Int -> Expr
bv32 n = if n >= 0
          then
            bv n 32 -- "(_ bv" ++ (show n) ++ " 32)".. _ is an identifier?
          else
            bvneg (bv32 (-n))

-- syntactic sugar
declareConst :: Name -> Type -> Command
declareConst n t = CmdDeclareFun n [] t



-- push the pathGuard by 1
pushGuard :: Expr -> Symb ()
pushGuard z = do g <- use pathGuard
                 pathGuard .= z : g
                 return ()

-- pop the pathGuard by 1
popGuard :: Symb ()
popGuard = do g <- use pathGuard
              pathGuard .= tail g
              return ()

-- what??
getGuard :: Symb Script
getGuard = do g <- use pathGuard
              return $ foldr and (ZVar "true") g


-- append to the smt list inside of SymbState. This should be a Command stack
addCmd :: Command -> Symb ()
addCmd e = do z <- use smt
             let z' = z ++ [e]
             smt .= z'
             return ()

-- given a type, make a temporary variable. increment counter. add to stack.
tempVar :: Type -> Symb String
tempVar t = do n <- use counter
               counter += 1
               let v = "var" ++ (show n)
               addCmd $ declareConst v t
               return v



-- likely fine as is. checks to see if variable name is member of sketch vars
isSketchVar :: String -> Symb Bool
isSketchVar n = do vs <- use (sketchState . sketchVars)
                   return $ Set.member n vs




-- variable name version
vName v k = v ++ "_" ++ (show k)

--
getVar :: String -> Symb String
getVar var = do
    m <- use varLab
    b <- isSketchVar var
    if b
     then return var
     else case Map.lookup var m of
              Nothing -> error ("Looking up undeclared variable: " ++ var)
              Just k -> return $ vName var k


-- looks like this takes a name and type and overwrites some variable in varLab, then declares in
-- the Command stack
overwriteVar :: String -> Type -> Symb ()
overwriteVar n t = do
    m <- use varLab
    let k' = case Map.lookup n m of
               Nothing -> 0
               Just k -> k+1
    varLab .= Map.insert n k' m
    addCmd $ declareConst (vName n k') t
    return ()


-- takes an expr and asserts it in the Command stack?
zAssert :: Expr -> Symb ()
zAssert e = addCmd $ CmdAssert e

-- put this stuff into the Symbolic class...
symbMethodDecl :: MemberDecl -> Symb ()
symbMethodDecl (MethodDecl _ _ _ _ args _ (MethodBody (Just b))) = do mapM_ symbFormalParam args
                                                                      symbBlock b
                                                                      return ()

-- er.. given a formal parameter, adds a constant to the stack?
symbFormalParam :: FormalParam -> Symb ()
symbFormalParam (FormalParam _ t _ (VarId (Ident n))) = do addCmd $ declareConst n (symbType t)
                                                           return ()

-- mass block statements
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
                                r <- use retVar
                                zAssert $ g ==> ((ZVar r) === (ZVar v))
                                return ()
symbStmt (IfThen e s) = symbStmt $ IfThenElse e s Empty
symbStmt (IfThenElse e s1 s2) = do
    v1 <- symbExp e
    m1 <- use varLab
    pushGuard (ZVar v1)
    v2 <- symbStmt s1
    popGuard
    m2 <- use varLab
    pushGuard $ not (ZVar v1)
    v3 <- symbStmt s2
    popGuard
    m3 <- use varLab
    g <- getGuard
    let g1 = and g (ZVar v1)
    let g2 = and g (not (ZVar v1))
    mapM ((flip overwriteVar) tInt . fst) (filter ((/='A') . head . fst) (Map.toList m1))
    m4 <- use varLab
    mapM (\(x,n) -> zAssert $ g1 ==> ((mVar x m4) === (mVar x m2)))
         (Map.toList m1)
    mapM (\(x,n) -> zAssert $ g2 ==> ((mVar x m4) === (branch2Var x m1 m2 m3)))
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
symbStmt (While e s) = do
       d <- use unrollDepth
       m <- use maxUnrollDepth
       if d >= m
        then
         do v <- symbExp e
            zAssert $ not (ZVar v)
            return ()
        else
         do unrollDepth +=1
            symbStmt $ IfThenElse e (StmtBlock $ Block [BlockStmt s, BlockStmt $ While e s]) Empty
            unrollDepth -= 1
            return ()
symbStmt s = fail (prettyPrint s)



-- overwrite and assert it's equal to 0?
symbVarDecl :: Type -> VarDecl -> Symb ()
symbVarDecl t (VarDecl (VarId (Ident n)) vinit) = do
    overwriteVar n tInt -- why isn't this t?
    n' <- getVar n
    zAssert $ ((ZVar n') === (BV32 0))

symbExp :: Exp -> Symb String
symbExp (ArrayAccess (ArrayIndex arr n)) = do
    arr' <- symbExp arr
    n' <- symbExp n
    v <- tempVar tInt
    upper_bound <- getVar "length"
    zAssert $ ((ZVar v) === (select arr' n' upper_bound))
    return v

    where
    select :: String -> String -> String -> Expr
    select arr n upper =
        ite (and (bvsge (ZVar n) (symbLit $ Int 0)) (bvslt (ZVar n) (ZVar upper)))
             (select (ZVar arr) (ZVar n))
             (symbLit $ Int 123)

symbExp (Assign (NameLhs (Name [Ident v])) AddA e) = symbExp $ Assign (NameLhs (Name [Ident v])) EqualA (BinOp e Add (ExpName (Name [Ident v])))
symbExp (Assign (NameLhs (Name [Ident v])) EqualA e) = symbAssign v e
symbExp (Lit l) = do v <- tempVar (litType l)
                     zAssert $ ((ZVar v) === (symbLit l))
                     return v
symbExp (Cond e1 e2 e3) = do
    v1 <- symbExp e1
    v2 <- symbExp e2
    v3 <- symbExp e3
    v <- tempVar tInt
    zAssert $ (ZVar v) === (ite (ZVar v1) (ZVar v2) (ZVar v3))
    return v
symbExp (BinOp e1 o e2) | opType o == tBool = do
    v1 <- symbExp e1
    v2 <- symbExp e2
    v <- tempVar tBool
    zAssert $ (ZVar v) ===  ((opName o) (ZVar v1) (ZVar v2))
    return v
symbExp (BinOp e1 o e2) = do v1 <- symbExp e1
                             v2 <- symbExp e2
                             v <- tempVar tInt
                             zAssert $ (ZVar v) === ((opName o) (ZVar v1) (ZVar v2))
                             return v
symbExp (ExpName (Name [Ident n])) = getVar n
symbExp (ExpName (Name xs)) = getVar "length"
symbExp e = fail ("BAD: " ++ show e)

symbAssign :: String -> Exp -> Symb String
symbAssign n e = do ev <- symbExp e
                    overwriteVar n tInt
                    v <- getVar n
                    zAssert $ (ZVar v) === (ZVar ev)
                    return v

-- is === correct? also, not bv versions of And and Or
opName :: Op -> String
opName Mult = bvmul
opName Add = bvadd
opName Sub = bvsub
opName Div = bvsdiv
opName Equal = (===)
opName LThan = bvslt
opName GThan = bvsgt
opName CAnd = and
opName COr = or

opType :: Op -> Type
opType Mult = tInt
opType Add = tInt
opType Sub = tInt
opType Div = tInt
opType Equal = tBool
opType LThan = tBool
opType GThan = tBool
opType CAnd = tBool
opType COr = tBool


-- Uh oh! That's bad; look up what PrimType and RefType are. Collision? Java..
symbType :: Type? -> Type
symbType (PrimType IntT) = tInt
symbType (PrimType BooleanT) = tBool
symbType (RefType (ArrayType t)) = tArray tInt (symbType t)

-- Java literals I assume
litType :: Literal -> Type
litType (Int _) = tInt
litType (Boolean _) = tBool


symbLit :: Literal -> Expr
symbLit (Int n) = BV32 $ fromInteger n
symbLit (Boolean True) = ZVar "true"
symbLit (Boolean False) = ZVar "false"

symbTest :: MemberDecl -> [Int] -> Int -> Symb ()
symbTest (MethodDecl _ _ _ _ args _ (MethodBody (Just b))) inputs output = do
  overwriteVar "retVar" tInt
  r <- getVar "retVar"
  zAssert $ (ZVar r) === (BV32 output)

  overwriteVar "A" (tArray tInt tInt)
  arr <- getVar "A"

  overwriteVar "length" tInt
  len <- getVar "length"
  zAssert $ (ZVar len) === (BV32 $ head $ inputs)

  retVar .= r
  mapM_ (\(e,i) -> do
    v <- symbExp e
    zAssert $ (ZVar v) === (select (ZVar arr) (symbLit $ Int i))) (zip expInputs [0..])
  symbBlock b
  where
    expInputs = map (Lit . Int . toInteger) (tail inputs)


declare :: String -> Symb ()
declare n = do addCmd $ declareConst n tInt
               return ()

declareSketchVars :: Symb ()
declareSketchVars = do skvs <- use (sketchState . sketchVars)
                       mapM_ declare (Set.toList skvs)
                       return ()

evalSketch :: MemberDecl -> SketchState -> [([Int], Int)] -> Int -> String
evalSketch dec skst tests maxunroll = concat $ map pretty $ (execState runTests (startState skst maxunroll)) ^. smt
 where
    runTests :: Symb ()
    runTests = do declareSketchVars
                  mapM_ (uncurry $ symbTest dec) tests
                  addCmd CmdCheckSat
                  addCmd GetModel
                  return ()

