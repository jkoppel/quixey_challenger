module Symbolic where

import Control.Monad
import Control.Monad.State
import Control.Lens ( makeLenses, (^.), (.=), use, (+=), (-=) )

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import Language.Java.Syntax hiding (Assert)
import Language.Java.Pretty (prettyPrint)
import qualified SMTLib2 as Smt
import qualified SMTLib2.Core as Smt
import qualified SMTLib2.Int as Smt
import qualified SMTLib2.BitVector as Smt
import qualified SMTLib2.Array as Smt

import Debug.Trace

import Sketch
import Lookups (opName, opType, symbType, litType)

data SymbState = SymbState { _counter :: Int,
                             _smt :: [Smt.Command],
                             _pathGuard :: [Smt.Expr],
                             _retVar :: Smt.Name,
                             _varLab :: Map.Map String Int, -- SSA; Single Static Assignment
                             _sketchState :: SketchState,
                             _unrollDepth :: Int,
                             _maxUnrollDepth :: Int
                             }

makeLenses ''SymbState

startState :: SketchState -> Int -> SymbState
startState skst maxunroll = SymbState {_counter = 0,
                                       _smt = [], -- this should start with logic and options
                                       _pathGuard = [],
                                       _retVar = Smt.N "",
                                       _varLab = Map.insert "A" 0 Map.empty, -- prob not needed
                                       _sketchState = skst,
                                       _unrollDepth = 0,
                                       _maxUnrollDepth = maxunroll}

type Symb = State SymbState


{- Symbolic -}
class Symbolic t v | t -> v where
  symb :: t -> Symb v

instance Symbolic MemberDecl () where
--symbMethodDecl :: MemberDecl -> Symb ()
  symb (MethodDecl _ _ _ _ args _ (MethodBody (Just b))) = do mapM_ symb args
                                                              symb b

instance Symbolic FormalParam () where
--symbFormalParam :: FormalParam -> Symb ()
  symb (FormalParam _ t _ (VarId (Ident n))) = addDeclareConst (Smt.N n) (symbType t)

instance Symbolic Block () where
-- symbBlock :: Block -> Symb ()
  symb (Block stms) = mapM_ symb stms

instance Symbolic BlockStmt () where
-- symbBlockStmt :: BlockStmt -> Symb ()
  symb (LocalVars _ t decs) = mapM_ (symbVarDecl t) decs
  symb (BlockStmt s) = symb s

-- overwrite and assert it's equal to 0?
symbVarDecl :: Type -> VarDecl -> Symb ()
symbVarDecl t (VarDecl (VarId (Ident n)) vinit) = do
    overwriteVar n (symbType t) -- changed this from tInt
    n' <- getVar n
    addAssert $ ((smtVar n') Smt.=== (bv32 0))

instance Symbolic Stmt () where
-- symbStmt :: Stmt -> Symb ()
  symb (StmtBlock b) = symb b
  symb Empty = return ()
  symb (ExpStmt e) = do symb e
                        return ()
  symb (Return (Just e)) = do v <- symb e
                              g <- getGuard
                              r <- use retVar
                              addAssert $ g Smt.==> ((smtVar r) Smt.=== (smtVar v))
  symb (IfThen e s) = symb $ IfThenElse e s Empty
  symb (IfThenElse e s1 s2) = do
      v1 <- symb e
      m1 <- use varLab
      pushGuard (smtVar v1)
      v2 <- symb s1
      popGuard
      m2 <- use varLab
      pushGuard $ Smt.not (smtVar v1)
      v3 <- symb s2
      popGuard
      m3 <- use varLab
      g <- getGuard
      let g1 = Smt.and g (smtVar v1)
      let g2 = Smt.and g (Smt.not (smtVar v1))
      mapM_ ((flip overwriteVar) Smt.tInt . fst) (filter ((/='A') . head . fst) (Map.toList m1)) -- gonna get rid of the filter probably
      m4 <- use varLab
      mapM_ (\(x,n) -> addAssert $ g1 Smt.==> ((mVar x m4) Smt.=== (mVar x m2)))
           (Map.toList m1)
      mapM_ (\(x,n) -> addAssert $ g2 Smt.==> ((mVar x m4) Smt.=== (branch2Var x m1 m2 m3)))
           (Map.toList m1)
      -- oh god
    where
     lookup x m = fromJust $ Map.lookup x m

     mVar x m = smtVar $ Smt.N $ vName x (lookup x m)
     branch2Var x m1 m2 m3 = if lookup x m3 > lookup x m2
                              then
                                mVar x m3
                              else
                                mVar x m1
  symb (While e s) = do
         d <- use unrollDepth
         m <- use maxUnrollDepth
         if d >= m
          then
           do v <- symb e
              addAssert $ Smt.not (smtVar v)
          else
           do unrollDepth += 1
              symb $ IfThenElse e (StmtBlock $ Block [BlockStmt s, BlockStmt $ While e s]) Empty
              unrollDepth -= 1
  symb s = fail (prettyPrint s)

instance Symbolic Exp Smt.Name where
-- symbExp :: Exp -> Symb Smt.Name
  symb (ArrayAccess (ArrayIndex arr n)) = do
      arr' <- symb arr -- Exp
      n' <- symb n -- Exp
      v <- tempVar Smt.tInt -- ??
      upper_bound <- getVar "length"
      addAssert $ ((smtVar v) Smt.=== (select arr' n' upper_bound))
      return v

      where
          select :: Smt.Name -> Smt.Name -> Smt.Name -> Smt.Expr
          select arr n upper =
              Smt.ite (Smt.and (Smt.bvsge (smtVar n) (symbLit $ Int 0)) (Smt.bvslt (smtVar n) (smtVar upper)))
                   (Smt.select (smtVar arr) (smtVar n))
                   (symbLit $ Int 123)

  symb (Assign (NameLhs (Name [Ident v])) AddA e) = symb $ Assign (NameLhs (Name [Ident v])) EqualA (BinOp e Add (ExpName (Name [Ident v])))
  symb (Assign (NameLhs (Name [Ident v])) EqualA e) = symbAssign v e
  symb (Lit l) = do v <- tempVar (litType l)
                    addAssert $ ((smtVar v) Smt.=== (symbLit l))
                    return v
  symb (Cond e1 e2 e3) = do
      v1 <- symb e1
      v2 <- symb e2
      v3 <- symb e3
      v <- tempVar Smt.tInt
      addAssert $ (smtVar v) Smt.=== (Smt.ite (smtVar v1) (smtVar v2) (smtVar v3))
      return v
  symb (BinOp e1 o e2) | opType o == Smt.tBool = do
      v1 <- symb e1
      v2 <- symb e2
      v <- tempVar Smt.tBool
      addAssert $ (smtVar v) Smt.===  ((opName o) (smtVar v1) (smtVar v2))
      return v
  symb (BinOp e1 o e2) = do v1 <- symb e1
                            v2 <- symb e2
                            v <- tempVar Smt.tInt
                            addAssert $ (smtVar v) Smt.=== ((opName o) (smtVar v1) (smtVar v2))
                            return v
  symb (ExpName (Name [Ident n])) = getVar n
  symb (ExpName (Name xs)) = getVar "length"
  symb e = fail ("BAD: " ++ show e)

symbAssign :: String -> Exp -> Symb Smt.Name
symbAssign n e = do ev <- symb e
                    overwriteVar n Smt.tInt
                    v <- getVar n
                    addAssert $ (smtVar v) Smt.=== (smtVar ev)
                    return v

symbLit :: Literal -> Smt.Expr
symbLit (Int n) = bv32 $ toInteger n
symbLit (Boolean True) = Smt.true
symbLit (Boolean False) = Smt.false

symbTest :: MemberDecl -> [Int] -> Int -> Symb ()
symbTest (MethodDecl _ _ _ _ args _ (MethodBody (Just b))) inputs output = do
  overwriteVar "retVar" Smt.tInt
  r <- getVar "retVar"
  addAssert $ (smtVar r) Smt.=== (bv32 $ toInteger output)

  overwriteVar "A" (Smt.tArray Smt.tInt Smt.tInt)
  arr <- getVar "A"

  overwriteVar "length" Smt.tInt
  len <- getVar "length"
  addAssert $ (smtVar len) Smt.=== (bv32 $ toInteger $ head $ inputs)

  retVar .= r
  mapM_ (\(e,i) -> do
    v <- symb e
    addAssert $ (smtVar v) Smt.=== (Smt.select (smtVar arr) (symbLit $ Int i))) (zip expInputs [0..])
  symb b
  where
    expInputs = map (Lit . Int . toInteger) (tail inputs)


{- Misc Functions -}
-- pseudoconstructor
bv32 :: Integer -> Smt.Expr
bv32 n = if n >= 0
          then
            Smt.bv n 32 -- "(_ bv" ++ (show n) ++ " 32)".. _ is an identifier?
          else
            Smt.bvneg (bv32 (-n))

-- turn names into expressions!
smtVar :: Smt.Name -> Smt.Expr
smtVar n = Smt.App (Smt.I n []) Nothing []



{- Path Guards -}
pushGuard :: Smt.Expr -> Symb ()
pushGuard z = do g <- use pathGuard
                 pathGuard .= z : g

popGuard :: Symb ()
popGuard = do g <- use pathGuard
              pathGuard .= tail g

getGuard :: Symb Smt.Expr
getGuard = do g <- use pathGuard
              return $ foldr Smt.and Smt.true g


{- Commands -}
-- append to the smt list inside of SymbState. This should be a Command stack
addCmd :: Smt.Command -> Symb ()
addCmd e = do z <- use smt
              let z' = z ++ [e]
              smt .= z'

-- syntactic sugar
addDeclareConst :: Smt.Name -> Smt.Type -> Symb ()
addDeclareConst n t = addCmd $ Smt.CmdDeclareFun n [] t

-- takes an expr and asserts it in the Command stack?
addAssert :: Smt.Expr -> Symb ()
addAssert e = addCmd $ Smt.CmdAssert e

-- declare a constant
declare :: String -> Symb ()
declare n = addDeclareConst (Smt.N n) Smt.tInt

-- declare all the sketch variables in one go
declareSketchVars :: Symb ()
declareSketchVars = do skvs <- use (sketchState . sketchVars)
                       mapM_ declare (Set.toList skvs)


{- Variable Operations -}
-- given a type, make a temporary variable. increment counter. add to stack.
tempVar :: Smt.Type -> Symb Smt.Name
tempVar t = do n <- use counter
               counter += 1
               let v = Smt.N ("var" ++ (show n))
               addDeclareConst v t
               return v

-- likely fine as is. checks to see if variable name is member of sketch vars
isSketchVar :: String -> Symb Bool
isSketchVar n = do vs <- use (sketchState . sketchVars)
                   return $ Set.member n vs

-- variable name version
-- Smt.Name?
vName :: String -> Int -> String
vName v k = v ++ "_" ++ (show k)

-- finds a variable and returns it
getVar :: String -> Symb Smt.Name
getVar var = do
            m <- use varLab
            b <- isSketchVar var
            if b
             then return $ Smt.N var
             else case Map.lookup var m of
                      Nothing -> error ("Looking up undeclared variable: " ++ var)
                      Just k -> return $ Smt.N $ vName var k

-- looks like this takes a name and type and overwrites some variable in varLab, then declares in
-- the Command stack
overwriteVar :: String -> Smt.Type -> Symb ()
overwriteVar n t = do
    m <- use varLab
    let k' = case Map.lookup n m of
               Nothing -> 0
               Just k -> k+1
    varLab .= Map.insert n k' m
    addDeclareConst (Smt.N $ vName n k') t











{- Final Form! -}
evalSketch :: MemberDecl -> SketchState -> [([Int], Int)] -> Int -> String
evalSketch dec skst tests maxunroll = show $ Smt.pp $ Smt.Script $ (execState runTests (startState skst maxunroll)) ^. smt -- I bet that we can just remove the concat
 where
    runTests :: Symb ()
    runTests = do declareSketchVars
                  mapM_ (uncurry $ symbTest dec) tests
                  addCmd Smt.CmdCheckSat

