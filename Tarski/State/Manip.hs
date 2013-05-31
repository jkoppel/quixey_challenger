module Tarski.State.Manip where

import Control.Lens ( (.=), use )

import qualified Data.Set as Set

import qualified SMTLib2 as Smt
import qualified SMTLib2.Core as Smt
import qualified SMTLib2.BitVector as Smt

import Tarski.Sketch (sketchVars)
import Tarski.State.SymbState (Symb, pathGuard, smt, sketchState)

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
declare n = addDeclareConst (Smt.N n) (Smt.tBitVec 32)

-- declare all the sketch variables in one go
declareSketchVars :: Symb ()
declareSketchVars = do skvs <- use (sketchState . sketchVars)
                       mapM_ declare (Set.toList skvs)
