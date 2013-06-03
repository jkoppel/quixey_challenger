module Tarski.State.Variable where

import Control.Lens ( (.=), use, (+=) )

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified SMTLib2 as Smt

import Tarski.Sketch (sketchVars)
import Tarski.State.SymbState (counter, sketchState, Symb, varLab)
import Tarski.State.Manip (addDeclareConst)

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
