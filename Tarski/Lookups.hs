module Tarski.Lookups where

import Language.Java.Syntax
import qualified SMTLib2 as Smt
import qualified SMTLib2.Core as Smt
import qualified SMTLib2.BitVector as Smt
import qualified SMTLib2.Array as Smt

tBV :: Smt.Type
tBV = Smt.tBitVec 32

{- Op Lookups -}
opName :: Op -> (Smt.Expr -> Smt.Expr -> Smt.Expr)
opName Mult = Smt.bvmul
opName Add = Smt.bvadd
opName Sub = Smt.bvsub
opName Div = Smt.bvsdiv
opName Equal = (Smt.===)
opName LThan = Smt.bvslt
opName GThan = Smt.bvsgt
opName CAnd = Smt.and
opName COr = Smt.or

{- Type Lookups? -}
opType :: Op -> Smt.Type
opType Mult = tBV
opType Add = tBV
opType Sub = tBV
opType Div = tBV
opType Equal = Smt.tBool
opType LThan = Smt.tBool
opType GThan = Smt.tBool
opType CAnd = Smt.tBool
opType COr = Smt.tBool

symbType :: Type -> Smt.Type
symbType (PrimType IntT) = tBV
symbType (PrimType BooleanT) = Smt.tBool
symbType (RefType (ArrayType t)) = Smt.tArray tBV (symbType t)

-- Java literals I assume
litType :: Literal -> Smt.Type
litType (Int _) = tBV
litType (Boolean _) = Smt.tBool
