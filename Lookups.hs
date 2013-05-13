module Lookups where

import Language.Java.Syntax
import qualified SMTLib2 as Smt
import qualified SMTLib2.Core as Smt
import qualified SMTLib2.Int as Smt
import qualified SMTLib2.BitVector as Smt
import qualified SMTLib2.Array as Smt

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
opType Mult = Smt.tInt
opType Add = Smt.tInt
opType Sub = Smt.tInt
opType Div = Smt.tInt
opType Equal = Smt.tBool
opType LThan = Smt.tBool
opType GThan = Smt.tBool
opType CAnd = Smt.tBool
opType COr = Smt.tBool

symbType :: Type -> Smt.Type
symbType (PrimType IntT) = Smt.tInt
symbType (PrimType BooleanT) = Smt.tBool
symbType (RefType (ArrayType t)) = Smt.tArray Smt.tInt (symbType t)

-- Java literals I assume
litType :: Literal -> Smt.Type
litType (Int _) = Smt.tInt
litType (Boolean _) = Smt.tBool
