
module SMR.Prim.Op where
import SMR.Prim.Op.Base
import SMR.Prim.Op.Bool
import SMR.Prim.Op.Nat
import Data.Text                (Text)
import Data.Set                 (Set)
import qualified Data.Set       as Set


primEvals :: [PrimEval s Prim]
primEvals
 = concat
 [ primOpsBool
 , primOpsNat]

primOpTextNames :: Set Text
primOpTextNames
 = Set.fromList [ n | PrimOp n <- map primEvalName $ primEvals ]