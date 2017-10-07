
module SMR.Prim.Op where
import SMR.Prim.Op.Base
import SMR.Prim.Op.Bool
import Data.Text                (Text)
import Data.Set                 (Set)
import qualified Data.Set       as Set


primEvals :: [PrimEval s Prim]
primEvals
 = concat
 [ primOpsBool ]

primOpTextNames :: Set Text
primOpTextNames
 = Set.fromList [ n | PrimOp n <- map primEvalName $ primEvals ]