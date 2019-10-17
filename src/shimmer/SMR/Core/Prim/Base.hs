
module SMR.Core.Prim.Base
        ( module SMR.Core.Exp
        , module SMR.Core.World
        , PrimEval(..))
where
import SMR.Core.Exp
import SMR.Core.World


-- | Primitive evaluator.
data PrimEval w
        = PrimEval
        { -- ^ Operator name.
          primEvalName  :: PrimOp

          -- ^ Operator description.
        , primEvalDesc  :: Text

          -- ^ Operator evaluation function.
        , primEvalFun  :: World w -> [Val] -> IO (Maybe [Val])
        }

