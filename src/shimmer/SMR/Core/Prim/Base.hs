
module SMR.Core.Prim.Base
        ( module SMR.Core.Exp
        , module SMR.Core.World
        , PrimEval(..))
where
import SMR.Core.Exp
import SMR.Core.World

-- | Primitive evaluator.
data PrimEval w
 -- | Pure operator.
 = PP   { name  :: Text                 -- ^ Operator name.
        , desc  :: Text                 -- ^ Operator description.
        , eval  :: [Val] -> Maybe [Val] -- ^ Pure evaluation.
        }

 | PE   { name  :: Text                 -- ^ Operator name
        , desc  :: Text                 -- ^ Operator description.
        , exec  :: World w -> [Val] -> IO (Maybe [Val])
 }

