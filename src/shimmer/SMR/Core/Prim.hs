
module SMR.Core.Prim
        ( module SMR.Core.Prim.Base
        , primNames
        , primOps
        , primOpsBool
        , primOpsList
        , primOpsMatch
        , primOpsNat
        , primOpsNom
        , primOpsSym)
where
import SMR.Core.Prim.Base
import SMR.Core.Prim.Bool
import SMR.Core.Prim.Nat
import SMR.Core.Prim.Sym
import SMR.Core.Prim.Nom
import SMR.Core.Prim.List
import SMR.Core.Prim.Match
import Data.Text                (Text)
import Data.Set                 (Set)
import qualified Data.Set       as Set


-- | Set containing textual names of all the primitive operators.
primNames :: Set Text
primNames
 = Set.fromList $ map name primOps

-- | Evaluators for all the primitive operators.
primOps :: [PrimEval w]
primOps
 = concat
        [ primOpsBool
        , primOpsNat
        , primOpsList
        , primOpsSym
        , primOpsNom
        , primOpsMatch ]


