module SMR.Prim.Op
        ( primNames
        , primOps
        , primOpsBool
        , primOpsList
        , primOpsMatch
        , primOpsNat
        , primOpsNom
        , primOpsSym)
where
import SMR.Prim.Op.Base
import SMR.Prim.Op.Bool
import SMR.Prim.Op.Nat
import SMR.Prim.Op.Sym
import SMR.Prim.Op.Nom
import SMR.Prim.Op.List
import SMR.Prim.Op.Match
import Data.Text                (Text)
import Data.Set                 (Set)
import qualified Data.Set       as Set


-- | Set containing textual names of all the primitive operators.
primNames :: Set Text
primNames
 = Set.fromList [ n | PrimOp n <- map primEvalName $ primOps ]


-- | Evaluators for all the primitive operators.
primOps :: [PrimEval Text Prim w]
primOps
 = concat
        [ primOpsBool
        , primOpsNat
        , primOpsList
        , primOpsSym
        , primOpsNom
        , primOpsMatch ]


