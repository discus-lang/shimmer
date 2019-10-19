
module SMR.Core.Prim.List where
import SMR.Core.Prim.Base
import SMR.Core.Exp


-- | Primitive evaluators for list operators.
primOpsList :: [PrimEval w]
primOpsList
 = [ PP { name  = "list'cons"
        , desc  = "add an element to the front of a list"
        , eval  = \case [v, VList vs] -> Just [VList (v : vs)]
                        _ -> Nothing }

   , PP { name  = "list'uncons"
        , desc  = "split an element from the front of a list"
        , eval  = \case [VList (v : vs)] -> Just [v, VList vs]
                        _ -> Nothing }

   , PP { name  = "list'append"
        , desc  = "append two lists"
        , eval  = \case [VList vs1, VList vs2] -> Just [VList (vs1 ++ vs2)]
                        _ -> Nothing }
   ]
