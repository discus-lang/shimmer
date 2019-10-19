
module SMR.Core.Prim.Sym where
import SMR.Core.Prim.Base
import SMR.Core.Exp.Base

-- | Primitive operators for symbols.
primOpsSym :: [PrimEval w]
primOpsSym
 = [ PP { name  = "sym'eq"
        , desc  = "check equality of two symbols"
        , eval  = \case [VSym s1, VSym s2]
                          -> Just $ if s1 == s2 then [VTrue] else [VFalse]
                        _ -> Nothing
        }
   ]