
module SMR.Core.Prim.Sym where
import SMR.Core.Prim.Base
import SMR.Core.Exp.Base


-- | Primitive evaluator for symbol operators.
primOpsSym :: [PrimEval w]
primOpsSym
 = [ primOpSymEq ]


-- | Check equality of two symbols.
primOpSymEq :: PrimEval w
primOpSymEq
 = PrimEval
        (PrimOp "sym-eq")
        ("check equality of two symbols")
        fn'
 where
        fn' _world [XSym s1, XSym s2]
         = return $ Just $ if s1 == s2 then XTrue else XFalse
        fn' _world _
         = return $ Nothing
