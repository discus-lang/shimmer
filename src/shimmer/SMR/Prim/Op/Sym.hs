{-# LANGUAGE OverloadedStrings #-}
module SMR.Prim.Op.Sym where
import SMR.Prim.Op.Base
import SMR.Core.Exp.Base


-- | Primitive evaluator for symbol operators.
primOpsSym :: Eq s => [PrimEval s Prim w]
primOpsSym
 = [ primOpSymEq ]


-- | Check equality of two symbols.
primOpSymEq :: Eq s => PrimEval s Prim w
primOpSymEq
 = PrimEval
        (PrimOp "sym-eq")
        ("check equality of two symbols")
        [PVal, PVal] fn'
 where
        fn' _world as0
         | Just (XRef (RSym n1), as1) <- takeArgExp as0
         , Just (XRef (RSym n2), [])  <- takeArgExp as1
         = return $ Just
                  $ if n1 == n2 then XRef $ RPrm $ PrimLitBool True
                                else XRef $ RPrm $ PrimLitBool False
        fn' _world _
         = return $ Nothing
