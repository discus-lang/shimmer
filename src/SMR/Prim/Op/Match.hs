{-# LANGUAGE OverloadedStrings #-}
module SMR.Prim.Op.Match where
import SMR.Core.Exp
import SMR.Prim.Op.Base


-- | Primitive matching operators.
primOpsMatch :: [PrimEval s Prim w]
primOpsMatch
 = [ primOpMatchSym
   , primOpMatchApp
   , primOpMatchAbs ]



-- | Match against a given symbol.
primOpMatchSym :: PrimEval s Prim w
primOpMatchSym
 = PrimEval
        (PrimOp "match-sym")
        "match a symbol"
        [PVal, PExp, PExp] fn'
 where
        fn' _world as0
         | Just (x1, as1) <- takeArgExp as0
         , Just (x2, as2) <- takeArgExp as1
         , Just (x3, [])  <- takeArgExp as2
         = case x1 of
                XRef (RSym _s1)
                  -> return $ Just $ XApp x2 [x1]
                _ -> return $ Just $ x3

        fn' _world _
         = return $ Nothing


-- | Match an application.
--   TODO(BL: pack the args into a list)
primOpMatchApp :: PrimEval s Prim w
primOpMatchApp
 = PrimEval
        (PrimOp "match-app")
        "match an application"
        [PVal, PExp, PExp] fn'
 where
        fn' _world as0
         | Just (x1, as1) <- takeArgExp as0
         , Just (x2, as2) <- takeArgExp as1
         , Just (x3, [])  <- takeArgExp as2
         = case x1 of
                XApp x11 xs12
                  -> return $ Just $ XApp x2 (x11 : xs12)
                _ -> return $ Just $ x3

        fn' _world _
         = return $ Nothing


-- Match an abstraction.
primOpMatchAbs :: PrimEval s Prim w
primOpMatchAbs
 = PrimEval
        (PrimOp "match-abs")
        "match an abstraction"
        [PVal, PExp, PExp] fn'
 where
        fn' _world as0
         | Just (x1, as1) <- takeArgExp as0
         , Just (x2, as2) <- takeArgExp as1
         , Just (x3, [])  <- takeArgExp as2
         = case x1 of
                XAbs _ps11 x12
                  -> return $ Just $ XApp x2 (XRef (RPrm (PrimLitNat 99)) : [x12])
                _ -> return $ Just $ x3

        fn' _world _
         = return $ Nothing


