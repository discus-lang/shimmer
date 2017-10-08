{-# LANGUAGE OverloadedStrings #-}
module SMR.Prim.Op.Nom where
import SMR.Prim.Op.Base
import SMR.Core.Exp.Base
import SMR.Core.World
import Data.IORef


-- | Primitive evalutor for nominal variable operators.
primOpsNom :: [PrimEval s Prim w]
primOpsNom
 = [ primOpNomEq
   , primOpNomFresh
   , primOpNomClose ]


-- | Check for equality of two nominal variables.
primOpNomEq :: PrimEval s Prim w
primOpNomEq
 = PrimEval
        (PrimOp "nom-eq")
        ("check equality of two nominal variables")
        [PVal, PVal] fn'
 where
        fn' _world as0
         | Just (XRef (RNom n1), as1) <- takeArgExp as0
         , Just (XRef (RNom n2), [])  <- takeArgExp as1
         = return $ Just
                  $ if n1 == n2 then XRef $ RPrm $ PrimLitBool True
                                else XRef $ RPrm $ PrimLitBool False
        fn' _world _
         = return $ Nothing


-- | Allocate a fresh nominal variable.
primOpNomFresh :: PrimEval s Prim w
primOpNomFresh
 = PrimEval
        (PrimOp "nom-fresh")
        "allocate a fresh nominal variable"
        [PVal] fn'
 where
        fn' world as0
         | Just (XRef (RPrm PrimTagUnit), []) <- takeArgExp as0
         = do   ix  <- readIORef (worldNomGen world)
                writeIORef (worldNomGen world) (ix + 1)
                return $ Just $ XRef (RNom ix)

        fn' _world _
         = do   return $ Nothing


-- | Create a closing substitution for a nominal variable.
primOpNomClose :: PrimEval s Prim w
primOpNomClose
 = PrimEval
        (PrimOp "nom-close")
        ("creating a closing substitution for a nominal variable")
        [PVal, PExp, PExp] fn'
 where
        fn' _world as0
         | Just (XRef (RNom n1), as1) <- takeArgExp as0
         , Just (x1, as2)  <- takeArgExp as1
         , Just (x2, [])   <- takeArgExp as2
         = return $ Just $ XSub [CSim (SSnv [BindNom n1 x1])] x2

        fn' _world _
         = return $ Nothing
