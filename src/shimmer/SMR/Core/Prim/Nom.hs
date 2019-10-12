
module SMR.Core.Prim.Nom where
import SMR.Core.Prim.Base
import SMR.Core.Exp.Base
import SMR.Core.World
import Data.IORef


-- | Primitive evalutor for nominal variable operators.
primOpsNom :: [PrimEval w]
primOpsNom
 = [ primOpNomEq
   , primOpNomFresh ]


-- | Check for equality of two nominal variables.
primOpNomEq :: PrimEval w
primOpNomEq
 = PrimEval
        (PrimOp "nom-eq")
        ("check equality of two nominal variables")
        fn'
 where
        fn' _world [XNom n1, XNom n2]
         = return $ Just $ if n1 == n2 then XTrue else XFalse
        fn' _world _
         = return $ Nothing


-- | Allocate a fresh nominal variable.
primOpNomFresh :: PrimEval w
primOpNomFresh
 = PrimEval
        (PrimOp "nom-fresh")
        "allocate a fresh nominal variable"
        fn'
 where
        fn' world [XUnit]
         = do   ix  <- readIORef (worldNomGen world)
                writeIORef (worldNomGen world) (ix + 1)
                return $ Just $ XNom ix

        fn' _world _
         = do   return $ Nothing


-- | Create a closing substitution for a nominal variable.
{-
primOpNomClose :: PrimEval w
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
-}
