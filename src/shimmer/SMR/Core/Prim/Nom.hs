
module SMR.Core.Prim.Nom where
import SMR.Core.Prim.Base
import SMR.Core.Exp.Base
import SMR.Core.World
import Data.IORef

-- | Primitive operators for nominal variables.
primOpsNom :: [PrimEval w]
primOpsNom
 = [ PP { name  = "nom'eq"
        , desc  = "check equality of two nominal variables"
        , eval  = \case [VNom n1, VNom n2]
                          -> Just $ if n1 == n2 then [VTrue] else [VFalse]
                        _ -> Nothing }

   , PE { name  = "nom'fresh"
        , desc  = "allocate a fresh nominal variable"
        , exec  =  \world
                -> \case [VUnit]
                          -> do ix  <- readIORef (worldNomGen world)
                                writeIORef (worldNomGen world) (ix + 1)
                                return $ Just [VNom ix]

                         _ -> return Nothing
        }
   ]

-- | Create a closing substitution for a nominal variable.
{-
primOpNomClose :: PrimEval w
primOpNomClose
 = PrimEval
        (PrimOp "nom'close")
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
