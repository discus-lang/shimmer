{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
module SMR.Prim.Op.Match where
import SMR.Core.Exp
import SMR.Core.World
import SMR.Prim.Op.Base
import Data.IORef


-- | Primitive matching operators.
primOpsMatch :: [PrimEval s Prim w]
primOpsMatch
 = [ primOpMatchSym
   , primOpMatchApp
   , primOpMatchAbs
   , primOpMatchAbs1 ]



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
                  -> return $ Just $ XApp x3 [x1]
                _ -> return $ Just $ x2

        fn' _world _
         = return $ Nothing


-- | Match an application.
--   TODO: pack the args into a list
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
                XRef{}          -> return $ Nothing
                XKey{}          -> return $ Nothing
                XApp x11 xs12   -> return $ Just $ XApp x3 (x11 : xs12)
                XVar{}          -> return $ Nothing
                XAbs{}          -> return $ Just x2
                XSub{}          -> return $ Nothing

        fn' _world _
         = return $ Nothing



-- | Match all parameters of an abstraction.
primOpMatchAbs :: PrimEval s Prim w
primOpMatchAbs
 = PrimEval
        (PrimOp "match-abs")
        "match all parameters of an abstraction"
        [PVal, PExp, PExp] fn'
 where
        fn' world as0
         | Just (x1, as1) <- takeArgExp as0
         , Just (x2, as2) <- takeArgExp as1
         , Just (x3, [])  <- takeArgExp as2
         = case x1 of
            XAbs ps11 x12 -> fnAbs world x3 ps11 x12
            _             -> return $ Just $ x2

        fn' _world _
         = return Nothing

        newNom world _
         = do   ix <- atomicModifyIORef (worldNomGen world)
                   $  \ix -> (ix + 1, ix)

                return ix

        fnAbs world x2 ps11 x12
         = do   -- Create new variables for each of the parameters.
                ixs     <- mapM (newNom world) ps11

                let boolOfForm PVal = True
                    boolOfForm PExp = False

                let xIxs
                        = makeXList
                                [ makeXList
                                        [ XRef (RNom ix)
                                        , XRef (RPrm (PrimLitBool (boolOfForm $ formOfParam p))) ]
                                | ix <- ixs | p  <- ps11 ]

                let xBody
                        = XSub  [CSim  (SSnv [BindVar (nameOfParam p) 0 (XRef (RNom ix))
                                              | p  <- ps11 | ix <- ixs ])]
                                 x12

                return  $ Just
                        $ XApp x2 (xIxs : [xBody])


-- | Match the first parameter of an abstraction.
primOpMatchAbs1 :: PrimEval s Prim w
primOpMatchAbs1
 = PrimEval
        (PrimOp "match-abs1")
        "match the first parameter of an abstraction"
        [PVal, PExp, PExp] fn'
 where
        fn' world as0
         | Just (x1, as1) <- takeArgExp as0
         , Just (x2, as2) <- takeArgExp as1
         , Just (x3, [])  <- takeArgExp as2
         = case x1 of
            XRef{}        -> return $ Nothing
            XKey{}        -> return $ Nothing
            XApp{}        -> return $ Just x2
            XVar{}        -> return $ Nothing
            XAbs ps11 x12 -> fnAbs world x3 ps11 x12
            XSub{}        -> return $ Nothing

        fn' _world _
         = return Nothing

        newNom world _
         = do   ix <- atomicModifyIORef (worldNomGen world)
                   $  \ix -> (ix + 1, ix)

                return ix

        fnAbs _world _x2 [] _x12
         = return Nothing

        fnAbs world x2 (p1 : ps11) x12
         = do   ix      <- newNom world p1

                let boolOfForm PVal = True
                    boolOfForm PExp = False

                let xIx = makeXList
                        [ XRef (RNom ix)
                        , XRef (RPrm (PrimLitBool (boolOfForm $ formOfParam p1))) ]

                let xBody
                        = XSub [ CSim (SSnv [BindVar (nameOfParam p1) 0 (XRef (RNom ix))])]
                        $ makeXAbs ps11 x12

                return  $ Just
                        $ XApp x2 (xIx : [xBody])

