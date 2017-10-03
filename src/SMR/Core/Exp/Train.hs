
module SMR.Core.Exp.Train where
import SMR.Core.Exp.Base
import Data.Maybe
import qualified Data.Text      as T
import qualified Data.Vector    as V


-- Train ----------------------------------------------------------------------
-- | Cons a car on the front of an existing train.
--
--   If the new car is empty it will be suppressed.
--
--   If the new car can be combined with the first car on the existing
--   train then it will be combined.
--
trainCons :: Car s p -> [Car s p] -> [Car s p]
trainCons c1 cs2
 | carIsEmpty c1 = cs2
 | otherwise
 = case cs2 of
        []
         -> c1 : []

        c2 : cs2'
         |  CUps ups1   <- c1
         ,  CUps ups2   <- c2
         -> CUps (upsCombine ups1 ups2) : cs2'

         |  otherwise
         -> c1 : cs2


-- | Append two trains.
trainAppend :: [Car s p] -> [Car s p] -> [Car s p]
trainAppend ccA ccB
 = case ccA of
        []        -> ccB
        cA : csA  -> trainAppend' cA csA ccB
 where
        trainAppend' c1 cs1 cc2
         = case cs1 of
                -- Combine the  state with the first car on the second train.
                []
                 -> trainCons c1 cc2

                -- Walk over the first train, combining ups's as we go.
                c1' : cs1'
                 |  CUps ups1  <- c1
                 ,  CUps ups1' <- c1'
                 -> trainAppend' (CUps (upsCombine ups1 ups1')) cs1' cc2

                 |  otherwise
                 -> c1 : (trainAppend' c1' cs1' cc2)


-- | Bump a train due to pushing it under an abstraction with the
--   given parameter names.
trainBump :: [Name] -> [Car s p] -> [Car s p]
trainBump ns cs
 = case cs of
        []     -> []

        CSim snv : cs'
         -> trainCons (CSim (snvBump ns snv)) $ trainBump ns cs'

        CRec snv : cs'
         -> trainCons (CRec (snvBump ns snv)) $ trainBump ns cs'

        CUps ups : cs'
         -> trainCons (CUps (upsBump ns ups)) $ trainBump ns cs'


-- | Wrap an expression in a substitution train.
--   If the expression is a plain
trainApply :: [Car s p] -> Exp s p -> Exp s p
trainApply cs1 xx
 | []  <- cs1
 = xx

 | otherwise
 = case xx of
        XRef (RMac n)   -> xx
        XRef (RSym n)   -> xx
        XRef (RPrm n)   -> xx
        XVar name depth -> trainApplyVar cs1 name depth
        XSub cs2  x2    -> trainApply (trainAppend (V.toList cs2) cs1) x2
        _               -> XSub (V.fromList cs1) xx

-- Apply a train onto a variable of a given name and depth.
trainApplyVar :: [Car s p] -> Name -> Int -> Exp s p
trainApplyVar cs name depth
 = case cs of
        []
         -> XVar name depth

        CSim snv : cs'
         -> trainApply cs' (snvApplyVar False snv name depth)

        CRec snv : cs'
         -> trainApply cs' (snvApplyVar True  snv name depth)

        CUps ups : cs'
         -> trainApply cs' (upsApplyVar ups name depth)


-- Car ------------------------------------------------------------------------
-- | Check if a substitution car is empty.
carIsEmpty :: Car s p -> Bool
carIsEmpty c
 = case c of
        CSim snv -> snvIsEmpty snv
        CRec snv -> snvIsEmpty snv
        CUps ups -> upsIsEmpty ups


-- Snv ------------------------------------------------------------------------
-- | Build a substitution from lists of names and arguments.
snvOfNamesArgs :: [Name] -> [Exp s p] -> Snv s p
snvOfNamesArgs ns xs
 = SSnv (V.zip  (V.zip (V.fromList ns) (V.replicate (length ns) 0))
                (V.fromList xs))


-- | Check if the given substitution is empty.
snvIsEmpty :: Snv s p -> Bool
snvIsEmpty (SSnv bs)
 = case V.toList bs of
        []      -> True
        _       -> False


-- | Bump a substitution due to pushing it under an abstraction with
--   the given parameter names.
snvBump :: [Name] -> Snv s p -> Snv s p
snvBump ns (SSnv ts)
 = SSnv $ V.fromList $ mapMaybe (snvBump1 ns) $ V.toList ts
 where
        snvBump1 names b
         | ((name, depth), x) <- b
         , elem name names
         = Nothing

         | ((name, depth), x) <- b
         = Just ( (name, depth + (if elem name names then 1 else 0))
                , upsApply
                   (UUps $ V.fromList
                        (map (\name' -> ((name', 0), 1)) names)) x)


-- | Wrap a train consisting of a single simultaneous substitution
--   around an expression.
snvApply :: Bool -> Snv s p -> Exp s p -> Exp s p
snvApply isRec snv@(SSnv bs) xx
 = case V.toList bs of
        []        -> xx
        _ | isRec -> trainApply (CRec snv : []) xx
        _         -> trainApply (CSim snv : []) xx


-- | Apply a substitution to a variable of a given name and depth.
snvApplyVar :: Bool -> Snv s p -> Name -> Int -> Exp s p
snvApplyVar isRec snv@(SSnv bs) name depth
 = case V.toList bs of
        []
         -> XVar name depth

        b'@((name', depth'), x') : bs'
         |  name  == name'
         ,  depth == depth'
         -> if isRec then XSub (V.fromList $ CRec snv : []) x'
                     else x'

         |  name   == name'
         ,  depth  >  depth'
         -> XVar name (depth - 1)

         |  otherwise
         -> snvApplyVar isRec (SSnv $ V.fromList bs') name depth


-- Ups ------------------------------------------------------------------------
-- | Check if the given ups is empty.
upsIsEmpty :: Ups -> Bool
upsIsEmpty (UUps bs)
 = case V.toList bs of
        []      -> True
        _       -> False


-- | Wrap an expression in a train consisting of a single ups.
upsApply :: Ups -> Exp s p -> Exp s p
upsApply ups@(UUps us) xx
 = case V.toList us of
        []      -> xx
        _       -> trainApply ((CUps ups) : []) xx


-- | Apply an ups to a variable.
upsApplyVar :: Ups -> Name -> Int -> Exp s n
upsApplyVar (UUps bs) name ix
 = case V.toList bs of
        []
         -> XVar name ix

        u'@((name', depth'), inc') : bs'
         |  name   == name'
         ,  depth' <= ix
         -> upsApplyVar (UUps $ V.fromList bs') name (ix + inc')

         |  otherwise
         -> upsApplyVar (UUps $ V.fromList bs') name ix


-- | Bump ups (substitution lifting) due to pushing it
--   under an absraction with the given named binders.
upsBump :: [Name] -> Ups -> Ups
upsBump ns (UUps bs)
 = UUps $ V.fromList $ mapMaybe (upsBump1 ns) $ V.toList bs
 where
        upsBump1 ns l
         | ((n, d), inc) <- l
         , elem n ns
         = if d == 0
                then Nothing
                else Just ((n, d + 1), inc)

         | otherwise
         = Just l


-- | Combine two lists of ups.
upsCombine :: Ups -> Ups -> Ups
upsCombine (UUps ts1) (UUps ts2)
 = UUps (V.fromList $ foldr upsCombineBump (V.toList ts2) (V.toList ts1))


-- | Combine a bump with an existing list of them.
--   Applying the result to an expression will achieve the same result as
--   applying the whole list and then the extra one.
upsCombineBump :: UpsBump -> [UpsBump] -> [UpsBump]
upsCombineBump b bs
 | ((name, depth), inc) <- b
 = case bs of
        -- We cannot combine the new bump with anything else,
        -- so add it to the end of the list.
        []
         -> [b]

        b'@((name', depth'), inc') : bs'
         -- Combine the new bump with an existing one of the same name.
         |  name  == name'
         ,  depth == depth'
         -> ((name, depth'), inc + inc') : bs'

         -- Try to combine the new bump with the tail of the list.
         |  otherwise
         -> b' : (upsCombineBump b bs')

