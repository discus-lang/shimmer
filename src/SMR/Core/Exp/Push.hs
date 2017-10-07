
module SMR.Core.Exp.Push where
import SMR.Core.Exp.Train
import SMR.Core.Exp.Compounds
import SMR.Core.Exp.Base


-- | Push down any outermost substitution train to reveal the head constructor.
pushHead :: Exp s p -> Maybe (Exp s p)
pushHead xx
 = case xx of
        XRef _          -> Nothing
        XVar _ _        -> Nothing
        XAbs _ _        -> Nothing
        XApp _ _        -> Nothing
        XSub cs2 x2     -> pushTrain cs2 x2
        XKey _ _        -> Nothing


-- | Push down the left-most substitution train in an expression,
--   or 'Nothing' if there isn't one.
pushDeep :: Exp s p -> Maybe (Exp s p)
pushDeep xx
 = case xx of
        XRef _          -> Nothing
        XVar _ _        -> Nothing

        XKey k1 xs2
         | Just xs2'    <- pushDeepFirst xs2
         -> Just $ XKey k1 xs2'

         | otherwise    -> Nothing

        XApp x1 xs2
         |  Just x1'    <- pushDeep x1
         -> Just $ XApp x1' xs2

         |  Just xs2'   <- pushDeepFirst xs2
         -> Just $ XApp x1 xs2'

         |  otherwise   -> Nothing


        XAbs ns x
         -> case pushDeep x of
                Nothing -> Nothing
                Just x' -> Just (XAbs ns x')

        XSub cs1 x2     -> pushTrain cs1 x2


-- | Push down the first substiution train in the given list.
pushDeepFirst :: [Exp s p] -> Maybe [Exp s p]
pushDeepFirst [] = Nothing
pushDeepFirst (x : xs)
 = case pushDeep x of
        Nothing
         |  Just xs'    <- pushDeepFirst xs
         -> Just (x : xs')
         | otherwise    -> Nothing

        Just x'
         -> Just (x' : xs)


-- | Push a substitution train down into an expression to reveal
--   the head constructor.
pushTrain :: [Car s p] -> Exp s p -> Maybe (Exp s p)
pushTrain cs1 x2
 = case x2 of
        -- Unfold macro under a substitution.
        -- Macro and symbol bodies are closed,
        -- so we can drop the substitution.
        XRef (RMac n)   -> Just x2
        XRef (RSym n)   -> Just x2
        XRef (RPrm n)   -> Just x2

        -- Reference to some other thing.
        XRef _          -> Nothing

        -- Apply the train to a variable.
        XVar name depth
         -> Just $ trainApplyVar cs1 name depth

        -- Push train under key.
        XKey k21 x22
         -> Just $ XKey k21 (map (trainApply cs1) x22)

        -- Push train into both sides of an application.
        XApp x21 x22
         -> Just $ XApp (trainApply cs1 x21) (map (trainApply cs1) x22)

        -- Push train under abstraction.
        XAbs ps21 x22
         -> let ns21    = map nameOfParam ps21
                cs1'    = trainBump ns21 cs1
            in  Just $ XAbs ps21 (trainApply cs1' x22)

        -- Combine trains.
        XSub cs2 x22
         -> Just $ trainApply (cs2 ++ cs1) x22


