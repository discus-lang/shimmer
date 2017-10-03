
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

        -- Push train under key.
        XKey k21 x22
         -> Just $ XKey k21 (trainApply cs1 x22)

