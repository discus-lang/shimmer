
module SMR.Core.Exp.Compounds where
import SMR.Core.Exp.Base
import Data.Vector              (Vector)
import qualified Data.Vector    as V

-- | Make an application of a function to the given list of arguments,
--   suppressing the application of there are no arguments.
makeXApps :: Exp s p -> [Exp s p] -> Exp s p
makeXApps xFun xsArgs
 = XApp xFun (V.fromList xsArgs)


-- | Take an application of a function to a list of arguments.
takeXApps :: Exp s p -> Maybe (Exp s p, [Exp s p])
takeXApps xx
 = case xx of
        XApp x1@(XApp _ _) x2
          -> case takeXApps x1 of
                Just (f1, xs1) -> Just (f1, xs1 ++ V.toList x2)
                Nothing        -> Nothing

        XApp x1 x2
          -> Just (x1, V.toList x2)

        _ -> Nothing
