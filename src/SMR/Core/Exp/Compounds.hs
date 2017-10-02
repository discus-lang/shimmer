
module SMR.Core.Exp.Compounds where
import SMR.Core.Exp.Base
import Data.Vector              (Vector)
import qualified Data.Vector    as V

-- | Make an application of a function to the given list of arguments,
--   suppressing the application of there are no arguments.
makeXApps :: Exp s p -> [Exp s p] -> Exp s p
makeXApps xFun xsArgs
 = case flattenExps xsArgs of
        []              -> xFun
        xArg : xsArg    -> makeXApps (XApp xFun xArg) xsArg


-- | Take an application of a function to a list of arguments.
takeXApps :: Exp s p -> Maybe (Exp s p, [Exp s p])
takeXApps xx
 = case xx of
        XApp x1@(XApp _ _) x2
          -> case takeXApps x1 of
                Just (f1, xs1) -> Just (f1, xs1 ++ flattenExp x2)
                Nothing        -> Nothing

        XApp x1 x2
          -> Just (x1, flattenExp x2)

        _ -> Nothing


-- | Flatten any XRet nodes into a vector of expressions.
flattenExp :: Exp s p -> [Exp s p]
flattenExp xArg
 = case xArg of
        XRet xsArgs     -> flattenExps $ V.toList xsArgs
        _               -> [xArg]


-- | Flatten any XRet nodes in a list to a list of expressions.
flattenExps :: [Exp s p] -> [Exp s p]
flattenExps xsArgs
 = case xsArgs of
        []              -> []
        xArg : xsArgs'  -> flattenExp xArg ++ flattenExps xsArgs'

