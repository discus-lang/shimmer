
module SMR.Core.Exp.Compounds where
import SMR.Core.Exp.Base


-- Apps -----------------------------------------------------------------------
-- | Make an application of a function to the given list of arguments,
--   suppressing the application of there are no arguments.
makeXApps :: Exp s p -> [Exp s p] -> Exp s p
makeXApps xFun []       = xFun
makeXApps xFun xsArgs   = XApp xFun xsArgs


-- | Take an application of a function to a list of arguments.
--   TODO(BL): fix rubbish list append complexity.
takeXApps :: Exp s p -> Maybe (Exp s p, [Exp s p])
takeXApps xx
 = case xx of
        XApp x1@(XApp _ _) x2
          -> case takeXApps x1 of
                Just (f1, xs1) -> Just (f1, xs1 ++ x2)
                Nothing        -> Nothing

        XApp x1 x2
          -> Just (x1, x2)

        _ -> Nothing


-- Abs ------------------------------------------------------------------------
-- | Make an abstraction,
--   short circuiting to the body if there are no parameters.
makeXAbs :: [Param] -> Exp s p -> Exp s p
makeXAbs [] xBody = xBody
makeXAbs ps xBody = XAbs ps xBody


-- Param ----------------------------------------------------------------------
-- | Get the name of a function parameter.
nameOfParam :: Param -> Name
nameOfParam p
 = case p of
        PParam n  _     -> n


-- | Get the argument form required by a parameter.
formOfParam :: Param -> Form
formOfParam p
 = case p of
        PParam _ f      -> f
