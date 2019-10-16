
module SMR.Core.Exp.Compounds where
import SMR.Core.Exp.Patterns
import SMR.Core.Exp.Base


-- | Make an application of an expression to its arguments.
makeXApps :: Exp -> [Exp] -> Exp
makeXApps xFun [] = xFun
makeXApps xFun (xArg : xsArgs)
 = makeXApps (XApp xFun xArg) xsArgs

