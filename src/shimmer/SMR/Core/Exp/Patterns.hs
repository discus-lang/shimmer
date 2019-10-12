
module SMR.Core.Exp.Patterns where
import SMR.Core.Exp.Base

pattern XSym n          = XRef (RSym n)
pattern XTxt n          = XRef (RTxt n)
pattern XMac n          = XRef (RMac n)
pattern XSet n          = XRef (RSet n)
pattern XNom n          = XRef (RNom n)
pattern XVal v          = XRef (RVal v)

pattern XApv xFun xsArg = XKey KApv (xFun : xsArg)
pattern XAps xFun  xArg = XKey KAps [xFun, xArg]
pattern XBox x          = XKey KBox [x]
pattern XRun x          = XKey KRun [x]
pattern XPrm p    xsArg = XKey (KPrm p) xsArg

pattern XUnit           = XVal VUnit

pattern XList xs        = XPrm PrimList xs
pattern XLIST vs        = XVal (VList vs)

pattern XBool b         = XVal (VBool b)
pattern XTrue           = XBool True
pattern XFalse          = XBool False

pattern XNat  n         = XVal (VNat  n)
pattern XInt  i         = XVal (VInt  i)

pattern XWord8  w       = XVal (VWord8  w)
pattern XWord16 w       = XVal (VWord16 w)
pattern XWord32 w       = XVal (VWord32 w)
pattern XWord64 w       = XVal (VWord64 w)

pattern XInt8   i       = XVal (VInt8   i)
pattern XInt16  i       = XVal (VInt16  i)
pattern XInt32  i       = XVal (VInt32  i)
pattern XInt64  i       = XVal (VInt64  i)

pattern XFloat32 f      = XVal (VFloat32 f)
pattern XFloat64 f      = XVal (VFloat64 f)

