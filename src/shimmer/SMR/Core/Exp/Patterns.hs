
module SMR.Core.Exp.Patterns where
import SMR.Core.Exp.Base


-- Terms ----------------------------------------------------------------------
pattern XRef r          = XVal (VRef r)
pattern XSym n          = XRef (RSym n)
pattern XTxt n          = XRef (RTxt n)
pattern XSet n          = XRef (RSet n)
pattern XNom n          = XRef (RNom n)

pattern XVec xs         = XKey KVec xs
pattern XPrm p     xArg = XKey (KPrm p) [xArg]
pattern XApp xFun xArgs = XKey KApp [xFun, xArgs]
pattern XApv xFun xsArg = XApp xFun (XVec xsArg)

pattern XList xs        = XPrm POList xs
pattern XLIST vs        = XVal (VList vs)

pattern XPrimVal pv     = XVal (VPrim pv)

pattern XUnit           = XPrimVal PVUnit

pattern XBool b         = XPrimVal (PVBool b)
pattern XTrue           = XBool True
pattern XFalse          = XBool False

pattern XNat  n         = XVal (VNat     n)
pattern XInt  i         = XVal (VInt     i)
pattern XWord8  w       = XVal (VWord8   w)
pattern XWord16 w       = XVal (VWord16  w)
pattern XWord32 w       = XVal (VWord32  w)
pattern XWord64 w       = XVal (VWord64  w)
pattern XInt8   i       = XVal (VInt8    i)
pattern XInt16  i       = XVal (VInt16   i)
pattern XInt32  i       = XVal (VInt32   i)
pattern XInt64  i       = XVal (VInt64   i)
pattern XFloat32 f      = XVal (VFloat32 f)
pattern XFloat64 f      = XVal (VFloat64 f)


-- Values ---------------------------------------------------------------------
pattern VSym n          = VRef (RSym n)
pattern VTxt n          = VRef (RTxt n)
pattern VSet n          = VRef (RSet n)
pattern VNom n          = VRef (RNom n)

pattern VUnit           = VPrim PVUnit

pattern VBool b         = VPrim (PVBool b)
pattern VTrue           = VBool True
pattern VFalse          = VBool False

pattern VNat  n         = VPrim (PVNat     n)
pattern VInt  i         = VPrim (PVInt     i)
pattern VWord8  w       = VPrim (PVWord8   w)
pattern VWord16 w       = VPrim (PVWord16  w)
pattern VWord32 w       = VPrim (PVWord32  w)
pattern VWord64 w       = VPrim (PVWord64  w)
pattern VInt8   i       = VPrim (PVInt8    i)
pattern VInt16  i       = VPrim (PVInt16   i)
pattern VInt32  i       = VPrim (PVInt32   i)
pattern VInt64  i       = VPrim (PVInt64   i)
pattern VFloat32 f      = VPrim (PVFloat32 f)
pattern VFloat64 f      = VPrim (PVFloat64 f)

