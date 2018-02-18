{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module SMR.Codec.Peek
        ( peekFileDecls
        , peekDecl
        , peekExp,   peekKey,     peekParam
        , peekCar,   peekSnvBind, peekUpsBump
        , peekRef
        , peekName,  peekBump,    peekNom
        , peekWord8, peekWord16,  peekWord32,  peekWord64)
where
import SMR.Core.Exp
import SMR.Prim.Op.Base

import qualified Foreign.Marshal.Utils          as F
import qualified Foreign.Marshal.Alloc          as F
import qualified Foreign.Storable               as F
import qualified Foreign.Ptr                    as F

import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.ByteString.Unsafe         as BS

import Control.Monad
import Foreign.Ptr
import Data.Text                                (Text)
import Data.Bits
import Data.Word


---------------------------------------------------------------------------------------------------
type Peek a = Ptr Word8 -> Int -> IO (a, Ptr Word8, Int)


---------------------------------------------------------------------------------------------------
-- | Peek a list of `Decl` from memory, including the SMR file header.
peekFileDecls :: Peek [Decl Text Prim]
peekFileDecls !p0 !n0
 = do   (b0, p1, n1) <- peekWord8 p0 n0
        (b1, p2, n2) <- peekWord8 p1 n1
        (b2, p3, n3) <- peekWord8 p2 n2
        (b3, p4, n4) <- peekWord8 p3 n3
        when ( b0 /= 0x53 || b1 /= 0x4d || b2 /= 0x52 || b3 /= 0x31)
         $ error "peekFileDecls: bad magic"

        (ds, p5, n5) <- peekList peekDecl p4 n4
        return (ds, p5, n5)
{-# NOINLINE peekFileDecls #-}


-- | Peek a `Decl` from memory.
peekDecl :: Peek (Decl Text Prim)
peekDecl !p0 !n0
 = do   (b0, p1, n1) <- peekWord8 p0 n0
        p1 `seq` case b0 of
         0xa0
          -> do (tx,  p2, n2) <- peekName p1 n1
                (x,   p3, n3) <- peekExp  p2 n2
                return (DeclMac tx x, p3, n3)

         0xa1
          -> do (tx,  p2, n2) <- peekName p1 n1
                (x,   p3, n3) <- peekExp  p2 n2
                return (DeclSet tx x, p3, n3)

         _ -> error "peekDecl: invalid header"
{-# NOINLINE peekDecl #-}


---------------------------------------------------------------------------------------------------
-- | Peek an `Exp` from memory.
peekExp :: Peek (Exp Text Prim)
peekExp !p0 !n0
 = do   (b0, p1, n1) <- peekWord8 p0 n0
        p1 `seq` case b0 of
         0xb0
          -> do (r,   p2, n2) <- peekRef p1 n1
                return  (XRef r, p2, n2)

         0xb1
          -> do (key, p2, n2) <- peekKey p1 n1
                (xx,  p3, n3) <- peekExp p2 n2
                return  (XKey key xx, p3, n3)

         0xb2
          -> do (x1,  p2, n2) <- peekExp p1 n1
                (xs,  p3, n3) <- peekList peekExp p2 n2
                return  (XApp x1 xs, p3, n3)

         0xb3
          -> do (n,   p2, n2) <- peekName p1 n1
                (i,   p3, n3) <- peekBump p2 n2
                return  (XVar n i, p3, n3)

         0xb4
          -> do (ps,  p2, n2) <- peekList peekParam p1 n1
                (x,   p3, n3) <- peekExp p2 n2
                return  (XAbs ps x, p3, n3)

         0xb5
          -> do (cs,  p2, n2) <- peekList peekCar p1 n1
                (x,   p3, n3) <- peekExp p2 n2
                return  (XSub cs x, p3, n3)

         -- Short circuit XRef.
         _
          -> do (r,   p1, n1) <- peekRef p0 n0
                return (XRef r, p1, n1)
{-# NOINLINE peekExp #-}


-- | Peek a `Key` from memory.
peekKey :: Peek Key
peekKey !p0 !n0
 = do   (b0, p1, n1) <- peekWord8 p0 n0
        p1 `seq` case b0 of
         0xba   -> return (KBox, p1, n1)
         0xbb   -> return (KRun, p1, n1)
         _      -> error $ "peekKey: invalid header"
{-# INLINE peekKey #-}


-- | Peek a `Param` from memory.
peekParam :: Peek Param
peekParam !p0 !n0
 = do   (b0, p1, n1) <- peekWord8 p0 n0
        p1 `seq` case b0 of
         0xbc
          -> do (tx, p2, n2) <- peekName p1 n1
                return (PParam tx PVal, p2, n2)

         0xbd
          -> do (tx, p2, n2) <- peekName p1 n1
                return (PParam tx PExp, p2, n2)

         _ -> error $ "peekParam: invalid header " ++ show b0 ++ " " ++ show p1
{-# INLINE peekParam #-}


-- | Peek a `Car` from memory.
peekCar :: Peek (Car Text Prim)
peekCar !p0 !n0
 = do   (b0, p1, n1) <- peekWord8 p0 n0
        p1 `seq` case b0 of
         0xc1
          -> do (sbs, p2, n2) <- peekList peekSnvBind p1 n1
                return (CSim (SSnv sbs), p2, n2)

         0xc2
          -> do (sbs, p2, n2) <- peekList peekSnvBind p1 n1
                return (CRec (SSnv sbs), p2, n2)

         0xc3
          -> do (ups, p2, n2) <- peekList peekUpsBump p1 n1
                return (CUps (UUps ups), p2, n2)

         _ -> error $ "peekCar: invalid header"
{-# INLINE peekCar #-}


-- | Peek an `SnvBind` from memory.
peekSnvBind :: Peek (SnvBind Text Prim)
peekSnvBind !p0 !n0
 = do   (b0, p1, n1) <- peekWord8 p0 n0
        p1 `seq` case b0 of
         0xca
          -> do (n, p2, n2) <- peekName p1 n1
                (d, p3, n3) <- peekBump p2 n2
                (x, p4, n4) <- peekExp  p3 n3
                return (BindVar n d x, p4, n4)

         0xcb
          -> do (n, p2, n2) <- peekNom  p1 n1
                (x, p3, n3) <- peekExp  p2 n2
                return (BindNom n x,   p3, n3)

         _ -> error $ "peekSnvBind: invalid header"
{-# INLINE peekSnvBind #-}


-- | Peek an `UpsBump` from memory.
peekUpsBump :: Peek UpsBump
peekUpsBump !p0 !n0
 = do   (b0, p1, n1) <- peekWord8 p0 n0
        when (b0 /= 0xcc) $ error $ "peekUpsBump: invalid header"
        (n,  p2, n2) <- peekName  p1 n1
        (d,  p3, n3) <- peekBump  p2 n2
        (i,  p4, n4) <- peekBump  p3 n3
        return  $ (((n, d), i), p4, n4)
{-# INLINE peekUpsBump #-}


---------------------------------------------------------------------------------------------------
-- | Peek a `Ref` from memory.
peekRef :: Peek (Ref Text Prim)
peekRef !p0 !n0
 = do   (b0, p1, n1) <- peekWord8 p0 n0
        p1 `seq` case b0 of
         0xd0
          -> do (tx, p2, n2) <- peekText p1 n1
                return (RSym tx, p2, n2)

         0xd1
          -> do (m,  p2, n2) <- peekPrim p1 n1
                return (RPrm m,  p2, n2)

         0xd2
          -> do (tx, p2, n2) <- peekText p1 n1
                return (RMac tx, p2, n2)

         0xd3
          -> do (tx, p2, n2) <- peekText p1 n1
                return (RSet tx, p2, n2)

         0xd4
          -> do (i,  p2, n2) <- peekNom  p1 n1
                return (RNom i,  p2, n2)

         -- TODO: short circuit Sym

         _ -> error "peekRef: invalid header"
{-# INLINE peekRef #-}


---------------------------------------------------------------------------------------------------
-- | Peek a `Name` from memory.
peekName :: Peek Name
peekName !p !n
 = do   peekText p n
{-# INLINE peekName #-}


-- | Peek a `Bump` counter from memory.
peekBump :: Peek Integer
peekBump !p0 !n0
 = do   (i, p1, n1) <- peekWord16 p0 n0
        return (fromIntegral i, p1, n1)
{-# INLINE peekBump #-}


-- | Peek a `Nom` from memory.
peekNom :: Peek Integer
peekNom !p0 !n0
 = do   (i, p1, n1) <- peekWord32 p0 n0
        return (fromIntegral i, p1, n1)
{-# INLINE peekNom #-}


---------------------------------------------------------------------------------------------------
-- | Peek a prim from memory.
peekPrim :: Peek Prim
peekPrim !p0 !n0
 | n0 >= 1
 = do   (b0, p1, n1) <- peekWord8' p0 n0
        p1 `seq` case b0 of
         0xda   -> return (PrimTagUnit,         p1, n1)
         0xdb   -> return (PrimLitBool True,    p1, n1)
         0xdc   -> return (PrimLitBool False,   p1, n1)

         0xdf
          -> do (tx, p2, n2) <- peekText p1 n1
                return  (PrimOp tx, p2, n2)

         0xef
          -> do (tx, p2, n2) <- peekText p1 n1
                case T.unpack tx of
                 "nat"
                  -> do (ls, p3, n3) <- peekList peekWord8 p2 n2
                        case ls of
                         [x0, x1, x2, x3, x4, x5, x6, x7]
                          -> do let w   =   to64 x0 `shiftL` 56
                                        .|. to64 x1 `shiftL` 48
                                        .|. to64 x2 `shiftL` 40
                                        .|. to64 x3 `shiftL` 32
                                        .|. to64 x4 `shiftL` 24
                                        .|. to64 x5 `shiftL` 16
                                        .|. to64 x6 `shiftL` 8
                                        .|. to64 x7
                                return (PrimLitNat $ fromIntegral w, p3, n3)
                         _ -> error "peekPrim: invalid payload"

                 s -> error $ "peekPrim: unknown tag " ++ show s

         _ -> error $ "peekPrim: invalid header"

 | otherwise
 = error "peekPrim: invalid header"
{-# INLINE peekPrim #-}


---------------------------------------------------------------------------------------------------
-- | Peek a list of things from memory.
peekList :: Peek a -> Peek [a]
peekList peekA p0 n0
 | n0 >= 1
 = do   (b0, _p1, n1) <- peekWord8' p0 n0
        case b0 of
         0xf1
          | n1 >= 1
          -> do nElems <- fmap fromIntegral $ peek8  p0 1
                go nElems [] (F.plusPtr p0 2) (n1 - 1)

         0xf2
          | n1 >= 2
          -> do nElems <- fmap fromIntegral $ peek16 p0 1
                go nElems [] (F.plusPtr p0 3) (n1 - 2)

         0xf3
          | n1 >= 4
          -> do nElems <- fmap fromIntegral $ peek32 p0 1
                go nElems [] (F.plusPtr p0 5) (n1 - 4)

         _ -> error "peekList: invalid header"

 | otherwise
 = error "peekList: invalid header"

 where  go (0 :: Int) acc p n
         = return (reverse acc, p, n)

        go i acc p n
         = do   (x, p', n') <- peekA p n
                go (i - 1) (x : acc) p' n'
        {-# NOINLINE go #-}

{-# INLINE peekList #-}


---------------------------------------------------------------------------------------------------
-- | Peek a text value from memory as UTF8 characters.
peekText :: Peek Text
peekText !p0 !n0
 | n0 >= 1
 = do   (b0, _, n1) <- peekWord8' p0 n0
        case b0 of
         0xf1
          | n1 >= 1
          -> do nBytes  <- fmap fromIntegral $ peek8 p0 1
                buf     <- F.mallocBytes nBytes
                let p2  =  F.plusPtr p0 2
                let n2  =  n0 - 2
                when (not (n2 >= nBytes)) $ error "peekText: pointer out of range"
                F.copyBytes buf p2 nBytes
                bs      <- BS.unsafePackMallocCStringLen (buf, nBytes)
                return (T.decodeUtf8 bs, F.plusPtr p2 nBytes, n2 - nBytes)

         0xf2
          -> do nBytes  <- fmap fromIntegral $ peek16 p0 1
                buf     <- F.mallocBytes nBytes
                let p2  =  F.plusPtr p0 3
                let n2  =  n0 - 3
                when (not (n2 >= nBytes)) $ error "peekText: pointer out of range"
                F.copyBytes buf p2 nBytes
                bs      <- BS.unsafePackMallocCStringLen (buf, nBytes)
                return (T.decodeUtf8 bs, F.plusPtr p2 nBytes, n2 - nBytes)

         0xf3
          -> do nBytes  <- fmap fromIntegral $ peek32 p0 1
                buf     <- F.mallocBytes nBytes
                let p2  =  F.plusPtr p0 5
                let n2  =  n0 - 5
                when (not (n2 >= nBytes)) $ error "peekText: pointer out of range"
                F.copyBytes buf p2 nBytes
                bs      <- BS.unsafePackMallocCStringLen (buf, nBytes)
                return (T.decodeUtf8 bs, F.plusPtr p2 nBytes, n2 - nBytes)

         _ -> error $ "peekText: invalid header"

 | otherwise
 = error "peekText: pointer out of range"
{-# NOINLINE peekText #-}

---------------------------------------------------------------------------------------------------
-- | Peek a `Word8` from memory, in network byte order, with bounds check.
peekWord8  :: Peek Word8
peekWord8 p n
 | n >= 1       = peekWord8' p n
 | otherwise    = error "peekWord8: pointer out of bounds"
{-# NOINLINE peekWord8 #-}


-- | Peek a `Word8` from memory, in network byte order, with no bounds check.
peekWord8' :: Peek Word8
peekWord8' p n
 = do   w  <- F.peek p
        return (w, F.plusPtr p 1, n - 1)
{-# INLINE peekWord8' #-}


-- | Peek a `Word16` from memory, in network byte order, with bounds check.
peekWord16  :: Peek Word16
peekWord16 p n
 | n >= 2       = peekWord16' p n
 | otherwise    = error "peekWord16: pointer out of bounds"
{-# NOINLINE peekWord16 #-}


-- | Peek a `Word16` from memory, in network byte order, with no bound check.
peekWord16' :: Peek Word16
peekWord16' p n
 = do   b0 <- fmap to16 $ peek8 p 0
        b1 <- fmap to16 $ peek8 p 1
        let w   =   b0 `shiftL` 8
                .|. b1
        return (w, F.plusPtr p 2, n - 2)
{-# INLINE peekWord16' #-}


-- | Peek a `Word32` from memory, in network byte order, with bounds check.
peekWord32  :: Peek Word32
peekWord32 p n
 | n >= 4       = peekWord32' p n
 | otherwise    = error "peekWord32: pointer out of bounds"
{-# NOINLINE peekWord32 #-}


-- | Peek a `Word32` from memory, in network byte order, with no bounds check.
peekWord32' :: Peek Word32
peekWord32' p n
 = do   b0 <- fmap to32 $ peek8 p 0
        b1 <- fmap to32 $ peek8 p 1
        b2 <- fmap to32 $ peek8 p 2
        b3 <- fmap to32 $ peek8 p 3
        let w   =   b0 `shiftL` 24
                .|. b1 `shiftL` 16
                .|. b2 `shiftL` 8
                .|. b3
        return (w, F.plusPtr p 4, n - 4)
{-# INLINE peekWord32' #-}


-- | Peek a `Word64` from memory, in network byte order, with bounds check.
peekWord64  :: Peek Word64
peekWord64 p n
 | n >= 8       = peekWord64' p n
 | otherwise    = error "peekWord64: pointer out of bounds"
{-# NOINLINE peekWord64 #-}


-- | Peek a `Word64` from memory, in network byte order, in network byte order.
peekWord64' :: Peek Word64
peekWord64' p n
 = do   b0 <- fmap to64 $ peek8 p 0
        b1 <- fmap to64 $ peek8 p 1
        b2 <- fmap to64 $ peek8 p 2
        b3 <- fmap to64 $ peek8 p 3
        b4 <- fmap to64 $ peek8 p 4
        b5 <- fmap to64 $ peek8 p 5
        b6 <- fmap to64 $ peek8 p 6
        b7 <- fmap to64 $ peek8 p 7
        let w   =   b0 `shiftL` 56
                .|. b1 `shiftL` 48
                .|. b2 `shiftL` 40
                .|. b3 `shiftL` 32
                .|. b4 `shiftL` 24
                .|. b5 `shiftL` 16
                .|. b6 `shiftL` 8
                .|. b7
        return (w, F.plusPtr p 8, n - 8)
{-# INLINE peekWord64' #-}


to16  :: Word8 -> Word16
to16 = fromIntegral
{-# INLINE to16 #-}


to64  :: Word8 -> Word64
to64 = fromIntegral
{-# INLINE to64 #-}


to32  :: Word8 -> Word32
to32 = fromIntegral
{-# INLINE to32 #-}


peek8 :: Ptr a -> Int -> IO Word8
peek8 p o = F.peekByteOff p o
{-# INLINE peek8 #-}


peek16 :: Ptr a -> Int -> IO Word16
peek16 p o = F.peekByteOff p o
{-# INLINE peek16 #-}


peek32 :: Ptr a -> Int -> IO Word32
peek32 p o = F.peekByteOff p o
{-# INLINE peek32 #-}

