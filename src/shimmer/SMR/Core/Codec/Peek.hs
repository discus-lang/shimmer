{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
module SMR.Core.Codec.Peek
        ( type Peek
        , peekFileDecls
        , peekDecl
        , peekExp
        , peekRef)
where
import SMR.Prim.Op.Base
import SMR.Core.Codec.Word
import SMR.Core.Exp

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
import Data.Int
import Numeric

---------------------------------------------------------------------------------------------------
-- | Type of a function that peeks an `a` thing from memory.
--
--   It takes the current pointer and count of remaining bytes in the buffer,
--   returns new pointer and remaining bytes.
--
type Peek a = Ptr Word8 -> Int -> IO (a, Ptr Word8, Int)


---------------------------------------------------------------------------------------------------
-- | Peek a list of `Decl` from memory, including the SMR file header.
--
--   If the packed data is malformed then `error`.
peekFileDecls :: Peek [Decl Text Prim]
peekFileDecls !p0 !n0
 = do   (b0, p1, n1) <- peekWord8 p0 n0
        (b1, p2, n2) <- peekWord8 p1 n1
        (b2, p3, n3) <- peekWord8 p2 n2
        (b3, p4, n4) <- peekWord8 p3 n3
        when ( b0 /= 0x53 || b1 /= 0x4d || b2 /= 0x52 || b3 /= 0x31)
         $ error "shimmer.peekFileDecls: bad magic"

        (ds, p5, n5) <- peekList peekDecl p4 n4
        return (ds, p5, n5)
{-# NOINLINE peekFileDecls #-}


-- | Peek a `Decl` from memory.
--
--   If the packed data is malformed then `error`.
peekDecl :: Peek (Decl Text Prim)
peekDecl !p0 !n0
 = do   (b0, p1, n1) <- peekWord8 p0 n0
        p1 `seq` case b0 of
         0xd0
          -> do (tx,  p2, n2) <- peekName p1 n1
                (x,   p3, n3) <- peekExp  p2 n2
                return (DeclMac tx x, p3, n3)

         0xd1
          -> do (tx,  p2, n2) <- peekName p1 n1
                (x,   p3, n3) <- peekExp  p2 n2
                return (DeclSet tx x, p3, n3)

         _ -> error $ failHeaderByte "peekDecl" b0 n0
{-# NOINLINE peekDecl #-}


---------------------------------------------------------------------------------------------------
-- | Peek an `Exp` from memory.
--
--   If the packed data is malformed then `error`.
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
          -> do (x1,  p2, n2) <- peekExp  p1 n1
                (xs,  p3, n3) <- peekList peekExp p2 n2
                return  (XApp x1 xs, p3, n3)

         0xb3
          -> do (n,   p2, n2) <- peekName p1 n1
                (i,   p3, n3) <- peekBump p2 n2
                return  (XVar n i, p3, n3)

         0xb4
          -> do (ps,  p2, n2) <- peekList peekParam p1 n1
                (x,   p3, n3) <- peekExp  p2 n2
                return  (XAbs ps x, p3, n3)

         0xb5
          -> do (cs,  p2, n2) <- peekList peekCar p1 n1
                (x,   p3, n3) <- peekExp  p2 n2
                return  (XSub cs x, p3, n3)

         _ -> case b0 .&. 0x0f0 of
                -- Short Variable Name.
                0x80
                 -> do  (tx, p2, n2) <- peekVar p0 n0
                        return (XVar tx 0, p2, n2)

                -- Short Abstraction.
                0x90    -> peekAbs p0 n0

                -- Short Application.
                0xa0    -> peekApp p0 n0

                -- Short Circuit to Ref.
                0xc0
                 -> do  (r, p2, n2) <- peekRef p0 n0
                        return (XRef r, p2, n2)

                -- Short Circuit to Sym.
                0xf0
                 -> do  (tx, p2, n2) <- peekText p0 n0
                        return (XRef $ RSym tx, p2, n2)

                _ -> failHeaderByte "peekExp" b0 n0

{-# NOINLINE peekExp #-}


-- | Peek a short abstraction from memory.
peekAbs :: Peek (Exp Text Prim)
peekAbs p0 n0
 | n0 >= 1
 = do   (b0, p1, n1) <- peekWord8' p0 n0

        when ((b0 .&. 0x0f0) /= 0x90)
         $ failHeaderByte "peekAbs" b0 n0

        go    (fromIntegral $ b0 .&. 0x00f) [] p1 n1

 | otherwise
 = error "shimmer.peekAbs: short header"

 where  go (0 :: Int) acc p n
         = do   (x, p2, n2) <- peekExp p n
                return (XAbs (reverse acc) x, p2, n2)

        go i acc p n
         = do   (x, p', n') <- peekParam p n
                go (i - 1) (x : acc) p' n'
        {-# NOINLINE go #-}
{-# INLINE peekAbs #-}


-- | Peek a short application from memory.
peekApp :: Peek (Exp Text Prim)
peekApp p0 n0
 | n0 >= 1
 = do   (b0, p1, n1) <- peekWord8' p0 n0
        when ((b0 .&. 0x0f0) /= 0xa0)
         $ failHeaderByte "peekApp" b0 n0

        (x0, p2, n2) <- peekExp    p1 n1
        go  x0 (fromIntegral $ b0 .&. 0x00f) [] p2 n2

 | otherwise
 = error "shimmer.peekApp: short header"

 where  go x0 (0 :: Int) acc p n
         = do   return (XApp x0 (reverse acc), p, n)

        go x0 i acc p n
         = do   (x, p', n') <- peekExp p n
                go x0 (i - 1) (x : acc) p' n'
        {-# NOINLINE go #-}
{-# INLINE peekApp #-}


-- | Peek a `Key` from memory.
peekKey :: Peek Key
peekKey !p0 !n0
 = do   (b0, p1, n1) <- peekWord8 p0 n0
        p1 `seq` case b0 of
         0xb6   -> return (KBox, p1, n1)
         0xb7   -> return (KRun, p1, n1)
         _      -> failHeaderByte "peekKey" b0 n0
{-# INLINE peekKey #-}


-- | Peek a `Param` from memory.
peekParam :: Peek Param
peekParam !p0 !n0
 = do   (b0, p1, n1) <- peekWord8 p0 n0
        p1 `seq` case b0 of
         0xb8
          -> do (tx, p2, n2) <- peekName p1 n1
                return (PParam tx PVal, p2, n2)

         0xb9
          -> do (tx, p2, n2) <- peekName p1 n1
                return (PParam tx PExp, p2, n2)

         _ -> failHeaderByte "peekParam" b0 n0
{-# INLINE peekParam #-}


-- | Peek a `Car` from memory.
peekCar :: Peek (Car Text Prim)
peekCar !p0 !n0
 = do   (b0, p1, n1) <- peekWord8 p0 n0
        p1 `seq` case b0 of
         0xba
          -> do (sbs, p2, n2) <- peekList peekSnvBind p1 n1
                return (CSim (SSnv sbs), p2, n2)

         0xbb
          -> do (sbs, p2, n2) <- peekList peekSnvBind p1 n1
                return (CRec (SSnv sbs), p2, n2)

         0xbc
          -> do (ups, p2, n2) <- peekList peekUpsBump p1 n1
                return (CUps (UUps ups), p2, n2)

         _ -> failHeaderByte "peekCar" b0 n1
{-# INLINE peekCar #-}


-- | Peek an `SnvBind` from memory.
peekSnvBind :: Peek (SnvBind Text Prim)
peekSnvBind !p0 !n0
 = do   (b0, p1, n1) <- peekWord8 p0 n0
        p1 `seq` case b0 of
         0xbd
          -> do (n, p2, n2) <- peekName p1 n1
                (d, p3, n3) <- peekBump p2 n2
                (x, p4, n4) <- peekExp  p3 n3
                return (BindVar n d x, p4, n4)

         0xbe
          -> do (n, p2, n2) <- peekNom  p1 n1
                (x, p3, n3) <- peekExp  p2 n2
                return (BindNom n x,   p3, n3)

         _ -> failHeaderByte "peekSnvBind" b0 n1
{-# INLINE peekSnvBind #-}


-- | Peek an `UpsBump` from memory.
peekUpsBump :: Peek UpsBump
peekUpsBump !p0 !n0
 = do   (b0, p1, n1) <- peekWord8 p0 n0

        when (b0 /= 0xbf)
         $ failHeaderByte "peekUpsBump" b0 n1

        (n,  p2, n2) <- peekName  p1 n1
        (d,  p3, n3) <- peekBump  p2 n2
        (i,  p4, n4) <- peekBump  p3 n3
        return  $ (((n, d), i), p4, n4)
{-# INLINE peekUpsBump #-}


---------------------------------------------------------------------------------------------------
-- | Peek a `Ref` from memory.
--
--   If the packed data is malformed then `error`.
peekRef :: Peek (Ref Text Prim)
peekRef !p0 !n0
 = do   (b0, p1, n1) <- peekWord8 p0 n0
        p1 `seq` case b0 of
         0xc0
          -> do (tx, p2, n2) <- peekText p1 n1
                return (RSym tx, p2, n2)

         0xc1
          -> do (m,  p2, n2) <- peekPrim p1 n1
                return (RPrm m,  p2, n2)

         0xc2
          -> do (tx, p2, n2) <- peekText p1 n1
                return (RTxt tx, p2, n2)

         0xc3
          -> do (tx, p2, n2) <- peekText p1 n1
                return (RMac tx, p2, n2)

         0xc4
          -> do (tx, p2, n2) <- peekText p1 n1
                return (RSet tx, p2, n2)

         0xc5
          -> do (i,  p2, n2) <- peekNom  p1 n1
                return (RNom i,  p2, n2)

         -- Short Circuit Sym.
         _
          -> do (r,   p1', n1') <- peekName p0 n0
                return (RSym r, p1', n1')
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
         0xe0   -> return (PrimTagUnit,         p1, n1)
         0xe1   -> return (PrimTagList,         p1, n1)
         0xe2   -> return (PrimLitBool True,    p1, n1)
         0xe3   -> return (PrimLitBool False,   p1, n1)

         -- WordN ----
         0xe4
          -> do (w8, p2, n2) <- peekWord8 p1 n1
                return (PrimLitWord8   w8, p2, n2)

         0xe5
          -> do (w16, p2, n2) <- peekWord16 p1 n1
                return (PrimLitWord16 w16, p2, n2)

         0xe6
          -> do (w32, p2, n2) <- peekWord32 p1 n1
                return (PrimLitWord32 w32, p2, n2)

         0xe7
          -> do (w64, p2, n2) <- peekWord64 p1 n1
                return (PrimLitWord64 w64, p2, n2)

         -- IntN -----
         0xe8
          -> do (w8, p2, n2)  <- peekWord8 p1 n1
                return (PrimLitInt8  $ fromIntegral  w8, p2, n2)

         0xe9
          -> do (w16, p2, n2) <- peekWord16 p1 n1
                return (PrimLitInt16 $ fromIntegral w16, p2, n2)

         0xea
          -> do (w32, p2, n2) <- peekWord32 p1 n1
                return (PrimLitInt32 $ fromIntegral w32, p2, n2)

         0xeb
          -> do (w64, p2, n2) <- peekWord64 p1 n1
                return (PrimLitInt64 $ fromIntegral w64, p2, n2)

         -- FloatN -----
         0xec
          -> do (f32, p2, n2) <- peekFloat32 p1 n1
                return (PrimLitFloat32 f32, p2, n2)

         0xed
          -> do (f64, p2, n2) <- peekFloat64 p1 n1
                return (PrimLitFloat64 f64, p2, n2)

         -----------
         0xee
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

                 "int"
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

                                F.allocaBytes 8 $ \pp
                                 -> do  F.poke (F.castPtr pp :: Ptr Word64) w
                                        i64 <- F.peek (F.castPtr pp :: Ptr Int64)
                                        return (PrimLitInt (fromIntegral i64), p3, n3)

                         _ -> error "shimmer.peekPrim: invalid payload"

                 s -> error $ "shimmer.peekPrim: unknown tag " ++ show s

         _ -> failHeaderByte "peekPrim" b0 n1

 | otherwise
 = error "shimmer.peekPrim: short header"
{-# INLINE peekPrim #-}


---------------------------------------------------------------------------------------------------
-- | Peek a list of things from memory.
peekList :: Peek a -> Peek [a]
peekList peekA p0 n0
 | n0 >= 1
 = do   (b0, p1, n1) <- peekWord8' p0 n0

        case b0 of
         0xfd
          | n1 >= 1
          -> do nElems <- fmap fromIntegral $ peek8  p0 1
                go nElems [] (F.plusPtr p0 2) (n1 - 1)

         0xfe
          | n1 >= 2
          -> do nElems <- fmap fromIntegral $ peek16 p0 1
                go nElems [] (F.plusPtr p0 3) (n1 - 2)

         0xff
          | n1 >= 4
          -> do nElems <- fmap fromIntegral $ peek32 p0 1
                go nElems [] (F.plusPtr p0 5) (n1 - 4)

         _ |  (b0 .&. 0x0f0) == 0xf0
           -> let nElems = fromIntegral (b0 .&. 0x0f)
              in  go nElems [] p1 n1

           | otherwise
           -> failHeaderByte "peekList" b0 n0

 | otherwise
 = error "shimmer.peekList: short header"

 where  go (0 :: Int) acc p n
         = return (reverse acc, p, n)

        go i acc p n
         = do   (x, p', n') <- peekA p n
                go (i - 1) (x : acc) p' n'
        {-# NOINLINE go #-}

{-# INLINE peekList #-}


---------------------------------------------------------------------------------------------------
-- | Peek a short variable name from memory.
peekVar  :: Peek Text
peekVar !p0 !n0
 | n0 >= 1
 = do   (b0, p1, n1) <- peekWord8' p0 n0

        when ((b0 .&. 0x0f0) /= 0x80)
         $ failHeaderByte "peekVar" b0 n0

        let nBytes  = fromIntegral $ b0 .&. 0x0f
        buf     <- F.mallocBytes nBytes
        F.copyBytes buf (F.castPtr p1) nBytes
        bs      <- BS.unsafePackMallocCStringLen (buf, nBytes)
        return (T.decodeUtf8 bs, F.plusPtr p1 nBytes, n1 - nBytes)

 | otherwise
 = error "shimmer.peekVar: short header"


-- | Peek a text value from memory as UTF8 characters.
peekText :: Peek Text
peekText !p0 !n0
 | n0 >= 1
 = do   (b0, p1, n1) <- peekWord8' p0 n0
        case b0 of

         0xfd
          | n1 >= 1
          -> do nBytes  <- fmap fromIntegral $ peek8 p0 1
                buf     <- F.mallocBytes nBytes
                let p2  =  F.plusPtr p0 2
                let n2  =  n0 - 2

                when (not (n2 >= nBytes))
                 $ error $ "shimmer.peekText.fd: pointer out of range"

                F.copyBytes buf p2 nBytes
                bs      <- BS.unsafePackMallocCStringLen (buf, nBytes)
                return (T.decodeUtf8 bs, F.plusPtr p2 nBytes, n2 - nBytes)

         0xfe
          | n1 >= 2
          -> do nBytes  <- fmap fromIntegral $ peek16 p0 1
                buf     <- F.mallocBytes nBytes
                let p2  =  F.plusPtr p0 3
                let n2  =  n0 - 3

                when (not (n2 >= nBytes))
                 $ error "shimmer.peekText.fe: pointer out of range"

                F.copyBytes buf p2 nBytes
                bs      <- BS.unsafePackMallocCStringLen (buf, nBytes)
                return (T.decodeUtf8 bs, F.plusPtr p2 nBytes, n2 - nBytes)

         0xff
          | n1 >= 4
          -> do nBytes  <- fmap fromIntegral $ peek32 p0 1
                buf     <- F.mallocBytes nBytes
                let p2  =  F.plusPtr p0 5
                let n2  =  n0 - 5

                when (not (n2 >= nBytes))
                 $ error "shimmer.peekText.ff: pointer out of range"

                F.copyBytes buf p2 nBytes
                bs      <- BS.unsafePackMallocCStringLen (buf, nBytes)
                return (T.decodeUtf8 bs, F.plusPtr p2 nBytes, n2 - nBytes)

         -- Short text.
         _
          -> do when ((b0 .&. 0x0f0) /= 0xf0)
                 $ error $ "shimmer.peekVar.fN: invalid header " ++ show b0

                let nBytes  = fromIntegral $ b0 .&. 0x0f
                buf     <- F.mallocBytes nBytes
                F.copyBytes buf (F.castPtr p1) nBytes
                bs      <- BS.unsafePackMallocCStringLen (buf, nBytes)
                return (T.decodeUtf8 bs, F.plusPtr p1 nBytes, n1 - nBytes)

 | otherwise
 = error "shimmer.peekText.start: pointer out of range"
{-# NOINLINE peekText #-}


---------------------------------------------------------------------------------------------------
-- | Peek a `Word8` from memory, in network byte order, with bounds check.
peekWord8  :: Peek Word8
peekWord8 p n
 | n >= 1       = peekWord8' p n
 | otherwise    = error "shimmer.peekWord8: pointer out of bounds"
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
 | otherwise    = error "shimmer.peekWord16: pointer out of bounds"
{-# NOINLINE peekWord16 #-}


-- | Peek a `Word16` from memory, in network byte order, with no bound check.
peekWord16' :: Peek Word16
peekWord16' p n
 = do   w  <- fmap fromBE16 $ peek16 p 0
        return (w, F.plusPtr p 2, n - 2)
{-# INLINE peekWord16' #-}


-- | Peek a `Word32` from memory, in network byte order, with bounds check.
peekWord32  :: Peek Word32
peekWord32 p n
 | n >= 4       = peekWord32' p n
 | otherwise    = error "shimmer.peekWord32: pointer out of bounds"
{-# NOINLINE peekWord32 #-}


-- | Peek a `Word32` from memory, in network byte order, with no bounds check.
peekWord32' :: Peek Word32
peekWord32' p n
 = do   w  <- fmap fromBE32 $ peek32 p 0
        return (w, F.plusPtr p 4, n - 4)
{-# INLINE peekWord32' #-}


-- | Peek a `Word64` from memory, in network byte order, with bounds check.
peekWord64  :: Peek Word64
peekWord64 p n
 | n >= 8       = peekWord64' p n
 | otherwise    = error "shimmer.peekWord64: pointer out of bounds"
{-# NOINLINE peekWord64 #-}


-- | Peek a `Word64` from memory, in network byte order.
peekWord64' :: Peek Word64
peekWord64' p n
 = do   w  <- fmap fromBE64 $ peek64 p 0
        return (w, F.plusPtr p 8, n - 8)
{-# INLINE peekWord64' #-}


-- | Peek a `Float32` from memory, in network byte order, with bounds check.
peekFloat32  :: Peek Float
peekFloat32 p0 n0
 | n0 >= 4
 = F.allocaBytes 4 $ \p'
 -> do  (w32, p1, n1) <- peekWord32' p0 n0
        F.poke (F.castPtr p' :: Ptr Word32) w32
        f32 <- F.peek (F.castPtr p' :: Ptr Float)
        return (f32, p1, n1)

 | otherwise    = error "shimmer.peekFloat32: pointer out of bounds"
{-# NOINLINE peekFloat32 #-}


-- | Peek a `Float64` from memory, in network byte order, with bounds check.
peekFloat64  :: Peek Double
peekFloat64 p0 n0
 | n0 >= 8
 = F.allocaBytes 8 $ \p'
 -> do  (w64, p1, n1) <- peekWord64' p0 n0
        F.poke (F.castPtr p' :: Ptr Word64) w64
        f64 <- F.peek (F.castPtr p' :: Ptr Double)
        return (f64, p1, n1)

 | otherwise    = error "shimmer.peekFloat64: pointer out of bounds"
{-# NOINLINE peekFloat64 #-}


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


peek64 :: Ptr a -> Int -> IO Word64
peek64 p o = F.peekByteOff p o
{-# INLINE peek64 #-}


-- Failure ----------------------------------------------------------------------------------------
failHeaderByte :: String -> Word8 -> Int -> a
failHeaderByte fn b n
 = error
 $ "shimmer." ++ fn
        ++ " invalid header byte "
        ++ showHex b "" ++ "@-" ++ showHex n ""
