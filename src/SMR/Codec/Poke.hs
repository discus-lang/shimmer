{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
module SMR.Codec.Poke
        ( pokeFileDecls
        , pokeDecl
        , pokeExp,   pokeKey,      pokeParam
        , pokeCar,   pokeSnvBind,  pokeUpsBump
        , pokeRef
        , pokeName,  pokeBump,     pokeNom
        , pokeWord8, pokeWord16,   pokeWord32,  pokeWord64)
where
import SMR.Core.Exp
import SMR.Prim.Op.Base

import qualified Foreign.Marshal.Utils          as F
import qualified Foreign.Storable               as F
import qualified Foreign.Ptr                    as F

import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.ByteString.Unsafe         as BS

import Data.Text                                (Text)
import Foreign.Ptr                              (Ptr)
import Control.Monad
import Data.Bits
import Data.Word


---------------------------------------------------------------------------------------------------
type Poke a = a -> Ptr Word8 -> IO (Ptr Word8)


---------------------------------------------------------------------------------------------------
-- | Poke a list of `Decl` into memory, including the SMR file header.
pokeFileDecls :: Poke [Decl Text Prim]
pokeFileDecls ds
        =   pokeWord8 0x53              -- 'S'
        >=> pokeWord8 0x4d              -- 'M'
        >=> pokeWord8 0x52              -- 'R'
        >=> pokeWord8 0x31              -- '1'
        >=> pokeList  pokeDecl ds
{-# NOINLINE pokeFileDecls #-}


-- | Poke a `Decl` into memory.
pokeDecl :: Poke (Decl Text Prim)
pokeDecl xx
 = case xx of
        DeclMac name x
         ->     pokeWord8 0xa1 >=> pokeText name >=> pokeExp x

        DeclSet name x
         ->     pokeWord8 0xa2 >=> pokeText name >=> pokeExp x
{-# NOINLINE pokeDecl #-}


---------------------------------------------------------------------------------------------------
-- | Poke an `Exp` into memory.
pokeExp :: Poke (Exp Text Prim)
pokeExp xx
 = case xx of
        XRef ref
         ->     pokeWord8 0xb1 >=> pokeRef ref

        XKey key x
         ->     pokeWord8 0xb2 >=> pokeKey key >=> pokeExp x

        XApp x1 xs
         ->     pokeWord8 0xb3 >=> pokeExp x1  >=> pokeList pokeExp xs

        XVar name i
         ->     pokeWord8 0xb4 >=> pokeName name >=> pokeBump i

        XAbs ps x
         ->     pokeWord8 0xb5 >=> pokeList pokeParam ps >=> pokeExp x

        XSub cs x
         ->     pokeWord8 0xb6 >=> pokeList pokeCar cs >=> pokeExp x
{-# NOINLINE pokeExp #-}


-- | Poke a `Key` into memory.
pokeKey :: Poke Key
pokeKey key
 = case key of
        KBox -> pokeWord8 0xba
        KRun -> pokeWord8 0xbb
{-# INLINE pokeKey #-}


-- | Poke a `Param` into memory.
pokeParam :: Poke Param
pokeParam pp
 = case pp of
        PParam tx PVal
         ->     pokeWord8 0xbc >=> pokeName tx

        PParam tx PExp
         ->     pokeWord8 0xbd >=> pokeName tx
{-# INLINE pokeParam #-}


-- | Poke a `Car` into memory.
pokeCar :: Poke (Car Text Prim)
pokeCar car
 = case car of
        CSim (SSnv sbs)
         ->     pokeWord8 0xc1 >=> pokeList pokeSnvBind sbs

        CRec (SSnv sbs)
         ->     pokeWord8 0xc2 >=> pokeList pokeSnvBind sbs

        CUps (UUps ups)
         ->     pokeWord8 0xc3 >=> pokeList pokeUpsBump ups
{-# INLINE pokeCar #-}


-- | Poke an `SnvBind` into memory.
pokeSnvBind :: Poke (SnvBind Text Prim)
pokeSnvBind !b
 = case b of
        BindVar n d x
         -> pokeWord8 0xca >=> pokeName n >=> pokeBump d >=> pokeExp x

        BindNom n x
         -> pokeWord8 0xcb >=> pokeNom  n >=> pokeExp x
{-# INLINE pokeSnvBind #-}


-- | Poke an `UpsBump` into memory.
pokeUpsBump :: Poke UpsBump
pokeUpsBump ((n, d), i)
 =      pokeWord8 0xcc >=> pokeName n >=> pokeBump d >=> pokeBump i
{-# INLINE pokeUpsBump #-}


---------------------------------------------------------------------------------------------------
-- | Poke a `Ref` into memory.
pokeRef :: Poke (Ref Text Prim)
pokeRef !r
 = case r of
        RSym tx -> pokeWord8 0xd1 >=> pokeName tx
        RPrm p  -> pokeWord8 0xd2 >=> pokePrim p
        RMac tx -> pokeWord8 0xd3 >=> pokeName tx
        RSet tx -> pokeWord8 0xd4 >=> pokeName tx
        RNom i  -> pokeWord8 0xd5 >=> pokeNom  i
{-# INLINE pokeRef #-}


---------------------------------------------------------------------------------------------------
-- | Peek a `Name` from memory.
pokeName :: Poke Name
pokeName !p n
 =      pokeText p n
{-# INLINE pokeName #-}


-- | Poke a `Bump` into memory.
pokeBump :: Poke Integer
pokeBump !n !p
 = if n <= 2^(16 :: Int) then
    do  pokeWord16 (fromIntegral n) p
   else error "shimmer.pokeBump: bump counter too large."
{-# NOINLINE pokeBump #-}


-- | Poke a `Nom` into memory.
pokeNom  :: Poke Integer
pokeNom !n !p
 = if n <= 2^(28 :: Int) then
    do  pokeWord32 (fromIntegral n) p
   else error "shimmer.pokeNom: nominal constant index too large."
{-# NOINLINE pokeNom #-}


---------------------------------------------------------------------------------------------------
-- | Poke a prim into memory.
pokePrim :: Poke Prim
pokePrim !pp
 = case pp of
        PrimTagUnit             -> pokeWord8 0xda
        PrimLitBool True        -> pokeWord8 0xdb
        PrimLitBool False       -> pokeWord8 0xdc
        PrimOp tx               -> pokeWord8 0xdf >=> pokeText tx

        -- Integers are currently squashed into Word64s.
        PrimLitNat n
         -> pokeWord8 0xef
                >=> pokeName (T.pack "nat")
                >=> pokeList pokeWord8
                        [ fromIntegral $ (n .&. 0xff00000000000000) `shiftR` 56
                        , fromIntegral $ (n .&. 0x00ff000000000000) `shiftR` 48
                        , fromIntegral $ (n .&. 0x0000ff0000000000) `shiftR` 40
                        , fromIntegral $ (n .&. 0x000000ff00000000) `shiftR` 32
                        , fromIntegral $ (n .&. 0x00000000ff000000) `shiftR` 24
                        , fromIntegral $ (n .&. 0x0000000000ff0000) `shiftR` 16
                        , fromIntegral $ (n .&. 0x000000000000ff00) `shiftR` 8
                        , fromIntegral $ (n .&. 0x00000000000000ff)]

        PrimTagList{} -> error "TODO: pokePrim: handle lists"
{-# INLINE pokePrim #-}


---------------------------------------------------------------------------------------------------
-- | Poke a list of things into memory, including size info.
pokeList :: Poke a -> Poke [a]
pokeList pokeA ls
 = do   let  n     = length ls
        if n <= 2^(8 :: Int) - 1
         then   pokeWord8 0xf1 >=> pokeWord8  (fromIntegral n) >=> go ls

        else if n <= 2^(16 :: Int) - 1
         then   pokeWord8 0xf2 >=> pokeWord16 (fromIntegral n) >=> go ls

        else if n <= 2^(28 :: Int)
         then   pokeWord8 0xf2 >=> pokeWord32 (fromIntegral n) >=> go ls

        else error "shimmer.pokeList: list too long."

 where  go [] !p0 = return p0
        go (x : xs) !p0
         = do   p1 <- pokeA x p0
                go xs p1
        {-# NOINLINE go #-}

{-# INLINE pokeList #-}


---------------------------------------------------------------------------------------------------
-- | Poke a text value into memory as UTF8 characters.
pokeText :: Poke Text
pokeText !tx !p0
 = do   let bs = T.encodeUtf8 tx

        BS.unsafeUseAsCStringLen bs $ \(pStr, nBytes)
         -> if nBytes <= 255 then
             do p1 <- pokeWord8 0xf1 p0
                p2 <- pokeWord8 (fromIntegral nBytes) p1
                F.copyBytes (F.castPtr p2) pStr nBytes
                return (F.plusPtr p2 nBytes)

            else if nBytes <= 65535 then
             do p1 <- pokeWord8  0xf2 p0
                p2 <- pokeWord16 (fromIntegral nBytes) p1
                F.copyBytes (F.castPtr p2) pStr nBytes
                return (F.plusPtr p2 nBytes)

            -- The Haskell Int type is only guaranteed to have at least 29
            -- bits of precision. We just limit the string size to 2^28,
            -- as 256MB should be enough for any sort of program text.
            else if nBytes <= 2^(28 :: Int) then
             do p1 <- pokeWord8  0xf3 p0
                p2 <- pokeWord32 (fromIntegral nBytes) p1
                F.copyBytes (F.castPtr p2) pStr nBytes
                return (F.plusPtr p2 nBytes)

            else error "shimmer.pokeText: text string too large."
{-# NOINLINE pokeText #-}


---------------------------------------------------------------------------------------------------
-- | Poke a `Word8` into memory.
pokeWord8 :: Poke Word8
pokeWord8 w p
 = do   F.poke p w
        return (F.plusPtr p 1)
{-# INLINE pokeWord8 #-}


-- | Poke a `Word16` into memory, in network byte order.
pokeWord16 :: Poke Word16
pokeWord16 w p
 = do   poke8 p 0 $ from16 $ (w .&. 0xff00) `shiftR` 8
        poke8 p 1 $ from16 $ (w .&. 0x00ff)
        return (F.plusPtr p 2)
{-# INLINE pokeWord16 #-}


-- | Poke a `Word32` into memory, in network byte order.
pokeWord32 :: Poke Word32
pokeWord32 w p
 = do   poke8 p 0 $ from32 $ (w .&. 0xff000000) `shiftR` 24
        poke8 p 1 $ from32 $ (w .&. 0x00ff0000) `shiftR` 16
        poke8 p 2 $ from32 $ (w .&. 0x0000ff00) `shiftR`  8
        poke8 p 3 $ from32 $ (w .&. 0x000000ff)
        return (F.plusPtr p 4)
{-# INLINE pokeWord32 #-}


-- | Poke a `Word64` into memory, in network byte order.
pokeWord64 :: Poke Word64
pokeWord64 w p
 = do   poke8 p 0 $ from64 $ (w .&. 0xff00000000000000) `shiftR` 56
        poke8 p 1 $ from64 $ (w .&. 0x00ff000000000000) `shiftR` 48
        poke8 p 2 $ from64 $ (w .&. 0x0000ff0000000000) `shiftR` 40
        poke8 p 3 $ from64 $ (w .&. 0x000000ff00000000) `shiftR` 32
        poke8 p 4 $ from64 $ (w .&. 0x00000000ff000000) `shiftR` 24
        poke8 p 5 $ from64 $ (w .&. 0x0000000000ff0000) `shiftR` 16
        poke8 p 6 $ from64 $ (w .&. 0x000000000000ff00) `shiftR`  8
        poke8 p 7 $ from64 $ (w .&. 0x00000000000000ff)
        return (F.plusPtr p 8)
{-# INLINE pokeWord64 #-}


from16 :: Word16 -> Word8
from16 = fromIntegral
{-# INLINE from16 #-}


from32 :: Word32 -> Word8
from32 = fromIntegral
{-# INLINE from32 #-}


from64 :: Word64 -> Word8
from64 = fromIntegral
{-# INLINE from64 #-}


poke8 :: Ptr a -> Int -> Word8 -> IO ()
poke8 p i w = F.pokeByteOff p i w
{-# INLINE poke8 #-}

