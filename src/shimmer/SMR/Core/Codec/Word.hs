{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SMR.Core.Codec.Word
        ( fromBE64, fromLE64
        , fromBE32, fromLE32
        , fromBE16, fromLE16

        , toBE64,    toLE64
        , toBE32,    toLE32
        , toBE16,    toLE16)
where
import Data.Word        as Word
#include "MachDeps.h"

data Endian
        = Big | Little
        deriving Eq

-- | Get the endianness from the GHC header file.
--   We do this via a filty #include so that the information
--   is statically visible to the GHC simplifier.
systemEndian :: Endian
#ifdef WORDS_BIGENDIAN
systemEndian = Big
#else
systemEndian = Little
#endif


-- | Convert from a big endian 64 bit value to the cpu endianness.
fromBE64 :: Word64 -> Word64
fromBE64 = if systemEndian == Big then id else Word.byteSwap64
{-# INLINE fromBE64 #-}

-- | Convert from a little endian 64 bit value to the cpu endianness.
fromLE64 :: Word64 -> Word64
fromLE64 = if systemEndian == Little then id else Word.byteSwap64
{-# INLINE fromLE64 #-}


-- | Convert from a big endian 32 bit value to the cpu endianness.
fromBE32 :: Word32 -> Word32
fromBE32 = if systemEndian == Big then id else Word.byteSwap32
{-# INLINE fromBE32 #-}

-- | Convert from a little endian 32 bit value to the cpu endianness.
fromLE32 :: Word32 -> Word32
fromLE32 = if systemEndian == Little then id else Word.byteSwap32
{-# INLINE fromLE32 #-}


-- | Convert from a big endian 16 bit value to the cpu endianness.
fromBE16 :: Word16 -> Word16
fromBE16 = if systemEndian == Big then id else Word.byteSwap16
{-# INLINE fromBE16 #-}

-- | Convert from a little endian 16 bit value to the cpu endianness.
fromLE16 :: Word16 -> Word16
fromLE16 = if systemEndian == Little then id else Word.byteSwap16
{-# INLINE fromLE16 #-}


-- | Convert a 64 bit value in cpu endianess to big endian
toBE64 :: Word64 -> Word64
toBE64 = fromBE64
{-# INLINE toBE64 #-}

-- | Convert a 64 bit value in cpu endianess to little endian
toLE64 :: Word64 -> Word64
toLE64 = fromLE64
{-# INLINE toLE64 #-}


-- | Convert a 32 bit value in cpu endianess to big endian
toBE32 :: Word32 -> Word32
toBE32 = fromBE32
{-# INLINE toBE32 #-}

-- | Convert a 32 bit value in cpu endianess to little endian
toLE32 :: Word32 -> Word32
toLE32 = fromLE32
{-# INLINE toLE32 #-}


-- | Convert a 16 bit value in cpu endianness to big endian
toBE16 :: Word16 -> Word16
toBE16 = fromBE16
{-# INLINE toBE16 #-}

-- | Convert a 16 bit value in cpu endianness to little endian
toLE16 :: Word16 -> Word16
toLE16 = fromLE16
{-# INLINE toLE16 #-}


