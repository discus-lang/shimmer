{-# LANGUAGE OverloadedStrings #-}
module SMR.Prim.Name
        ( Prim (..)

        -- * Pretty
        , pprPrim
        , readPrim

        -- * Bool
        , makeXBool, takeXBool, takeArgBool

        -- * Nat
        , makeXNat,  takeXNat,  takeArgNat

        -- * List
        , makeXList)
where
import SMR.Prim.Op.Base
import Data.Text                (Text)
import Data.Set                 (Set)
import qualified Data.Set       as Set
import qualified Data.Char      as Char
import qualified Data.Text      as Text
import Numeric


-- | Pretty print a primitive operator.
pprPrim :: Prim -> Text
pprPrim pp
 = case pp of
        PrimTagUnit        -> "unit"
        PrimTagList        -> "list"

        PrimLitBool True   -> "true"
        PrimLitBool False  -> "false"

        PrimLitNat n       -> Text.pack $ "nat'" ++ show n
        PrimLitInt i       -> Text.pack $ "int'" ++ show i

        PrimLitWord8  w    -> Text.pack $ "w8'"  ++ showHex w ""
        PrimLitWord16 w    -> Text.pack $ "w16'" ++ showHex w ""
        PrimLitWord32 w    -> Text.pack $ "w32'" ++ showHex w ""
        PrimLitWord64 w    -> Text.pack $ "w64'" ++ showHex w ""

        PrimLitInt8   i    -> Text.pack $ "i8'"  ++ showHex i ""
        PrimLitInt16  i    -> Text.pack $ "i16'" ++ showHex i ""
        PrimLitInt32  i    -> Text.pack $ "i32'" ++ showHex i ""
        PrimLitInt64  i    -> Text.pack $ "i64'" ++ showHex i ""

        PrimLitFloat32 f   -> Text.pack $ "f32'" ++ show f
        PrimLitFloat64 f   -> Text.pack $ "f64'" ++ show f

        PrimOp op          -> op





-- | Parse a primitive name, without the leading '#'.
readPrim :: Set Text -> Text -> Maybe Prim
readPrim ps tx
 -- Literal Booleans.
 | tx == "true"         = Just $ PrimLitBool True
 | tx == "false"        = Just $ PrimLitBool False

 -- Literal Nats.
 | Text.isPrefixOf "nat'" tx
 , tx'  <- Text.unpack $ Text.drop 4 tx
 , all Char.isDigit tx'
 , not $ null tx'
 = Just $ PrimLitNat (read tx')

 -- Other primtiives.
 | Set.member tx ps
 = Just $ PrimOp tx

 | tx == "unit" = Just PrimTagUnit
 | tx == "list" = Just PrimTagList

 -- Unrecognised.
 | otherwise
 = Nothing
