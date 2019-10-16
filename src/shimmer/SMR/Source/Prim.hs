
module SMR.Source.Prim where
import SMR.Core.Exp
import SMR.Core.Prim
import Data.Text                (Text)
import Data.Set                 (Set)
import qualified Data.Set       as Set
import qualified Data.Char      as C
import qualified Data.Text      as T
import Numeric

-- | Pretty print a primitive operator.
pprPrim :: Prim -> Text
pprPrim pp
 = case pp of
        PList           -> T.pack "list"
        POp p           -> p

-- | Pretty print a primitive value.
pprVal :: Val -> Text
pprVal pp
 = case pp of
        VUnit           -> T.pack "unit"

        VBool True      -> T.pack "true"
        VBool False     -> T.pack "false"

        VNat n          -> T.pack $ "nat'" ++ show n
        VInt i          -> T.pack $ "int'" ++ show i

        VWord8  w       -> T.pack $ "w8'"  ++ showHex w ""
        VWord16 w       -> T.pack $ "w16'" ++ showHex w ""
        VWord32 w       -> T.pack $ "w32'" ++ showHex w ""
        VWord64 w       -> T.pack $ "w64'" ++ showHex w ""

        VInt8   i       -> T.pack $ "i8'"  ++ show i
        VInt16  i       -> T.pack $ "i16'" ++ show i
        VInt32  i       -> T.pack $ "i32'" ++ show i
        VInt64  i       -> T.pack $ "i64'" ++ show i

        VFloat32 f      -> T.pack $ "f32'" ++ show f
        VFloat64 f      -> T.pack $ "f64'" ++ show f

        VList vs        -> "[list|" <> T.intercalate "," (map pprVal vs) <> "]"


-- | Parse a primitive name, without the leading '#'.
readLitVal :: Text -> Maybe Val
readLitVal tx
 -- Literal Booleans.
 | tx == "true"         = Just $ VBool True
 | tx == "false"        = Just $ VBool False

 -- Literal Nats.
 | T.isPrefixOf "nat'" tx
 , tx'  <- T.unpack $ T.drop 4 tx
 , all C.isDigit tx'
 , not $ null tx'
 = Just $ VNat (read tx')

 | tx == "unit"         = Just VUnit

 -- Unrecognised.
 | otherwise
 = Nothing
