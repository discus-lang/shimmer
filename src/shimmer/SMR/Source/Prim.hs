
module SMR.Source.Prim where
import SMR.Core.Exp
import SMR.Core.Prim
import Data.Text                (Text)
import Data.Set                 (Set)
import qualified Data.Set       as Set
import qualified Data.Char      as C
import qualified Data.Text      as T
import Numeric


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
