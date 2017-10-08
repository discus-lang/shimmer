{-# LANGUAGE OverloadedStrings #-}
module SMR.Prim.Name where
import SMR.Prim.Op.Base
import Data.Text                (Text)
import Data.Set                 (Set)
import qualified Data.Set       as Set
import qualified Data.Char      as Char
import qualified Data.Text      as Text


-- | Pretty print a primitive operator.
pprPrim :: Prim -> Text
pprPrim pp
 = case pp of
        PrimOp op          -> op

        PrimLitBool True   -> "true"
        PrimLitBool False  -> "false"

        PrimLitNat n       -> Text.pack $ "nat'" ++ show n

        PrimTagList        -> "list"


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
 = Just $ PrimLitNat (read tx')

 -- Other primtiives.
 | Set.member tx ps
 = Just $ PrimOp tx

 | tx == "list"
 = Just $ PrimTagList

 -- Unrecognised.
 | otherwise
 = Nothing
