
module SMR.Source.Token where
import Data.Text (Text)


-- | Tokens for for the source language.
data Token
        = KEnd                  -- ^ End of input.
        | KPunc Char            -- ^ Punctuation character.
        | KName Space Text      -- ^ A scoped name.
        | KNat  Integer         -- ^ A literal natural number.
        | KText Text            -- ^ A literal text string.
        deriving (Show, Eq)


-- | Name space of a name.
data Space
        = SVar                  -- ^ Local variable.
        | SMac                  -- ^ Macro name.
        | SSym                  -- ^ Symbol name.
        | SSet                  -- ^ Set name.
        | SPrm                  -- ^ Primitive name.
        | SKey                  -- ^ Keyword (super primitive)
        | SNom                  -- ^ Nominal name.
        deriving (Show, Eq)


-- | Check if a token is equal to the give none.
isToken :: Token -> Token -> Bool
isToken k1 k2 = k1 == k2


-- | Check is token is punctuation using the given character.
isKPunc :: Char -> Token -> Bool
isKPunc c k
 = case k of
        KPunc c' -> c == c'
        _        -> False


-- | Take the name from a token, if any.
takeNameOfToken :: Space -> Token -> Maybe Text
takeNameOfToken ss1 kk
 = case kk of
        KName ss2 n
         | ss1 == ss2   -> Just n
         | otherwise    -> Nothing
        _               -> Nothing


-- | Take the name from a token, if any.
takeAnyNameOfToken :: Token -> Maybe (Space, Text)
takeAnyNameOfToken kk
 = case kk of
        KName ss2 n     -> Just (ss2, n)
        _               -> Nothing


-- | Take the natural number from a token, if any.
takeNatOfToken :: Token -> Maybe Integer
takeNatOfToken kk
 = case kk of
        KNat n          -> Just n
        _               -> Nothing


-- | Take the text string from a token, if any.
takeTextOfToken :: Token -> Maybe Text
takeTextOfToken kk
 = case kk of
        KText tx        -> Just tx
        _               -> Nothing

