
module SMR.Source.Lexer
        ( lexTokens
        , Located (..)
        , Location(..))
where
import SMR.Source.Token
import SMR.Data.Located
import Data.Text                (Text)
import qualified Data.Text      as Text
import qualified Data.Char      as Char


-- Lexer ----------------------------------------------------------------------
-- | Lex a sequence of tokens.
lexTokens :: Location -> [Char] -> ([Located Token], Location, [Char])
lexTokens lStart0 cs0
 = case skipSpace lStart0 cs0 of
    (lStart, [])
     -> ( LL lStart lStart KEnd : []
        , lStart, [])

    (lStart, cs)
     -> case lexToken lStart cs of
         Nothing
          -> ([], lStart, cs)

         Just (k, cs')
          |  (ks, lStart', cs'') <- lexTokens (endOfLocated k) cs'
          -> (k : ks, lStart', cs'')


-- | Lex a single token.
lexToken :: Location -> [Char] -> Maybe (Located Token, [Char])
lexToken lStart xx
 = case xx of
    []
     -> Nothing

    c : cs
        -- Punctuation.
        |  isCharPunc c
        -> let  lEnd = incCharOfLocation 1 lStart
                tok  = KPunc c
           in   Just (LL lStart lEnd tok, cs)

        -- Variable name.
        |  Just (space, xx')         <- takeSpace c cs
        ,  Just (name, lEnd, csRest) <- lexName   (incCharOfLocation 1 lStart) xx'
        -> let  tok      = KName space name
           in   Just (LL lStart lEnd tok, csRest)

        --  Natural number.
        |  Char.isDigit c
        ,  Just (nat, lEnd, csRest)  <- lexNat lStart (c : cs)
        -> let  tok      = KNat nat
           in   Just (LL lStart lEnd tok, csRest)

        |  otherwise
        -> Nothing


-- | Lex a variable name.
lexName :: Location -> [Char] -> Maybe (Text, Location, [Char])
lexName lStart xx
 = go lStart [] xx
 where
        go lStart' acc []
         | not $ null acc
         = let  name    = Text.pack $ reverse acc
           in   Just (name, lStart', [])

         | otherwise
         = Nothing

        go lStart' acc (c : cs)
         | isNameBodyChar c
         =      go (incCharOfLocation 1 lStart') (c : acc) cs

         | otherwise
         = let  name    = Text.pack $ reverse acc
           in   Just (name, lStart', c : cs)


-- | Lex a natural number.
lexNat  :: Location -> [Char] -> Maybe (Integer, Location, [Char])
lexNat lStart xx
 = go lStart [] xx
 where
        go lStart' acc []
         | all Char.isDigit acc
         , nat <- read $ reverse acc
         = Just (nat, lStart', [])

        go lStart' acc (c : cs)
         | Char.isDigit c
         = go (incCharOfLocation 1 lStart') (c : acc) cs

         | all Char.isDigit acc
         , nat <- read $ reverse acc
         = Just (nat, lStart', c : cs)

        go _ _ _
         = Nothing


-- Whitespace -----------------------------------------------------------------
skipSpace :: Location -> [Char] -> (Location, [Char])
skipSpace lStart xx
 = case xx of
    []  -> (lStart, xx)

    c : cs
        -- Skip whitespace.
        | c == ' '  -> skipSpace (incCharOfLocation 1 lStart) cs
        | c == '\n' -> skipSpace (incLineOfLocation 1 lStart) cs
        | c == '\t' -> skipSpace (incCharOfLocation 8 lStart) cs

        -- Skip comments
        |  c  == '-'
        ,  c2 : cs2 <- cs
        ,  c2 == '-'
        -> skipSpace lStart $ dropWhile (\x -> x /= '\n') cs2

        | otherwise -> (lStart, xx)


-- | Take the namespace qualifier from the front of a name.
takeSpace :: Char -> [Char] -> Maybe (Space, [Char])
takeSpace c cs
 | Char.isLower c = Just (SVar, c : cs)
 | c  == '@'    = Just (SMac, cs)
 | c  == '%'    = Just (SSym, cs)
 | c  == '+'    = Just (SSet, cs)
 | c  == '#'
 , c' : cs' <- cs
 , c' == '#'
 = Just (SKey, cs')

 | c == '#'     = Just (SPrm, cs)
 | otherwise    = Nothing


-- Character Classes ----------------------------------------------------------
-- | Check if this character can appear in the body of a name.
isNameBodyChar :: Char -> Bool
isNameBodyChar c
 =  Char.isLower c
 || Char.isUpper c
 || Char.isDigit c
 || (c == '-' || c == '\'' || c == '_')


-- | Check if this is a punctuation character.
isCharPunc :: Char -> Bool
isCharPunc c
 | c == '('     = True
 | c == ')'     = True
 | c == '{'     = True
 | c == '}'     = True
 | c == '['     = True
 | c == ']'     = True
 | c == '<'     = True
 | c == '>'     = True
 | c == '^'     = True
 | c == ','     = True
 | c == ':'     = True
 | c == '\\'    = True
 | c == '.'     = True
 | c == ';'     = True
 | c == '='     = True
 | c == '$'     = True
 | c == '!'     = True
 | c == '~'     = True
 | c == '?'     = True
 | otherwise    = False

