
module SMR.Source.Expected where
import SMR.Source.Parsec
import SMR.Source.Token
import SMR.Data.Located
import SMR.Data.Bag                     (Bag)
import Data.Text                        (Text)
import qualified SMR.Data.Bag           as Bag
import qualified Data.Text              as Text


-------------------------------------------------------------------------------
-- | What we were expecting at the point there was a parse error.
data Expected t s
        -- | Expecting end of input.
        = ExBaseEnd

        -- | Expecting a name in the given namespace.
        | ExBaseNameOf  Space

        -- | Expecting a name in any namespace.
        | ExBaseNameAny

        -- | Expecting a natural number.
        | ExBaseNat

        -- | Expecting a text string.
        | ExBaseText

        -- | Expecting a punctuation character.
        | ExBasePunc    Char

        -- | Expecting something described by the given message.
        | ExBaseMsg     String

        -- | Expecting something while parsing a declaration.
        | ExContextDecl
                Text
                (Bag (Blocker t (Expected t s)))

        -- | Expecting something while parsing a binding.
        | ExContextBind
                Text
                (Bag (Blocker t (Expected t s)))
        deriving Show


-- | Pretty print an expected thing.
pprExpected
        :: Show s
        => Expected (Located Token) s -> String
pprExpected bb
 = case bb of
        ExBaseEnd       -> "expecting end of input"
        ExBaseNameOf s  -> "expecting name " ++ show s
        ExBaseNat       -> "expecting natural number"
        ExBaseText      -> "expecting text string"
        ExBasePunc c    -> "expecting " ++ show c
        ExBaseMsg t     -> "expecting " ++ show t
        ExBaseNameAny   -> "expecting name"

        ExContextDecl n es
         -> "in declaration @" ++ Text.unpack n ++ "\n"
         ++ (unlines $ map pprBlocker $ Bag.toList es)

        ExContextBind n esBag
         | e : _        <- Bag.toList esBag
         -> "in binding " ++ Text.unpack n ++ "\n"
         ++ pprBlocker e

         | otherwise
         -> "in binding " ++ Text.unpack n


-- | Pretty print a blocker.
pprBlocker
        :: Show s
        => Blocker (Located Token) (Expected (Located Token) s)
        -> String

pprBlocker (Blocker [] e)
 = pprExpected e

pprBlocker (Blocker (t : _) e)
 =  pprLocation (startOfLocated t)
 ++ " " ++ pprExpected e


pprLocation :: Location -> String
pprLocation (L l c)
 = show l ++ ":" ++ show c


-------------------------------------------------------------------------------
-- | Parser error.
data ParseError t e
        = ParseError [Blocker t e]
        deriving Show


-- | Pretty print a parser error.
pprParseError
        :: Show s
        => ParseError (Located Token) (Expected (Located Token) s) -> String

pprParseError (ParseError [])
 = "at end of input"

pprParseError (ParseError (b : _bs))
 = pprBlocker b

