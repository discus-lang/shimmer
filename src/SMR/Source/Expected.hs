
module SMR.Source.Expected where
import SMR.Source.Parsec
import SMR.Source.Token
import SMR.Data.Bag                     (Bag)
import Data.Text                        (Text)
import qualified SMR.Data.Bag           as Bag
import qualified Data.Text              as Text

-------------------------------------------------------------------------------
-- | What we were expecting at the point there was a parse error.
data Expected t s p
        -- | Expecting end of input.
        = ExBaseEnd

        -- | Expecting a name in the given namespace.
        | ExBaseNameOf  Space

        -- | Expecting a name in any namespace.
        | ExBaseNameAny

        -- | Expecting a natural number.
        | ExBaseNat

        -- | Expecting a punctuation character.
        | ExBasePunc    Char

        -- | Expecting something described by the given message.
        | ExBaseMsg     String

        -- | Expecting something while parsing a declaration.
        | ExContextDecl
                Text
                (Bag (Blocker t (Expected t s p)))

        -- | Expecting something while parsing a binding.
        | ExContextBind
                Text
                (Bag (Blocker t (Expected t s p)))
        deriving Show


-- | Pretty print an expected thing.
pprExpected
        :: (Show t, Show s, Show p)
        => Expected t s p -> String
pprExpected bb
 = case bb of
        ExBaseEnd       -> "expecting end of input"
        ExBaseNameOf s  -> "expecting name " ++ show s
        ExBaseNat       -> "expecting natural number"
        ExBasePunc c    -> "expecting punctuation " ++ show c
        ExBaseMsg t     -> "expecting " ++ show t
        ExBaseNameAny   -> "expecting name"

        ExContextDecl n es
         -> "\n" ++ "in declaration " ++ Text.unpack n ++ "\n"
         ++ (unlines $ map pprBlocker $ Bag.toList es)

        ExContextBind n es
         -> "\n" ++ "in binding "     ++ Text.unpack n ++ "\n"
         ++ (unlines $ map pprBlocker $ Bag.toList es)


-- | Pretty print a blocker.
pprBlocker
        :: (Show t, Show s, Show p)
        => Blocker t (Expected t s p) -> String
pprBlocker (Blocker ts e)
 = case ts of
        []      -> pprExpected e
        t : _   -> "at token " ++ show t ++ " " ++ pprExpected e


-------------------------------------------------------------------------------
-- | Parser error.
data ParseError t e
        = ParseError [Blocker t e]
        deriving Show


-- | Pretty print a parser error.
pprParseError
        :: (Show t, Show s, Show p)
        => ParseError t (Expected t s p) -> String
pprParseError (ParseError bs)
 = unlines $ map pprBlocker bs

