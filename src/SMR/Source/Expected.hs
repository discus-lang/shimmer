
module SMR.Source.Expected where
import SMR.Source.Parsec
import SMR.Source.Token
import SMR.Data.Bag                     (Bag)
import Data.Text                        (Text)
import qualified SMR.Data.Bag           as Bag
import qualified Data.Text              as Text

-------------------------------------------------------------------------------
data Expected t s p
        -- Base expectactions are of particular tokens appearing on the input.
        = ExBaseEnd
        | ExBaseNameOf  Space
        | ExBaseNat
        | ExBasePunc    Char
        | ExBaseMsg     String
        | ExBaseNameAny

        -- Contextual expectations, which indicate what we were trying
        -- to parse while we had some other expectation.
        | ExContextDecl
                Text
                (Bag (Blocker t (Expected t s p)))

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


pprParseError
        :: (Show t, Show s, Show p)
        => ParseError t (Expected t s p) -> String
pprParseError (ParseError bs)
 = unlines $ map pprBlocker bs

