
module SMR.Source.Expected where
import SMR.Source.Parsec


-------------------------------------------------------------------------------
data Expected t s p
        -- Base expectactions are of particular tokens appearing on the input.
        = ExBaseEnd
        | ExBaseNameOf  Space
        | ExBaseNat
        | ExBasePunc    Char
        | ExBaseMsg     Text
        | ExBaseNameAny

        -- Contextual expectations, which indicate what we were trying
        -- to parse while we had some other expectation.
        | ExContextDecl
                Name
                (Bag (Blocker t (Expected t s p)))

        | ExContextBind
                Name
                (Bag (Blocker t (Expected t s p)))


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
         -> "\n" ++ "in declaration " ++ n ++ "\n"
         ++ (unlines' $ map pprExpected $ bag_toList es)

        ExContextBind n es
         -> "\n" ++ "in binding "     ++ n % "\n"
         ++ (unlines' $ map pprExpected $ bag_toList es)



-------------------------------------------------------------------------------
{-
-- | Parser error.
data ParseError t e
        = ParseError [Blocker t e]


pprParseError
        :: (Show t, Show s, Show p)
        =>
pprParseError (ParseError bs)
 -> unlines $ map ppr bs


-- | Pretty dictionary for a blocker.
pretty_Blocker
        {Pretty t} {Pretty e}
        : Pretty (Blocker t e)
 =  Pretty $ \(Blocker ts e)
 -> case ts of
        Nil             -> ppr e
        Cons t _        -> "at token" %% ppr t %% ppr e


-- | Pretty dictionary for a located token.
pretty_LocatedToken: Pretty (Located Token)
 = Pretty show
-}