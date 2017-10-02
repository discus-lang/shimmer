
module SMR.Source.Parser where
import SMR.Source.Expected
import SMR.Source.Token
import SMR.Source.Lexer
import qualified SMR.Source.Parsec      as P

type PParser s p a
        = Parser (Located Token) (Expected (Located Token) s p) a





-------------------------------------------------------------------------------
-- | Parser for a natural number.
pNat :: PParser s p Int
pNat = P.from ExBaseNat (takeNatOfToken . valueOfLocated)
