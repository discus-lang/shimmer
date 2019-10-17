
module SMR.Source.Parser where
import SMR.Core.Exp
import SMR.Source.Prim
import SMR.Source.Expected
import SMR.Source.Token
import SMR.Source.Lexer
import SMR.Data.Located

import Data.Text                        (Text)

import qualified SMR.Core.Prim          as Prim
import qualified SMR.Source.Parsec      as P
import qualified SMR.Data.Bag           as Bag
import qualified Data.Text              as Text


-------------------------------------------------------------------------------
type Parser a
        = P.Parser (Located Token) (Expected (Located Token) Text) a

type Error
         = ParseError (Located Token) (Expected (Located Token) Text)

data Config
        = Config


-- Interface ------------------------------------------------------------------
-- | Parse some Shimmer declarations from a list of tokens.
parseDecls
        :: Config -> [Located Token]
        -> Either Error [Decl]
parseDecls c ts
 = case P.parse pDeclsEnd ts of
        P.ParseSkip    es       -> Left $ ParseError (Bag.toList es)
        P.ParseReturn  _ xx     -> Right xx
        P.ParseFailure bs       -> Left $ ParseError (Bag.toList bs)
        P.ParseSuccess xx _     -> Right xx
 where
        pDeclsEnd
         = do   ds      <- pDecls c
                _       <- pEnd
                return ds


-- | Parse a Shimmer expression from a list of tokens.
parseExp
        :: Config -> [Located Token]
        -> Either Error Exp
parseExp c ts
 = case P.parse pExpEnd ts of
        P.ParseSkip    es       -> Left $ ParseError (Bag.toList es)
        P.ParseReturn  _ xx     -> Right xx
        P.ParseFailure bs       -> Left $ ParseError (Bag.toList bs)
        P.ParseSuccess xx _     -> Right xx
 where
        pExpEnd
         = do   x       <- pExp c
                _       <- pEnd
                return x


-- Decl -----------------------------------------------------------------------
-- | Parser for a list of declarations.
pDecls  :: Config -> Parser [Decl]
pDecls c
 =      P.some (pDecl c)


-- | Parser for a single declaration.
pDecl   :: Config -> Parser Decl
pDecl c
 = P.alts
 [ P.enterOn (pNameOfSpace SMac) ExContextDecl $ \name
    -> do nsParam <- P.some (pNameOfSpace SVar)
          _       <- pPunc '='
          xBody   <- pExp c
          _       <- pPunc ';'
          if length nsParam == 0
           then return (DeclMac name xBody)
           else return (DeclMac name $ XAbs nsParam xBody)

 , P.enterOn (pNameOfSpace SSet) ExContextDecl $ \name
    -> do _       <- pPunc '='
          xBody   <- pExp c
          _       <- pPunc ';'
          return (DeclSet name xBody)
 ]


-- Exp ------------------------------------------------------------------------
-- | Parser for an expression.
pExp  :: Config -> Parser Exp
pExp c
 = P.alts
 [ do   -- Abstraction.
        _       <- pPunc '{'
        nsBind  <- P.some (pNameOfSpace SVar)
        _       <- pPunc '}'
        xBody   <- pExp c
        return  $ XAbs  nsBind xBody

 , do   -- Application possibly using '$'
        xHead   <- pExpApp c
        P.alts
         [ do   _       <- pPunc '$'
                xRest   <- pExp c
                return  $  XApp xHead xRest

         , return xHead ]
 ]


-- | Parser for an application.
pExpApp :: Config -> Parser Exp
pExpApp c
 = P.alts
 [ do   -- Primitive application.
        -- Primitives must be saturated with a vector of arguments.
        nPrm    <- pNameOfSpace SPrm
        xArg    <- pExpArg c
        xsArg   <- P.some (pExpArg c)
        return  $  makeXApps (XPrm (POPrim nPrm) xArg) xsArg

 , do   -- General  application.
        xFun    <- pExpArg c
        xsArg   <- P.many (pExpArg c)
        return  $  makeXApps xFun xsArg

        -- Atom
 , do   pExpArg c
 ]


-- | Vector.
pExpVec :: Config -> Parser Exp
pExpVec c
 = do   -- Vector formation.
        _       <- pPunc '['
        xs      <- P.sepBy (pExp c) (pPunc ',')
        _       <- pPunc ']'
        return  $  XVec xs


-- | Parser for an argument expression.
pExpArg :: Config -> Parser Exp
pExpArg c
 = P.alts
 [ do   -- Parenthesised expression.
        _       <- pPunc '('
        x       <- pExp c
        _       <- pPunc ')'
        return x

 , do   -- Vector formation.
        _       <- pPunc '['
        xs      <- P.sepBy (pExp c) (pPunc ',')
        _       <- pPunc ']'
        return  $  XVec xs

        -- Nominal variable.
 , do   _ <- pPunc '?'
        n <- pNat
        return $ XRef (RNom n)

        -- Text string.
 , do   tx <- pText
        return $ XRef (RTxt tx)

        -- Named variable with or without index.
 , do   (space, name) <- pName

        case space of
         -- Named macro.
         SMac -> return $ XMac name

         -- Named variable.
         SVar
          -> P.alt (do  _       <- pPunc '^'
                        ix      <- pNat
                        return  $ XVar name ix)
                   (return $ XVar name 0)

         -- Named set.
         SSet -> return $ XRef (RSet name)

         -- Named symbol
         SSym -> return $ XRef (RSym name)

         -- Named primitive.
         SPrm
          -> case readLitVal name of
                Just v  -> return (XVal v)
                _       -> error $ "unknown literal" ++ show name

         -- Named keyword.
         SKey -> P.fail

         -- Named nominal (should be handled above)
         SNom -> P.fail
 ]


-------------------------------------------------------------------------------
-- | Parser for a natural number.
pNat  :: Parser Integer
pNat  =  P.from ExBaseNat  (takeNatOfToken . valueOfLocated)


-- | Parser for a text string.
pText :: Parser Text
pText =  P.from ExBaseText (takeTextOfToken . valueOfLocated)


-- | Parser for a name in the given name space.
pNameOfSpace :: Space -> Parser Text
pNameOfSpace s
 = P.from (ExBaseNameOf s) (takeNameOfToken s . valueOfLocated)


-- | Parser for a name of any space.
pName :: Parser (Space, Text)
pName
 = P.from ExBaseNameAny    (takeAnyNameOfToken . valueOfLocated)


-- | Parser for the end of input token.
pEnd  :: Parser ()
pEnd
 = do   _ <- P.satisfies ExBaseEnd (isToken KEnd . valueOfLocated)
        return ()


-- | Parser for a punctuation character.
pPunc  :: Char -> Parser ()
pPunc c
 = do   _ <- P.satisfies (ExBasePunc c) (isToken (KPunc c) . valueOfLocated)
        return ()

