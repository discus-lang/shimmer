
module SMR.Source.Parser where
import SMR.Core.Exp.Base
import SMR.Core.Exp.Compounds
import SMR.Source.Expected
import SMR.Source.Token
import SMR.Source.Lexer
import SMR.Data.Located

import Data.Text                        (Text)

import qualified SMR.Source.Parsec      as P
import qualified SMR.Data.Bag           as Bag
import qualified Data.Text              as Text
import qualified Data.Vector            as V


type Parser s p a
        = P.Parser (Located Token) (Expected (Located Token) s p) a


-- Config ---------------------------------------------------------------------
data Config s p
        = Config
        { configReadSym :: Text -> Maybe s
        , configReadPrm :: Text -> Maybe p }


-- Interface ------------------------------------------------------------------
parseDecls
        :: Config s p
        -> [Located Token]
        -> Either (ParseError (Located Token) (Expected (Located Token) s p))
                  [Decl s p]
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



-- | Parse a complete expression from the given list of tokens.
parseExp
        :: Config s p
        -> [Located Token]
        -> Either (ParseError (Located Token) (Expected (Located Token) s p))
                  (Exp s p)
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
pDecls  :: Config s p -> Parser s p [Decl s p]
pDecls c
 =      P.some (pDecl c)


pDecl   :: Config s p -> Parser s p (Decl s p)
pDecl c
 = P.alts
 [ P.enterOn (pNameOfSpace SMac) ExContextDecl $ \name
    -> do psParam <- P.some pParam
          _       <- pPunc '='
          xBody   <- pExp c
          _       <- pPunc ';'
          if length psParam == 0
           then return (Decl name xBody)
           else return (Decl name $ XAbs (V.fromList psParam) xBody)

 , P.enterOn (pNameOfSpace SSet) ExContextDecl $ \name
    -> do _       <- pPunc '='
          xBody   <- pExp c
          _       <- pPunc ';'
          return (Decl name xBody)
 ]


-- Exp ------------------------------------------------------------------------
pExp :: Config s p -> Parser s p (Exp s p)
pExp c
        -- Abstraction.
 = P.alts
 [ do   _       <- pPunc '\\'
        psParam <- P.some pParam
        _       <- pPunc '.'
        xBody   <- pExp c
        return  $ XAbs (V.fromList psParam) xBody

        -- Substitution train.
 , do   csTrain <- pTrain c
        _       <- pPunc '.'
        xBody   <- pExp c
        return  $  XSub (V.reverse $ V.fromList csTrain) xBody

        -- Application possibly using '$'
 , do   xHead   <- pExpApp c
        P.alt
            (do _       <- pPunc '$'
                xRest   <- pExp c
                return  $  XApp xHead xRest)
            (return xHead)
 ]


-- | Parser for an application.
pExpApp :: Config s p -> Parser s p (Exp s p)
pExpApp c
        -- Application of a superprim.
 = P.alts
 [ do   nKey
         <- do  nKey'   <- pNameOfSpace SKey
                if       nKey' == Text.pack "box" then return KBox
                 else if nKey' == Text.pack "run" then return KRun
                 else if nKey' == Text.pack "seq" then return KSeq
                 else if nKey' == Text.pack "tag" then return KTag
                 else P.fail

        xsArgs  <- P.some (pExpAtom c)

        case xsArgs of
         xFirst : xsRest
             -> return $ makeXApps (XKey nKey xFirst) xsRest

         []  -> P.fail

        -- Application of some other expression.
 , do   xFun    <- pExpAtom c
        xsArgs  <- P.some (pExpAtom c)
        case xsArgs of
         []  -> return $ xFun
         _   -> return $ foldl XApp xFun xsArgs
 ]


-- | Parser for an atomic expression.
pExpAtom :: Config s p -> Parser s p (Exp s p)
pExpAtom c
        -- Parenthesised expression.
 = P.alts
 [ do   _       <- pPunc '('
        x       <- pExp c
        _       <- pPunc ')'
        return x


        -- Multiple return values.
 , do   _       <- pPunc '<'
        xsArgs  <- P.sepBy (pExp c) (pPunc ',')
        _       <- pPunc '>'
        return  $ XRet $ V.fromList xsArgs


        -- Named variable with or without index.
 , do   (space, name) <- pName

        case space of
         -- Named variable.
         SVar
          -> P.alt (do  _       <- pPunc '^'
                        ix      <- pNat
                        return  $ XVar name ix)
                   (return $ XVar name 0)

         -- Named macro.
         SMac -> return $ XRef (RMac name)

         -- Named set.
         SSet -> return $ XRef (RSet name)

         -- Named symbol
         SSym
          -> case configReadSym c name of
                Just s  -> return (XRef (RSym s))
                Nothing -> P.fail

         -- Named primitive.
         SPrm
          -> case configReadPrm c name of
                Just p  -> return (XRef (RPrm p))
                Nothing -> P.fail

         -- Named keyword.
         SKey
          -> P.fail
 ]


-- Param ----------------------------------------------------------------------
-- | Parser for a function parameter.
pParam  :: Parser s p Param
pParam
 = P.alts
 [ do   _       <- pPunc '!'
        n       <- pNameOfSpace SVar
        return  $  PParam n PVal

 , do   _       <- pPunc '~'
        n       <- pNameOfSpace SVar
        return  $  PParam n PExp

 , do   n       <- pNameOfSpace SVar
        return  $  PParam n PVal

 ]


-- Train ----------------------------------------------------------------------
-- | Parser for a substitution train.
--   The cars are produced in reverse order.
pTrain  :: Config s p -> Parser s p [Car s p]
pTrain c
 = do   cCar    <- pTrainCar c
        P.alt
         (do csCar <- pTrain c
             return $ cCar : csCar)
         (do return $ cCar : [])


-- | Parse a single car in the train.
pTrainCar :: Config s p -> Parser s p (Car s p)
pTrainCar c
 = P.alt
        -- Substitution, both simultaneous and recursive
    (do car     <- pCarSimRec c
        return car)

    (do -- An ups car.
        ups     <- pUps
        return (CUps ups))


-- Snv ------------------------------------------------------------------------
-- | Parser for a substitution environment.
--
--   Snv   ::= '[' Bind*, ']'
--
pCarSimRec :: Config s p -> Parser s p (Car s p)
pCarSimRec c
 = do   _       <- pPunc '['

        P.alt   -- Recursive substitution.
         (do    _       <- pPunc '['
                bs      <- P.sepBy (pBind c) (pPunc ',')
                _       <- pPunc ']'
                _       <- pPunc ']'
                return  $ CRec (SSnv (V.reverse $ V.fromList bs)))

                -- Simultaneous substitution.
         (do    bs      <- P.sepBy (pBind c) (pPunc ',')
                _       <- pPunc ']'
                return  $ CSim (SSnv (V.reverse $ V.fromList bs)))


-- | Parser for a binding.
--
--   Bind ::= Name '=' Exp
--         |  Name '^' Nat '=' Exp
--
pBind   :: Config s p -> Parser s p (SnvBind s p)
pBind c
 =  P.enterOn (pNameOfSpace SVar) ExContextBind $ \name
 -> P.alt
        (do _       <- pPunc '='
            x       <- pExp c
            return  ((name, 0), x))

        (do _       <- pPunc '^'
            bump    <- pNat
            _       <- pPunc '='
            x       <- pExp c
            return  ((name, bump), x))


-- Ups ------------------------------------------------------------------------
-- | Parser for an ups.
--
--   Ups  ::= '{' Bump*, '}'
--
pUps :: Parser s p Ups
pUps
 = do   _       <- pPunc '{'
        bs      <- P.sepBy pBump (pPunc ',')
        _       <- pPunc '}'
        return  $ UUps (V.reverse $ V.fromList bs)


-- | Parser for a bump.
--
--   Bump ::= Name ':' Nat
--         |  Name '^' Nat ':' Nat
--
pBump :: Parser s p UpsBump
pBump
 = do   name    <- pNameOfSpace SVar
        P.alt
         (do    _       <- pPunc ':'
                inc     <- pNat
                return  ((name, 0), inc))

         (do    _       <- pPunc '^'
                depth   <- pNat
                _       <- pPunc ':'
                inc     <- pNat
                return  ((name, depth), inc))


-------------------------------------------------------------------------------
-- | Parser for a natural number.
pNat :: Parser s p Int
pNat
 = P.from ExBaseNat (takeNatOfToken . valueOfLocated)


-- | Parser for a name in the given space.
pNameOfSpace :: Space -> Parser s p Text
pNameOfSpace s
 = P.from (ExBaseNameOf s) (takeNameOfToken s . valueOfLocated)


-- | Parser for a name of any space.
pName :: Parser s p (Space, Text)
pName
 = P.from ExBaseNameAny    (takeAnyNameOfToken . valueOfLocated)


-- | Parser for the end of input token.
pEnd  :: Parser s p ()
pEnd
 = do   _ <- P.satisfies ExBaseEnd (isToken KEnd . valueOfLocated)
        return ()


-- | Parser for a punctuation character.
pPunc  :: Char -> Parser s p ()
pPunc c
 = do   _ <- P.satisfies (ExBasePunc c) (isToken (KPunc c) . valueOfLocated)
        return ()

