
-- | Combined lexer/parser for Shimmer source files.
--
--   This is hand written in Continuation Passing Style for two reasons:
--
--   1) To be simple and self contained, so we can use it as a guide when
--      bootstrapping the parser in Shimmer itself.
--
--   2) So we can use the first lookahead symbol to detect what sort of
--      thing we need to parse next. Machine generated parsers typically
--      use lookahead symbols to speed up parsing, but the monadic parser
--      combinator libraries don't.
--
module SMR.Source.Loader where
import SMR.Source.Prim
import SMR.Source.Token
import SMR.Core.Exp

import Data.IORef
import Data.Text                (Text)
import qualified Data.Text      as Text
import qualified Data.Char      as Char


---------------------------------------------------------------------- State --
data State
        = State
        { stateInput    :: IORef [Char]
        , statePosLine  :: IORef Int
        , statePosCol   :: IORef Int }


-- | Parser take the input state,
--    a default action to execute on parse failure,
--    and an action to execute on parse success.
type Parser a
        = forall b
        . State -> IO b -> (a -> IO b) -> IO b


-- | Run a parser on some input characters.
runParser :: Parser a -> [Char] -> IO (Maybe a)
runParser parser cs
 = do   refInput   <- newIORef cs
        refPosLine <- newIORef 1
        refPosCol  <- newIORef 1
        let state = State refInput refPosLine refPosCol
        parser state (return Nothing) $ \x -> return $ Just x


-- | Take a single character from the input stream.
takeChar  :: Parser Char
takeChar s f c
 = do   xx      <- readIORef $ stateInput s
        case xx of
         x : xs -> do writeIORef (stateInput s) xs; c x
         _      -> f


-- | Peek at a single character from the input stream,
--   not consuming it.
peekChar :: Parser Char
peekChar s f c
 = do   xx      <- readIORef $ stateInput s
        case xx of
         x : xs -> c x
         []     -> f


-- | Advance the input stream by one character,
--   consuming it.
nextChar :: Parser ()
nextChar s f c
 = takeChar s f (\_ -> c ())


-- | Check if we're currently at the end of the input stream.
peekEnd :: Parser Bool
peekEnd s f c
 = do   xx      <- readIORef $ stateInput s
        case xx of
         x : _  -> c False
         []     -> c True


-- | Save the current input state and try to execute a parser on the
--   input stream. If the parser fails then revert the state before
--   executing the failure action.
try :: Parser a -> Parser a
try p s f c
 = do   input   <- readIORef (stateInput   s)
        posLine <- readIORef (statePosLine s)
        posCol  <- readIORef (statePosCol  s)
        p s
         (do    writeIORef (stateInput   s) input
                writeIORef (statePosLine s) posLine
                writeIORef (statePosCol  s) posCol
                f)
         c


---------------------------------------------------------------------- Names --
isNameChar :: Char -> Bool
isNameChar x
 = Char.isAlpha x || Char.isDigit x || x == '-' || x == '_' || x == '\''


takeName :: Parser Text
takeName s f c
 =  takeChar    s f $ \x
 -> takeName1 x s f c


takeName1 :: Char -> Parser Text
takeName1 x s f c
 = if Char.isLower x
        then takeNameBody s f $ \xs -> c (Text.pack (x : xs))
        else f


takeNameBody' :: Parser [Char]
takeNameBody' s f c
 =  takeChar s f $ \x
 -> if isNameChar x
    then takeNameBody s f $ \xs
      -> c (x : xs)
    else f


takeNameBody :: Parser [Char]
takeNameBody s f c
 =  peekEnd s f $ \b
 -> if b then c []
    else peekChar s f $ \x
      -> if isNameChar x
         then nextChar s f $ \_
           -> takeNameBody s f $ \xs
           -> c (x : xs)
         else c []


--------------------------------------------------------------------- Tokens --
takeSpaceName :: Parser Token
takeSpaceName s f c
 =  takeChar s f $ \x
 -> takeSpaceName1 x s f c

takeSpaceName1 :: Char -> Parser Token
takeSpaceName1 x s f c
 = case x of
        '#'     -> takeNameWithSpace SPrm s f c
        '@'     -> takeNameWithSpace SMac s f c
        '%'     -> takeNameWithSpace SSym s f c
        '+'     -> takeNameWithSpace SSet s f c
        '?'     -> takeNameWithSpace SNom s f c

        _ | Char.isLower x
                -> takeNameBody s f $ \xs
                -> c (KName SVar $ Text.pack (x : xs))

        _       -> f

takeNameWithSpace :: Space -> Parser Token
takeNameWithSpace space s f c
 =  takeNameBody' s f $ \xs
 -> c (KName space $ Text.pack xs)


takeNameOfSpace :: Space -> Parser Token
takeNameOfSpace space s f c
 =  takeChar         s f $ \x
 -> takeSpaceName1 x s f $ \k
 -> case k of
        KName space' _
         | space == space' -> c k
        _ -> f


takeToken  :: Parser Token
takeToken s f c
 =  takeChar s f $ \x
 -> case x of
        -- Whitespace
        ' '     -> takeToken s f c
        '\t'    -> takeToken s f c
        '\n'    -> takeToken s f c

        -- Punctuation
        '('     -> c (KPunc x)
        ')'     -> c (KPunc x)
        '{'     -> c (KPunc x)
        '}'     -> c (KPunc x)
        ']'     -> c (KPunc x)
        '['     -> c (KPunc x)
        '.'     -> c (KPunc x)
        ','     -> c (KPunc x)
        ';'     -> c (KPunc x)
        '='     -> c (KPunc x)
        '$'     -> c (KPunc x)
        '!'     -> c (KPunc x)
        '~'     -> c (KPunc x)

        -- Names
        _       -> takeSpaceName1 x s f c


------------------------------------------------------------------------ Exp --
-- | Take an expression.
takeExp :: Parser Exp
takeExp s f c
 =  takeToken  s f $ \k
 -> takeExp1 k s f c


-- | Take an expression including the given token.
takeExp1 :: Token -> Parser Exp
takeExp1 k s f c
 = case k of
        KPunc '~'
         -> takeExp s f $ \x
         -> c (XDel x)

        KPunc '!'
         -> takeExp s f $ \x
         -> c (XNow x)

        KPunc '{'
         -> takeToken s f $ \k
         -> takeExpAbsParams1 k s f $ \ps
         -> takeExp s f $ \x
         -> let (bs, ns) = unzip ps
            in  c (XAbs bs ns x)

        _ -> takeExpApp1 k s f c


-- | Take parameters of an abstraction.
takeExpAbsParams1 :: Token -> Parser [(Bool, Name)]
takeExpAbsParams1 k s f c
 = case k of
        KPunc '~'
         -> takeName  s f $ \n
         -> takeToken s f $ \k
         -> takeExpAbsParams1 k s f $ \ps
         -> c ((False, n) : ps)

        KPunc '!'
         -> takeName  s f $ \n
         -> takeToken s f $ \k
         -> takeExpAbsParams1 k s f $ \ps
         -> c ((True, n) : ps)

        KName SVar n
         -> takeToken s f $ \k
         -> takeExpAbsParams1 k s f $ \ps
         -> c ((True, n) : ps)

        KPunc '}'
         -> c []

        _ -> f


--------------------------------------------------------------------- ExpApp --
-- | Expression or application.
takeExpApp1 :: Token -> Parser Exp
takeExpApp1 k s f c
 = case k of
        KName SPrm n
         -> takeExpArgs s f $ \xx
         -> case xx of
                []      -> f
                x : xs  -> c (makeXApps (XPrm (POPrim n) x) xs)

        _ -> takeExpArg1 k s f $ \x
          -> takeExpArgs s f $ \xs
          -> c (makeXApps x xs)


--------------------------------------------------------------------- ExpArg --
takeExpArgs :: Parser [Exp]
takeExpArgs s f c
 =  try takeExpArg s (c []) $ \x
 -> takeExpArgs s f $ \xs
 -> c (x : xs)

takeExpArg :: Parser Exp
takeExpArg s f c
 =  takeToken     s f $ \k
 -> takeExpArg1 k s f c

takeExpArg1 :: Token -> Parser Exp
takeExpArg1 k s f c
 = case k of
        KName SVar name -> c $ XVar name 0
        KName SMac name -> c $ XMac name
        KName SSym name -> c $ XSym name
        KName SSet name -> c $ XSet name

        KName SPrm name
         -> case readLitVal name of
                Just v  -> c $ XVal v
                _       -> error $ "unknown literal" ++ show name

        KPunc '('
         -> takeExp   s f $ \x
         -> takeToken s f $ \case
                KPunc ')' -> c x
                _         -> f

        KPunc '['
         -> takeToken        s f $ \k
         -> takeExpArgVec1 k s f $ \xs
         -> c (XVec xs)

        _ -> f

takeExpArgVec1 :: Token -> Parser [Exp]
takeExpArgVec1 k s f c
 = case k of
        KPunc ']' -> c []

        _ -> takeExp1  k s f $ \x
          -> takeToken   s f $ \k
          -> case k of
                KPunc ',' -> takeToken s f $ \k
                          -> takeExpArgVec1 k s f $ \xs
                          -> c (x : xs)

                KPunc ']' -> c [x]

                _         -> f
