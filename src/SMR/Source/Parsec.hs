
module SMR.Source.Parsec where
import qualified SMR.Data.Bag   as Bag
import SMR.Data.Bag             (Bag)

-------------------------------------------------------------------------------
-- | Parser is a function that takes a list of tokens,
--   and returns a list of remaining tokens along with
--    (on error)   a list of descriptions of expected input,
--    (on success) a parsed value.
--
data Parser t e a
        = Parser ([t] -> ParseResult t e a)


-- | Result of a parser,
--   parameterised by
--      (t) the type of tokens,
--      (e) the type for decriptions of what we're expecting to parse.
--      (a) type of value to parse.
--
data ParseResult t e a
        -- Parser failed after consuming no input.
        --  The parser looked at one or more tokens at the front of the
        --  input but based on these the input does not look like whatever
        --  syntax the parser was supposed to parse.
        = ParseSkip
            (Bag (Blocker t e)) -- ^ Where we got blocked trying other parses.

        -- Parser yielding a value after consuming no input.
        --  The parser returned a value without looking at any tokens,
        --  this is a pure value returning action.
        | ParseReturn
            (Bag (Blocker t e)) -- ^ Where we got blocked trying other parses.
            a                   -- ^ Produced value.

        -- Parse failed after partially consuming input.
        --   The parser thought that the input sequence looked like what it
        --   was supposed to parse, but complete parsing failed once it
        --   had committed.
        | ParseFailure
           (Bag (Blocker t e))  -- ^ Where we got blocked trying other parses.

        -- Parse succeeded yielding a value after consuming input.
        --   We have a complete value, and have consumed some input tokens.
        | ParseSuccess
            a                   -- ^ Produced value.
           [t]                  -- ^ Remaining input tokens.


-- | Describes why the parser could not make further progress.
data Blocker t e
        = Blocker
        { blockerTokens   :: [t] -- ^ Remaining input tokens where we failed.
        , blockerExpected :: e   -- ^ Description of what we were expecting.
        }


-------------------------------------------------------------------------------
-- | Apply a parser to a list of input tokens.
parse :: Parser t e a -> [t] -> ParseResult t e a
parse (Parser p) ts = p ts


-- Functor --------------------------------------------------------------------
instance Functor (Parser t e) where
 fmap f parserA
  = Parser $ \ts0
  -> case parse parserA ts0 of
        ParseSkip    bs1        -> ParseSkip    bs1
        ParseReturn  bs1 x      -> ParseReturn  bs1 (f x)
        ParseFailure bs1        -> ParseFailure bs1
        ParseSuccess a ts1      -> ParseSuccess (f a) ts1


-- Applicative ----------------------------------------------------------------
instance Applicative (Parser t e) where
 pure x
  = Parser $ \_
  -> ParseReturn Bag.nil x

 (<*>) parserF parserA
  = Parser $ \ts0
  -> case parse parserF ts0 of
        ParseSkip es1
         -> ParseSkip es1

        ParseFailure bs1
         -> ParseFailure bs1

        ParseReturn es1 f
         -> case parse parserA ts0 of
             ParseSkip    es2   -> ParseSkip    (Bag.union es1 es2)
             ParseReturn  es2 x -> ParseReturn  (Bag.union es1 es2) (f x)
             ParseFailure bs2   -> ParseFailure (Bag.union es1 bs2)
             ParseSuccess x ts2 -> ParseSuccess (f x) ts2

        ParseSuccess f ts1
         -> case parse parserA ts1 of
             ParseSkip    bs2   -> ParseFailure bs2
             ParseReturn  _ x   -> ParseSuccess (f x) ts1
             ParseFailure bs2   -> ParseFailure bs2
             ParseSuccess x ts2 -> ParseSuccess (f x) ts2


-- Monad ----------------------------------------------------------------------
instance Monad (Parser t e) where
 return x
  = Parser $ \_
  -> ParseReturn Bag.nil x

 (>>=) parserA mkParserB
  = Parser $ \ts0
  -> case parse parserA ts0 of
        ParseSkip bs1
         -> ParseSkip bs1

        ParseFailure bs1
         -> ParseFailure bs1

        -- First parser produced a value but did not consume input.
        ParseReturn _ xa
         -> parse (mkParserB xa) ts0

        -- First parser produced a value and consumed input.
        ParseSuccess xa ts1
         -> case parse (mkParserB xa) ts1 of
             -- The second parser skipped, but as we've already consumed
             -- input tokens we treat this as a failure.
             ParseSkip    bs2    -> ParseFailure bs2

             -- The second parser returned a value, and though it didn't
             -- consume input itself, the whole computation has,
             -- so still treat this as a success.
             ParseReturn  _ xb   -> ParseSuccess xb ts1

             -- The second parser failed.
             ParseFailure bs2    -> ParseFailure bs2

             -- The second parser suceeded, to take the new value.
             ParseSuccess xb ts2 -> ParseSuccess xb ts2


-- Prim -----------------------------------------------------------------------
-- Primitive parsers.

-- | Always fail, producing no possible parses and no helpful error message.
fail :: Parser t e a
fail
 =  Parser $ \_
 -> ParseFailure Bag.nil


-- | Always fail, yielding the given message describing what was expected.
expected :: e -> Parser t e a
expected xe
 =  Parser $ \ts
 -> ParseFailure (Bag.singleton (Blocker ts xe))


-- | Commit to the given parser, so if it skips or returns without
--   consuming any input then treat that as failure.
commit :: Parser t e a -> Parser t e a
commit parserA
 =  Parser $ \ts0
 -> case parse parserA ts0 of
        ParseSkip    bs1        -> ParseFailure bs1
        ParseReturn  bs1 _      -> ParseFailure bs1
        ParseFailure bs1        -> ParseFailure bs1
        ParseSuccess xb xs2     -> ParseSuccess xb xs2


-- | Parse in an expectation context.
enter :: (Bag (Blocker t e) -> e) -> Parser t e a -> Parser t e a
enter mk parserA
 = Parser $ \ts0
 -> case parse parserA ts0 of
        ParseSkip    bs1
         -> ParseSkip    (Bag.singleton (Blocker ts0 (mk bs1)))

        ParseReturn  bs1 x
         -> ParseReturn  (Bag.singleton (Blocker ts0 (mk bs1))) x

        ParseFailure bs1
         -> ParseFailure (Bag.singleton (Blocker ts0 (mk bs1)))

        ParseSuccess xb ts2
         -> ParseSuccess xb ts2


-- | If the given parser suceeds then enter an expectation context
--   for the next one.
enterOn :: Parser t e a
        -> (a -> Bag (Blocker t e) -> e)
        -> (a -> Parser t e b)
        -> Parser t e b

enterOn parserA mk mkParserB
 = Parser $ \ts0
 -> case parse parserA ts0 of
        ParseSkip bs0
         -> ParseSkip bs0

        ParseFailure bs1
         -> ParseFailure bs1

        ParseReturn _ xa
         -> case parse (mkParserB xa) ts0 of
                ParseSkip bs2
                 -> ParseSkip    (Bag.singleton (Blocker ts0 (mk xa bs2)))

                ParseReturn bs2 xb
                 -> ParseReturn  (Bag.singleton (Blocker ts0 (mk xa bs2))) xb

                ParseFailure bs2
                 -> ParseFailure (Bag.singleton (Blocker ts0 (mk xa bs2)))

                ParseSuccess xb ts2
                 -> ParseSuccess xb ts2


        ParseSuccess xa ts1
         -> case parse (mkParserB xa) ts1 of
                ParseSkip bs2
                 -> ParseSkip    (Bag.singleton (Blocker ts0 (mk xa bs2)))

                ParseReturn bs2 xb
                 -> ParseReturn  (Bag.singleton (Blocker ts0 (mk xa bs2))) xb

                ParseFailure bs2
                 -> ParseFailure (Bag.singleton (Blocker ts0 (mk xa bs2)))

                ParseSuccess xb ts2
                 -> ParseSuccess xb ts2


-- | Peek at the first input token, without consuming at it.
peek :: Parser t e t
peek
 = Parser $ \ts
 -> case ts of
        []              -> ParseFailure Bag.nil
        t : _           -> ParseReturn  Bag.nil t


-- | Consume the first input token, failing if there aren't any.
item :: e -> Parser t e t
item xe
 = Parser $ \ts
 -> case ts of
        []              -> ParseSkip   (Bag.singleton (Blocker ts xe))
        t : ts'         -> ParseSuccess t ts'


-- | Consume the first input token if it matches the given predicate,
--   failing without consuming if the predicate does not match.
satisfies :: e -> (t -> Bool) -> Parser t e t
satisfies xe p
 = Parser $ \ts
 -> case ts of
        []              -> ParseSkip    (Bag.singleton (Blocker ts xe))
        t : ts'
         | p t          -> ParseSuccess t ts'
         | otherwise    -> ParseSkip    (Bag.singleton (Blocker ts xe))


-- | Consume the first input token if it is accepted by the given match
--   function. Fail without consuming if there is no match.
from :: e -> (t -> Maybe a) -> Parser t e a
from xe accept
 = Parser $ \ts
 -> case ts of
        []              -> ParseSkip    (Bag.singleton (Blocker ts xe))
        t : ts'
         -> case accept t of
               Just x   -> ParseSuccess x ts'
               Nothing  -> ParseSkip    (Bag.singleton (Blocker ts xe))


-- | Given two parsers, try the first and if it succeeds produce
--   the output of that parser, if not try the second.
alt :: Parser t e a -> Parser t e a -> Parser t e a
alt parserA parserB
 = alts (parserA : parserB : [])


-- | Like 'alt' but take a list of parser, trying them in order.
alts :: [Parser t e a] -> Parser t e a
alts parsers
 = Parser $ \ts0
 -> go ts0 (False, Nothing) (Bag.nil, Bag.nil) parsers
 where
        go _   (False, Nothing)  (bsSkip, _bsFail) []
         = ParseSkip    bsSkip

        go _   (False, (Just x)) (bsSkip, _bsFail) []
         = ParseReturn  bsSkip x

        go _   (True,  _)        (_bsSkip, bsFail) []
         = ParseFailure bsFail

        go ts0 (failed, mx)      (bsSkip, bsFail) (p : ps)
         = case parse p ts0 of
            ParseSkip    bs1
             -> go ts0 (failed, mx)     (Bag.union bsSkip bs1, bsFail) ps

            ParseFailure bs1
             -> go ts0 (True,   mx)     (bsSkip, Bag.union bsFail bs1) ps

            ParseReturn  bs1 x
             -> go ts0 (failed, Just x) (Bag.union bsSkip bs1, bsFail) ps

            ParseSuccess x ts1
             -> ParseSuccess  x ts1


-- Derived --------------------------------------------------------------------
-- Parsers derived from the primitive ones.

-- | Parse zero or more things, yielding a list of those things.
some :: Parser t e a -> Parser t e [a]
some parserA
 = alt (do
        x       <- parserA
        xs      <- some parserA
        return  $ x : xs)
       (return [])


-- | Parse one or more things, yielding a list of those things.
many :: Parser t e a -> Parser t e [a]
many parserA
 = do   x       <- parserA
        xs      <- some parserA
        return  $ x : xs


-- | Parse some things separated by other things.
sepBy   :: Parser t e a -> Parser t e s -> Parser t e [a]
sepBy parserA parserS
 = alt  (sepBy1 parserA parserS)
        (return [])


-- | Parse at least one thing separated by other things.
sepBy1  :: Parser t e a -> Parser t e s -> Parser t e [a]
sepBy1 parserA parserS
 = do   x       <- parserA
        alt
         (do    _s      <- parserS
                xs      <- sepBy1 parserA parserS
                return  $ x : xs)

         (do    return  $ x : [])


-- | Run a parser, peeking at the starting and ending tokens.
withDelims :: Parser t e a -> Parser t e (t, a, t)
withDelims p
 = do   kStart  <- peek
        x       <- p
        kEnd    <- peek
        return  (kStart, x, kEnd)


