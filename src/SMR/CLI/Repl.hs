module SMR.CLI.Repl where
import SMR.Core.Exp
import qualified SMR.Core.Step                  as Step
import qualified SMR.Source.Parser              as Source
import qualified SMR.Source.Lexer               as Source
import qualified SMR.Source.Pretty              as Source
import qualified Data.Text.Lazy.IO              as TL
import qualified Data.Text.Lazy.Builder         as BL
import qualified System.Console.Haskeline       as HL
import qualified Data.Char                      as Char
import qualified Data.List                      as List
import qualified Data.Map                       as Map
import qualified Data.Text                      as Text
import Control.Monad.IO.Class
import Data.Text                                (Text)
import Data.Maybe
import Data.Monoid


-------------------------------------------------------------------------------
data Mode s p
        = ModeNone
        | ModeParse
        | ModePush (Exp s p)
        | ModeStep (Step.Config s p) (Exp s p)
        deriving Show

data State s p
        = State
        { stateMode     :: Mode s p
        , stateDecls    :: [Decl s p] }


-------------------------------------------------------------------------------
replStart :: State Text Text -> IO ()
replStart state
 = HL.runInputT HL.defaultSettings
 $ do   HL.outputStrLn "Shimmer, version 0.1. The Lambda Machine."
        replLoop state


-- | Main repl loop dispatcher
replLoop :: State Text Text  -> HL.InputT IO ()
replLoop state
 = do   minput  <- HL.getInputLine "> "
        case minput of
         Nothing
          -> return ()

         Just "quit"
          -> return ()

         Just input
          |  all Char.isSpace input
          -> case stateMode state of
                ModeNone        -> replLoop state
                ModePush xx     -> replPush_next state xx
                ModeStep c xx   -> replStep_next state c xx

          |  Just str <- List.stripPrefix ":parse" input
          -> replParse state str

          |  Just str <- List.stripPrefix ":push"  input
          -> replPush_load state str

          |  Just str <- List.stripPrefix ":step"  input
          -> replStep_load state str

          | otherwise
          -> do HL.outputStrLn $ "Input was: " ++ input
                replLoop state


-- | Parse and print back an expression.
replParse :: State Text Text -> String -> HL.InputT IO ()
replParse state str
 = do   result  <- liftIO $ replParseExp state str
        case result of
         Nothing
          -> replLoop state

         Just xx
          -> do liftIO  $ TL.putStrLn
                        $ BL.toLazyText
                        $ Source.buildExp Source.CtxTop xx

                replLoop state


-------------------------------------------------------------------------------
-- | Parse an expression and push down substitutions.
replPush_load :: State Text Text -> String -> HL.InputT IO ()
replPush_load state str
 = do   result  <- liftIO $ replParseExp state str
        case result of
         Nothing -> replLoop state
         Just xx -> replPush_next state xx


-- | Advance the train pusher.
replPush_next :: State Text Text -> Exp Text Text -> HL.InputT IO ()
replPush_next state xx
 = case pushDeep xx of
        Nothing -> replLoop $ state { stateMode = ModeNone }
        Just xx'
         -> do  liftIO  $ TL.putStrLn
                        $ BL.toLazyText
                        $ Source.buildExp Source.CtxTop xx'

                replLoop $ state { stateMode = ModePush xx' }


-------------------------------------------------------------------------------
-- | Parse an expression and single-step it.
replStep_load :: State Text Text -> String -> HL.InputT IO ()
replStep_load state str
 = do   result  <- liftIO $ replParseExp state str
        case result of
         Nothing -> replLoop state

         Just xx
          -> let
                decls   = Map.fromList
                        $ [ (n, x) | DeclMac n x <- stateDecls state ]

                config  = Step.Config
                        { Step.configUnderLambdas = True
                        , Step.configHeadArgs     = True
                        , Step.configDeclsMac     = decls }

             in replStep_next state config xx


-- | Advance the single stepper.
replStep_next
        :: State Text Text
        -> Step.Config Text Text
        -> Exp Text Text
        -> HL.InputT IO ()

replStep_next state config xx
 = case Step.step config xx of
        Left Step.ResultDone
         -> replLoop $ state { stateMode = ModeNone }

        Left (Step.ResultError msg)
         -> do  HL.outputStrLn
                        $ Text.unpack
                        $ Text.pack "error: " <> msg

        Right xx'
         -> do  liftIO  $ TL.putStrLn
                        $ BL.toLazyText
                        $ Source.buildExp Source.CtxTop xx'

                replLoop $ state { stateMode = ModeStep config xx' }


-------------------------------------------------------------------------------
replParseExp :: State Text Text -> String -> IO (Maybe (Exp Text Text))
replParseExp state str
 = do   let (ts, loc, csRest)
                = Source.lexTokens (Source.L 1 1) str

        let config
                = Source.Config
                { Source.configReadSym  = Just
                , Source.configReadPrm  = Just }

        case Source.parseExp config ts of
         Left err
          -> do liftIO $ print err
                return Nothing

         Right xx
          -> return (Just xx)


