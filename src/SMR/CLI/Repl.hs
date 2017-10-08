{-# LANGUAGE BangPatterns #-}
module SMR.CLI.Repl where
import SMR.Core.Exp
import qualified SMR.CLI.Help                   as Help
import qualified SMR.Core.Step                  as Step
import qualified SMR.Core.World                 as World
import qualified SMR.Prim.Name                  as Prim
import qualified SMR.Prim.Op                    as Prim
import qualified SMR.Prim.Op.Base               as Prim
import qualified SMR.Source.Parser              as Source
import qualified SMR.Source.Lexer               as Source
import qualified SMR.Source.Pretty              as Source
import qualified SMR.Source.Expected            as Source
import qualified Data.Text.Lazy.IO              as TL
import qualified Data.Text.Lazy.Builder         as BL
import qualified System.Console.Haskeline       as HL
import qualified Data.Char                      as Char
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import qualified Data.Text                      as Text
import Control.Monad.IO.Class
import Data.Text                                (Text)
import Data.Set                                 (Set)
import Data.Monoid


-------------------------------------------------------------------------------
data Mode s p w
        = ModeNone
        | ModeParse
        | ModePush (Exp s p)
        | ModeStep (Step.Config s p w) (Exp s p)


data State s p w
        = State
        { -- | Current interpreter mode.
          stateMode     :: Mode s p w

          -- | Top-level declarations parsed from source files.
        , stateDecls    :: [Decl s p]

          -- | Working source files.
        , stateFiles    :: [FilePath]

          -- | Execution world.
        , stateWorld    :: World.World w }


type RState     = State Text Prim.Prim ()
type RConfig    = Step.Config Text Prim.Prim ()
type RWorld     = World.World  ()
type RDecl      = Decl  Text Prim.Prim
type RExp       = Exp   Text Prim.Prim


-------------------------------------------------------------------------------
replStart :: RState -> IO ()
replStart state
 = HL.runInputT HL.defaultSettings
 $ do   HL.outputStrLn "Shimmer, version 0.1. The Lambda Machine."
        HL.outputStrLn "Type :help for help."
        replReload state


-- | Main repl loop dispatcher
replLoop :: RState -> HL.InputT IO ()
replLoop state
 = do   minput  <- HL.getInputLine "> "
        case minput of
         Nothing
          -> return ()

         Just input
          |  all Char.isSpace input
          -> case stateMode state of
                ModeNone        -> replLoop state
                ModePush xx     -> replPush_next state xx
                ModeStep c xx   -> replStep_next state c xx
                _               -> replLoop state

          | otherwise
          -> case words input of
                ":quit"    : []   -> replQuit    state
                ":help"    : []   -> replHelp    state
                ":reload"  : []   -> replReload  state
                ":r"       : []   -> replReload  state
                ":grammar" : []   -> replGrammar state
                ":prims"   : []   -> replPrims   state

                ":decls"   : xs
                 -> let strip ('@' : name) = name
                        strip name         = name
                    in  replDecls state
                                $ Set.fromList $ map Text.pack
                                $ map strip xs

                ":parse"   : xs   -> replParse   state (unwords xs)
                ":push"    : xs   -> replPush    state (unwords xs)
                ":step"    : xs   -> replStep    state (unwords xs)
                ":steps"   : xs   -> replSteps   state (unwords xs)
                ":trace"   : xs   -> replTrace   state (unwords xs)
                _                 -> replSteps   state input


-------------------------------------------------------------------------------
-- | Quit the repl.
replQuit  :: RState -> HL.InputT IO ()
replQuit _state
 = do   return ()


-------------------------------------------------------------------------------
-- | Display the help page.
replHelp  :: RState -> HL.InputT IO ()
replHelp state
 = do   HL.outputStr $ Help.helpCommands
        replLoop state


-------------------------------------------------------------------------------
-- | Display the language grammar.
replGrammar  :: RState -> HL.InputT IO ()
replGrammar state
 = do   HL.outputStr $ Help.helpGrammar
        replLoop state


-------------------------------------------------------------------------------
-- | Display the list of primops.
replPrims  :: RState -> HL.InputT IO ()
replPrims state
 = do   HL.outputStrLn
         $ "  name          params    description"

        HL.outputStrLn
         $ "  ----          ------    -----------"

        HL.outputStr
         $ unlines
         [ "  #unit                   unit value"
         , "  #true                   boolean true"
         , "  #false                  boolean false"
         , "  #nat'NAT                natural number"
         , "  #list                   list constructor"
         , "" ]

        HL.outputStr
         $ unlines
         $ [   leftPad 16 ("  #" ++ (Text.unpack $ name))
            ++ leftPad 10  (concat [showForm f | f <- Prim.primEvalForm p])
            ++ Text.unpack (Prim.primEvalDesc p)

           | p@(Prim.PrimEval { Prim.primEvalName = Prim.PrimOp name })
                <- Prim.primEvals ]

        replLoop state

showForm :: Form -> String
showForm PVal   = "!"
showForm PExp   = "~"

leftPad :: Int -> [Char] -> [Char]
leftPad n ss
 = ss ++ replicate (n - length ss) ' '


-------------------------------------------------------------------------------
-- | Display the list of current declarations.
replDecls :: RState -> Set Name -> HL.InputT IO ()
replDecls state names
 = do   liftIO  $ mapM_ (printDecl names)
                $ stateDecls state

        replLoop state


printDecl :: Set Name -> RDecl -> IO ()
printDecl names decl
 | Set.null names
 = do TL.putStr
         $ BL.toLazyText
         $ Source.buildDecl decl

 | DeclMac name _ <- decl
 , Set.member name names
 = do   TL.putStr
         $ BL.toLazyText
         $ Source.buildDecl decl

 | otherwise
 = return ()


-------------------------------------------------------------------------------
-- | Reload the current source file.
replReload :: RState -> HL.InputT IO ()
replReload state
 = do
        decls   <- liftIO
                $  fmap concat $ mapM (loadDecls state)
                $  stateFiles state

        replLoop (state
                { stateDecls    = decls })


loadDecls :: RState -> FilePath -> IO [RDecl]
loadDecls _state path
 = do
        let config
                = Source.Config
                { Source.configReadSym  = Just
                , Source.configReadPrm  = Prim.readPrim Prim.primOpTextNames }

        str     <- readFile path
        let (ts, _loc, _csRest)
                = Source.lexTokens (Source.L 1 1) str

        case Source.parseDecls config ts of
           Left err
            -> do  liftIO
                        $ putStrLn
                        $ "parse error\n"
                        ++ Source.pprParseError err
                   return []

           Right decls
            -> return decls


-------------------------------------------------------------------------------
-- | Parse and print back an expression.
replParse :: RState -> String -> HL.InputT IO ()
replParse state str
 = do   result  <- liftIO $ replParseExp state str
        case result of
         Nothing
          -> replLoop state

         Just xx
          -> do liftIO  $ TL.putStrLn
                        $ BL.toLazyText
                        $ Source.buildExp Source.CtxTop xx
                HL.outputStr "\n"

                replLoop state


-------------------------------------------------------------------------------
-- | Parse an expression and push down substitutions.
replPush :: RState -> String -> HL.InputT IO ()
replPush state str
 = do   result  <- liftIO $ replParseExp state str
        case result of
         Nothing -> replLoop state
         Just xx -> replPush_next state xx


-- | Advance the train pusher.
replPush_next :: RState -> RExp -> HL.InputT IO ()
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
replStep :: RState -> String -> HL.InputT IO ()
replStep state str
 = replLoadExp state str replStep_next

-- | Advance the single stepper.
replStep_next
        :: RState -> RConfig -> RExp
        -> HL.InputT IO ()

replStep_next state config xx
 = do   erx     <- liftIO $ Step.step config (stateWorld state) xx
        case erx of
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
-- | Parse an expression and normalize it.
replSteps :: RState -> String -> HL.InputT IO ()
replSteps state str
 = replLoadExp state str replSteps_next

-- | Advance the evaluator stepper.
replSteps_next
        :: RState -> RConfig -> RExp
        -> HL.InputT IO ()

replSteps_next state config xx
 = do   erx     <- liftIO $ Step.steps config (stateWorld state) xx
        case erx of
         Left msg
          -> do  HL.outputStrLn
                         $ Text.unpack
                         $ Text.pack "error: " <> msg

         Right xx'
          -> do  liftIO  $ TL.putStrLn
                         $ BL.toLazyText
                         $ Source.buildExp Source.CtxTop xx'

                 replLoop $ state { stateMode = ModeNone }


-------------------------------------------------------------------------------
-- | Parse an expression and normalize it,
--   printing out each intermediate state.
replTrace :: RState -> String -> HL.InputT IO ()
replTrace state str
 = replLoadExp state str replTrace_next

-- | Advance the evaluator stepper.
replTrace_next
        :: RState -> RConfig -> RExp
        -> HL.InputT IO ()

replTrace_next state config !xx0
 = loop xx0
 where
  loop !xx
   = do erx <- liftIO $ Step.step config (stateWorld state) xx
        case erx of
         Left (Step.ResultError msg)
          -> do  HL.outputStrLn
                  $ Text.unpack
                  $ Text.pack "error: " <> msg

         Left Step.ResultDone
          -> replLoop $ state { stateMode = ModeNone }

         Right xx'
          -> do  liftIO  $ TL.putStrLn
                         $ BL.toLazyText
                         $ Source.buildExp Source.CtxTop xx'

                 loop xx'

-------------------------------------------------------------------------------
replLoadExp
        :: RState -> String
        -> (RState -> RConfig -> RExp -> HL.InputT IO ())
        -> HL.InputT IO ()
replLoadExp state str eat
 = do   result  <- liftIO $ replParseExp state str
        case result of
         Nothing -> replLoop state

         Just xx
          -> let
                decls   = Map.fromList
                        $ [ (n, x) | DeclMac n x <- stateDecls state ]

                prims   = Map.fromList
                        $ [ (Prim.primEvalName p, p) | p <- Prim.primEvals ]

                config  = Step.Config
                        { Step.configUnderLambdas = True
                        , Step.configHeadArgs     = True
                        , Step.configDeclsMac     = decls
                        , Step.configPrims        = prims }

              in eat state config xx


-------------------------------------------------------------------------------
replParseExp :: RState -> String -> IO (Maybe RExp)
replParseExp _state str
 = do   let (ts, _loc, _csRest)
                = Source.lexTokens (Source.L 1 1) str

        let config
                = Source.Config
                { Source.configReadSym  = Just
                , Source.configReadPrm  = Prim.readPrim Prim.primOpTextNames }

        case Source.parseExp config ts of
         Left err
          -> do liftIO  $ putStrLn
                        $ "parse error\n"
                        ++ Source.pprParseError err
                return Nothing

         Right xx
          -> return (Just xx)

