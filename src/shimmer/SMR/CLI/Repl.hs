{-# LANGUAGE BangPatterns #-}
module SMR.CLI.Repl where
import SMR.Core.Exp
import qualified SMR.CLI.Help                   as Help
import qualified SMR.CLI.Driver.Load            as Driver
import qualified SMR.Core.Eval                  as Eval
import qualified SMR.Core.Step                  as Step
import qualified SMR.Core.World                 as World
import qualified SMR.Core.Prim                  as Prim
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
data Mode w
        = ModeNone
        | ModeParse
        | ModeEval (Step.Config w) Exp


data State w
        = State
        { -- | Current interpreter mode.
          stateMode     :: Mode w

          -- | Top-level declarations parsed from source files.
        , stateDecls    :: [Decl]

          -- | Working source files.
        , stateFiles    :: [FilePath]

          -- | Execution world.
        , stateWorld    :: World.World w }


type RState     = State ()
type RConfig    = Step.Config ()
type RWorld     = World.World  ()
type RDecl      = Decl
type RExp       = Exp


-------------------------------------------------------------------------------
replStart :: RState -> IO ()
replStart state
 = HL.runInputT HL.defaultSettings
 $ do   HL.outputStrLn "Shimmer v0.2. The Lambda Machine."
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
                ":eval"    : xs   -> replEval    state (unwords xs)
                ":trace"   : xs   -> replTrace   state (unwords xs)
                _                 -> replEval    state input


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
         , "  #list                   list constructor" ]

        HL.outputStr
         $ unlines
         $ [   leftPad 16 ("  #" ++ (Text.unpack $ name))
            ++ Text.unpack (Prim.primEvalDesc p)

           | p@(Prim.PrimEval { Prim.primEvalName = Prim.POPrim name })
                <- Prim.primOps ]

        replLoop state

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
                $  fmap concat $ mapM Driver.runLoadFileDecls
                $  stateFiles state

        replLoop (state
                { stateDecls    = decls })


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
-- | Parse an expression and single-step it.
replEval :: RState -> String -> HL.InputT IO ()
replEval state str
 = replLoadExp state str replEval_next

-- | Advance the single stepper.
replEval_next
        :: RState -> RConfig -> RExp
        -> HL.InputT IO ()

replEval_next state config xx
 = do   vs <- liftIO $ Eval.eval config (stateWorld state) [] xx

        liftIO  $ TL.putStrLn
                $ BL.toLazyText
                $ Source.buildExp Source.CtxTop (XVec $ map XVal vs)

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
                        $ [ (Prim.primEvalName p, p) | p <- Prim.primOps ]

                config  = Step.Config
                        { Step.configDeclsMac     = decls
                        , Step.configPrims        = prims }

              in eat state config xx


-------------------------------------------------------------------------------
replParseExp :: RState -> String -> IO (Maybe RExp)
replParseExp _state str
 = do   let (ts, _loc, _csRest)
                = Source.lexTokens (Source.L 1 1) str

        let config = Source.Config
        case Source.parseExp config ts of
         Left err
          -> do liftIO  $ putStrLn
                        $ "parse error\n"
                        ++ Source.pprParseError err
                return Nothing

         Right xx
          -> return (Just xx)

