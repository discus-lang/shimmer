
module Main where
import SMR.Core.Exp.Base
import qualified SMR.Source.Parser              as Source
import qualified SMR.Source.Lexer               as Source
import qualified SMR.CLI.Config                 as Config
import qualified System.Environment             as System
import qualified System.Exit                    as System
import qualified System.IO                      as System
import qualified System.Console.Haskeline       as HL
import qualified Data.Char                      as Char


main :: IO ()
main
 = do   args    <- System.getArgs
        config  <- Config.parseArgs args Config.configZero

        case Config.configMode config of
         Config.ModeNone
          -> do putStr $ Config.usage
                System.exitSuccess

         Config.ModeCheck file
          -> runCheck file

         Config.ModeREPL file
          -> runRepl  file


runCheck :: FilePath -> IO ()
runCheck path
 = do   str     <- readFile path

        let (ts, loc, csRest)
                = Source.lexTokens (Source.L 1 1) str

        let config
                = Source.Config
                { Source.configReadSym  = Just
                , Source.configReadPrm  = Just }

        case Source.parseDecls config ts of
         Left err       -> error $ show err
         Right decls
          -> do putStrLn $ show decls


runRepl :: FilePath -> IO ()
runRepl path
 = do   str     <- readFile path

        let (ts, loc, csRest)
                = Source.lexTokens (Source.L 1 1) str

        let config
                = Source.Config
                { Source.configReadSym  = Just
                , Source.configReadPrm  = Just }

        case Source.parseDecls config ts of
         Left err       -> error $ show err
         Right decls    -> repl_start decls



repl_start :: [Decl s p] -> IO ()
repl_start decls
 = HL.runInputT HL.defaultSettings (repl_loop decls)

repl_loop  :: [Decl s p] -> HL.InputT IO ()
repl_loop decls
 = do   minput  <- HL.getInputLine "> "
        case minput of
         Nothing
          -> return ()

         Just "quit"
          -> return ()

         Just input
          | all Char.isSpace input
          -> repl_loop decls

          | otherwise
          -> do HL.outputStrLn $ "Input was: " ++ input
                repl_loop decls


