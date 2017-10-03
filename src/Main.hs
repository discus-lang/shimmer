
module Main where
import SMR.Core.Exp.Base
import SMR.Core.Exp.Train
import SMR.Core.Exp.Push
import qualified SMR.CLI.Config                 as Config
import qualified SMR.CLI.Repl                   as Repl
import qualified SMR.Source.Parser              as Source
import qualified SMR.Source.Lexer               as Source
import qualified SMR.Source.Pretty              as Source
import qualified System.Environment             as System
import qualified System.Exit                    as System
import qualified System.IO                      as System
import qualified Data.Text.Lazy.IO              as TL
import qualified Data.Text.Lazy.Builder         as BL
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
         Left err
          -> error $ show err

         Right decls
          -> TL.putStr
                $ BL.toLazyText
                $ mconcat
                $ map Source.buildDecl decls


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
         Left err
          -> error $ show err

         Right decls
          -> Repl.replStart
                $ Repl.State
                { Repl.stateMode        = Repl.ModeNone
                , Repl.stateDecls       = decls }





