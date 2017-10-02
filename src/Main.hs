
module Main where
import SMR.Core.Exp.Base
import qualified SMR.Source.Parser      as Source
import qualified SMR.Source.Lexer       as Source
import qualified SMR.CLI.Config         as Config
import qualified System.Environment     as System
import qualified System.Exit            as System

main :: IO ()
main
 = do   args    <- System.getArgs
        config  <- Config.parseArgs args Config.configZero

        case Config.configMode config of
         Config.ModeNone
          -> do putStrLn $ Config.usage
                System.exitSuccess

         Config.ModeCheck file
          -> runCheck file



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


