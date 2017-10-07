
module Main where
import SMR.Core.Exp.Base
import SMR.Core.Exp.Train
import SMR.Core.Exp.Push
import qualified SMR.Prim.Op                    as Prim
import qualified SMR.Prim.Op.Base               as Prim
import qualified SMR.Prim.Name                  as Prim
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
import qualified Data.Set                       as Set


main :: IO ()
main
 = do   args    <- System.getArgs
        config  <- Config.parseArgs args Config.configZero

        case Config.configMode config of
         Config.ModeNone
          -> runRepl Nothing

         Config.ModeCheck file
          -> runCheck file

         Config.ModeREPL mFile
          -> runRepl mFile



runCheck :: FilePath -> IO ()
runCheck path
 = do   str     <- readFile path

        let (ts, loc, csRest)
                = Source.lexTokens (Source.L 1 1) str

        let config
                = Source.Config
                { Source.configReadSym  = Just
                , Source.configReadPrm  = Prim.readPrim Prim.primOpTextNames }

        case Source.parseDecls config ts of
         Left err
          -> error $ show err

         Right decls
          -> TL.putStr
                $ BL.toLazyText
                $ mconcat
                $ map Source.buildDecl decls


runRepl :: Maybe FilePath -> IO ()
runRepl mPath
 = do
        let config
                = Source.Config
                { Source.configReadSym  = Just
                , Source.configReadPrm  = Prim.readPrim Prim.primOpTextNames }

        decls
         <- case mPath of
                Nothing -> return []
                Just path
                 -> do  str     <- readFile path

                        let (ts, loc, csRest)
                                = Source.lexTokens (Source.L 1 1) str

                        case Source.parseDecls config ts of
                         Left err     -> error $ show err
                         Right decls' -> return decls'

        Repl.replStart
         $ Repl.State
         { Repl.stateMode   = Repl.ModeNone
         , Repl.stateDecls  = decls }


