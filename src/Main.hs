
module Main where
import qualified SMR.Core.World                 as World
import qualified SMR.Prim.Op                    as Prim
import qualified SMR.Prim.Name                  as Prim
import qualified SMR.CLI.Config                 as Config
import qualified SMR.CLI.Repl                   as Repl
import qualified SMR.Source.Parser              as Source
import qualified SMR.Source.Lexer               as Source
import qualified SMR.Source.Pretty              as Source
import qualified SMR.Codec.Size                 as Codec
import qualified System.Environment             as System
import qualified Data.Text.Lazy.IO              as TL
import qualified Data.Text.Lazy.Builder         as BL
import qualified Data.Maybe                     as Maybe


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

        let (ts, _loc, _csRest)
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
 = do   world <- World.worldInit ()
        Repl.replStart
         $ Repl.State
         { Repl.stateMode   = Repl.ModeNone
         , Repl.stateDecls  = []
         , Repl.stateFiles  = Maybe.maybeToList mPath
         , Repl.stateWorld  = world }


