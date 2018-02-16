
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
import qualified SMR.Codec.Poke                 as Codec
import SMR.Core.Exp                             (Decl)
import SMR.Prim.Op.Base                         (Prim)

import qualified Foreign.Marshal.Alloc          as Foreign

import qualified System.Environment             as System
import qualified System.FilePath                as System
import qualified System.IO                      as System
import qualified Data.Text.Lazy.IO              as TL
import qualified Data.Text.Lazy.Builder         as BL
import qualified Data.Maybe                     as Maybe

import Data.Text                                (Text)


-------------------------------------------------------------------------------
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

         Config.ModeConvert file1 file2
          -> runConvert file1 file2


-------------------------------------------------------------------------------
runCheck :: FilePath -> IO ()
runCheck path
 = do   decls   <- runLoadFile path
        TL.putStr
                $ BL.toLazyText
                $ mconcat
                $ map Source.buildDecl decls


runLoadFile :: FilePath -> IO [Decl Text Prim]
runLoadFile path
 = do   str     <- readFile path

        let (ts, _loc, _csRest)
                = Source.lexTokens (Source.L 1 1) str

        let config
                = Source.Config
                { Source.configReadSym  = Just
                , Source.configReadPrm  = Prim.readPrim Prim.primOpTextNames }

        case Source.parseDecls config ts of
         Left err       -> error $ show err
         Right decls    -> return decls


-------------------------------------------------------------------------------
runRepl :: Maybe FilePath -> IO ()
runRepl mPath
 = do   world <- World.worldInit ()
        Repl.replStart
         $ Repl.State
         { Repl.stateMode   = Repl.ModeNone
         , Repl.stateDecls  = []
         , Repl.stateFiles  = Maybe.maybeToList mPath
         , Repl.stateWorld  = world }


-------------------------------------------------------------------------------
runConvert :: FilePath -> FilePath -> IO ()
runConvert pathSrc pathDst
 -- Encode text shimmer file to binary store.
 | System.takeExtension pathSrc == ".smr"
 , System.takeExtension pathDst == ".sms"
 = do
        decls   <- runLoadFile pathSrc
        let len = Codec.sizeOfFile decls
        Foreign.allocaBytes len  $ \pBuf
         -> do  _ <- Codec.pokeFileDecls decls pBuf
                h <- System.openBinaryFile pathDst System.WriteMode
                System.hPutBuf h pBuf len
                System.hClose h
                return ()

 | otherwise
 = error "runConvert cannot convert"


