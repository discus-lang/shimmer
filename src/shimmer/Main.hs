
module Main where
import qualified SMR.CLI.Config                 as Config
import qualified SMR.CLI.Repl                   as Repl
import qualified SMR.CLI.Driver.Load            as Driver
import qualified SMR.Core.World                 as World
import qualified SMR.Source.Pretty              as Source
import qualified SMR.Codec.Size                 as Codec
import qualified SMR.Codec.Poke                 as Codec

import qualified Foreign.Marshal.Alloc          as Foreign

import qualified System.Environment             as System
import qualified System.FilePath                as System
import qualified System.IO                      as System
import qualified Data.Text.Lazy.IO              as TL
import qualified Data.Text.Lazy.Builder         as BL
import qualified Data.Maybe                     as Maybe
import Data.Monoid

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
 = do   decls   <- Driver.runLoadFileDecls path
        TL.putStr
                $ BL.toLazyText
                $ mconcat
                $ map Source.buildDecl decls


-------------------------------------------------------------------------------
runRepl :: Maybe FilePath -> IO ()
runRepl mPath
 = do   world <- World.newWorld ()
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
        decls   <- Driver.runLoadFileDecls pathSrc
        let len = Codec.sizeOfFileDecls decls
        Foreign.allocaBytes len $ \pBuf
         -> do  _ <- Codec.pokeFileDecls decls pBuf
                h <- System.openBinaryFile pathDst System.WriteMode
                System.hPutBuf h pBuf len
                System.hClose h
                return ()

 -- | Decode binary store to text shimmer file.
 | System.takeExtension pathSrc == ".sms"
 , System.takeExtension pathDst == ".smr"
 = do   decls   <- Driver.runLoadFileDecls pathSrc
        TL.writeFile pathDst
                $ BL.toLazyText
                $ mconcat
                $ [ Source.buildDecl d <> BL.fromString "\n" | d <- decls]

 | otherwise
 = error "runConvert: cannot convert"

