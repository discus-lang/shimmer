
module SMR.CLI.Driver.Load
        (runLoadFileDecls)
where
import qualified SMR.Prim.Op                    as Prim
import qualified SMR.Prim.Name                  as Prim
import qualified SMR.Source.Parser              as Source
import qualified SMR.Source.Lexer               as Source
import qualified SMR.Core.Codec                 as Codec
import SMR.Core.Exp                             (Decl)
import SMR.Prim.Op.Base                         (Prim)
import qualified Data.ByteString                as BS

import qualified Foreign.Marshal.Alloc          as Foreign

import qualified System.FilePath                as System
import qualified System.IO                      as System
import Control.Monad
import Data.Text                                (Text)


-- | Load decls from the given file.
runLoadFileDecls :: FilePath -> IO [Decl Text Prim]
runLoadFileDecls path
 -- Shimmer text source file.
 | System.takeExtension path == ".smr"
 = do   str     <- readFile path

        let (ts, _loc, _csRest)
                = Source.lexTokens (Source.L 1 1) str

        let config
                = Source.Config
                { Source.configReadSym  = Just
                , Source.configReadPrm  = Prim.readPrim Prim.primNames }

        case Source.parseDecls config ts of
         Left err       -> error $ show err
         Right decls    -> return decls


 -- Somee other file.
 | otherwise
 = do
        bs      <- BS.readFile path

        let magicSMR    = BS.pack [0x53, 0x4d, 0x52, 0x31]
        when (not $ BS.isPrefixOf magicSMR (BS.take 4 bs))
         $ error "runLoadFileDecls: cannot load this file"

        return $ Codec.unpackFileDecls bs

