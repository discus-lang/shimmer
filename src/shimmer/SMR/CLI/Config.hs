
module SMR.CLI.Config where
import qualified System.Exit    as System


-- | Command line mode.
data Mode
        -- No mode specified.
        = ModeNone

        -- Start the REPL with the given file.
        | ModeREPL  (Maybe FilePath)

        -- Load a file and print the human readable version to stdout.
        | ModeLoad FilePath

        -- Convert a file from one format to another.
        | ModeConvert FilePath FilePath
        deriving Show


-- | Command line config.
data Config
        = Config
        { configMode    :: Mode }
        deriving Show


configZero :: Config
configZero
        = Config
        { configMode    = ModeNone }


-- | Parse command-line arguments.
parseArgs :: [String] -> Config -> IO Config
parseArgs [] config
 = return config

parseArgs ss config
 | "-help"  : _ssRest <- ss
 = do   putStr usage
        System.exitSuccess

 | "--help"  : _ssRest <- ss
 = do   putStr usage
        System.exitSuccess

 | "-load" : filePath : ssRest <- ss
 = parseArgs ssRest
 $ config { configMode = ModeLoad filePath }

 | "-convert" : fileSource : fileDest : ssRest <- ss
 = parseArgs ssRest
 $ config { configMode = ModeConvert fileSource fileDest }

 | filePath : ssRest <- ss
 , c : _       <- filePath
 , c /= '-'
 = parseArgs ssRest
 $ config { configMode  = ModeREPL (Just filePath) }

 | otherwise
 = do   putStr usage
        System.exitSuccess

usage :: String
usage
 = unlines
 [ "Shimmer, the reflective lambda machine."
 , ""
 , "  shimmer                       Start the REPL with no source file."
 , "  shimmer FILE                  Start the REPL with the given source file."
 , "  shimmer -help                 Display this help page."
 , "  shimmer -load FILE            Load a file and print it to stdout."
 , "  shimmer -convert FILE1 FILE2  Convert file from one format to another."
 , ""]
