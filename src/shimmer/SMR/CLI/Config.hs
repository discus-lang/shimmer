
module SMR.CLI.Config where
import qualified System.Exit    as System


-- | Command line mode.
data Mode
        -- No mode specified.
        = ModeNone

        -- Parse and check a .smr source file.
        | ModeCheck FilePath

        -- Start the REPL with the given file.
        | ModeREPL  (Maybe FilePath)

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
 | "-check" : filePath : ssRest <- ss
 = parseArgs ssRest
 $ config { configMode = ModeCheck filePath }

 | "-convert" : fileSource : fileDest : ssRest <- ss
 = parseArgs ssRest
 $ config { configMode = ModeConvert fileSource fileDest }

 | "-help"  : _ssRest <- ss
 = do   putStr usage
        System.exitSuccess

 | "--help"  : _ssRest <- ss
 = do   putStr usage
        System.exitSuccess


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
 [ "shimmer                       Start the REPL with no soure file."
 , "shimmer FILE                  Start the REPL with the given file."
 , "shimmer -help                 Display this help page."
 , "shimmer -check FILE           Check that a source file is well formed."
 , "shimmer -convert FILE1 FILE2  Convert file from one format to another." ]
