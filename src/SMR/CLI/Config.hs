
module SMR.CLI.Config where
import qualified System.Exit    as System


-- | Command line mode.
data Mode
        -- No mode specified.
        = ModeNone

        -- Parse and check a .smr source file.
        | ModeCheck FilePath

        -- Start the REPL with the given file.
        | ModeREPL  FilePath
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
 $ config { configMode  = ModeCheck filePath }

 | "-repl"  : filePath : ssRest <- ss
 = parseArgs ssRest
 $ config { configMode  = ModeREPL  filePath }

 | otherwise
 = do   putStr usage
        System.exitSuccess



usage :: String
usage
 = unlines
 [ "shimmer -check FILE.smr      Check that a source file is well formed."
 , "shimmer -repl  FILE.smr      Start the REPL with the given file." ]
