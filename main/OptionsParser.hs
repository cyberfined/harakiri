module OptionsParser
    ( Architecture(..)
    , OS(..)
    , Options(..)
    , astDumpPath
    , irDumpPath
    , assemblyDumpPath
    , objectFilePath
    , parseOptions
    ) where

import Control.Monad (when)
import Data.Maybe (fromJust)
import Data.List (find)
import Options.Applicative
import Text.ParserCombinators.ReadP (choice, string, readP_to_S)
import System.Exit
import System.FilePath

import Harakiri.Compiler.Architecture.Aarch64

import qualified System.Info as Info

data Architecture = ArchAarch64 Aarch64 deriving Eq

archStrings :: [(Architecture, String)]
archStrings = [(ArchAarch64 Aarch64, "aarch64")]

instance Show Architecture where
    show = showByList archStrings

instance Read Architecture where
    readsPrec = readByList archStrings

defaultArch :: Architecture
defaultArch = case find ((==Info.arch) . snd) archStrings of
    Nothing    -> fst $ head archStrings
    Just (a,_) -> a

data OS = Linux deriving Eq

osStrings :: [(OS, String)]
osStrings = [(Linux, "linux")]

instance Show OS where
    show = showByList osStrings

instance Read OS where
    readsPrec = readByList osStrings

defaultOS :: OS
defaultOS = case find ((==Info.os) . snd) osStrings of
    Nothing    -> fst $ head osStrings
    Just (o,_) -> o

data Options = Options
    { showVersion  :: !Bool
    , dumpAST      :: !Bool
    , dumpIR       :: !Bool
    , arch         :: !Architecture
    , os           :: !OS
    , inputFile    :: !FilePath
    , outputFile   :: !FilePath
    , stdlibPath   :: !FilePath
    , asPath       :: !FilePath
    , ldPath       :: !FilePath
    } deriving Show

astDumpPath :: Options -> FilePath
astDumpPath opts = dropExtension (inputFile opts) ++ "-ast"

irDumpPath :: Options -> FilePath
irDumpPath opts = dropExtension (inputFile opts) ++ "-ir"

assemblyDumpPath :: Options -> FilePath
assemblyDumpPath opts = replaceExtension (inputFile opts) "s"

objectFilePath :: Options -> FilePath
objectFilePath opts = replaceExtension (inputFile opts) "o"

parseOptions :: IO Options
parseOptions = do
    opts <- execParser (info optionsParser fullDesc)
    when (not (showVersion opts) && null (inputFile opts)) $ do
        putStrLn "error: no input file"
        exitWith (ExitFailure 2)
    when (not (null $ inputFile opts) && takeExtension (inputFile opts) /= ".hk") $ do
        putStrLn "error: input file's extension must be .hk"
        exitWith (ExitFailure 2)
    return opts

optionsParser :: Parser Options
optionsParser = setDefaultOptions <$> (options <**> helper)

options :: Parser Options
options = Options
       <$> switch
           ( long "version"
          <> short 'v'
          <> help "Display compiler version information"
           )
       <*> switch
           ( long "dump-ast"
          <> short 't'
          <> help "Dump AST representation"
           )
       <*> switch
           ( long "dump-ir"
          <> short 'i'
          <> help "Dump intermediate representation"
           )
       <*> option auto
           ( long "arch"
          <> short 'a'
          <> metavar "ARCH"
          <> value defaultArch
          <> showDefault
          <> help "Set target architecture"
           )
       <*> option auto
           ( long "platform"
          <> short 'p'
          <> metavar "OS"
          <> value defaultOS
          <> showDefault
          <> help "Set target operating system"
           )
       <*> strOption
           ( short 'c'
          <> metavar "PATH"
          <> value ""
          <> noArgError (ErrorMsg "No input file")
          <> help "Set source file"
           )
       <*> strOption
           ( short 'o'
          <> metavar "PATH"
          <> value ""
          <> help "Set output file"
           )
       <*> strOption
           ( short 'l'
          <> metavar "PATH"
          <> value ""
          <> help "Set standard library path"
           )
       <*> strOption
           ( long "as-path"
          <> metavar "PATH"
          <> value ""
          <> help "Set path to the assembler"
           )
       <*> strOption
           ( long "ld-path"
          <> metavar "PATH"
          <> value ""
          <> help "Set path to the linker"
           )

setDefaultOptions :: Options -> Options
setDefaultOptions opts = opts { outputFile = setIfEmpty (outputFile opts) output
                              , stdlibPath = setIfEmpty (stdlibPath opts) stdLib
                              , asPath = setIfEmpty (asPath opts) as
                              , ldPath = setIfEmpty (ldPath opts) ld
                              }
  where (as, ld) = if show (arch opts) == Info.os
                      then ("/usr/bin/as", "/usr/bin/ld")
                      else ("", "")
        stdLib = "lib" </> show (arch opts) </> show (os opts) </> "lib.o"
        output = dropExtension (inputFile opts)
        setIfEmpty def val = if null def then val else def

showByList :: Eq a => [(a, String)] -> a -> String
showByList xs x = fromJust $ lookup x xs

readByList :: Eq a => [(a, String)] -> Int -> ReadS a
readByList xs _ = readP_to_S $ choice $ map (\(x,s) -> string s *> pure x) xs
