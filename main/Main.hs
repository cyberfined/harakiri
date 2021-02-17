{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Text (Text)
import System.IO (stderr, openFile, hClose, IOMode(..))
import System.Process (callProcess)
import System.Exit

import Harakiri.Expr hiding (showFunction, interpret)
import Harakiri.Compiler
import Harakiri.IR hiding (showFunction)
import Harakiri.Parser
import Harakiri.SourceCode
import Harakiri.TypeCheck

import qualified Data.Text.IO as TIO

import qualified Harakiri.Expr as Expr
import qualified Harakiri.IR as IR

import OptionsParser

printVersion :: IO ()
printVersion = do
    putStrLn "harakiri version 0.1.0.0"
    exitWith (ExitFailure 2)

rightOrPrintError :: Either Text a -> IO a
rightOrPrintError e = case e of
    Left err -> do
        TIO.hPutStrLn stderr err
        exitWith (ExitFailure 2)
    Right val -> return val

dumpToFile :: FilePath -> (a -> Text) -> [a] -> IO ()
dumpToFile fp showItem items = bracket (openFile fp WriteMode) hClose $ \hdl ->
    mapM_ (TIO.hPutStrLn hdl . showItem) items

runAs :: Options -> FilePath -> IO ()
runAs opts input = callProcess (asPath opts) asArgs
  where asArgs = ["-c", input, "-o", objectFilePath opts]

runLd :: Options -> IO ()
runLd opts = callProcess (ldPath opts) ldArgs
  where ldArgs = [objectFilePath opts, stdlibPath opts, "-o", outputFile opts]

main :: IO ()
main = do
    opts <- parseOptions
    when (showVersion opts) printVersion

    sourceCode <- SourceCode <$> TIO.readFile (inputFile opts)
    annFuncs <- rightOrPrintError (parseFromText (inputFile opts) sourceCode)
    typedFuncs <- rightOrPrintError (typeCheck sourceCode annFuncs)
    let strippedFuncs = map (fmap stripAnnotation) $ getTypedFunctions typedFuncs
    when (dumpAST opts) $
        dumpToFile (astDumpPath opts) Expr.showFunction strippedFuncs

    transRes <- rightOrPrintError (translateToIR typedFuncs)
    when (dumpIR opts) $
        dumpToFile (irDumpPath opts) IR.showFunction (IR.functions transRes)

    let (allocRegs, compileAllocRes) = case arch opts of
            ArchAarch64 a -> (allocateRegisters a, compileAllocateResult a)
        assemblyFile = assemblyDumpPath opts
    allocRes <- rightOrPrintError (allocRegs transRes)
    TIO.writeFile assemblyFile (compileAllocRes allocRes)

    runAs opts assemblyFile
    runLd opts
