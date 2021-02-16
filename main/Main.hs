{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Harakiri.Expr hiding (showFunction, interpret)
import Harakiri.Compiler
import Harakiri.Compiler.Architecture.Aarch64
import Harakiri.IR hiding (showFunction)
import Harakiri.Parser
import Harakiri.SourceCode
import Harakiri.TypeCheck

import qualified Data.Text.IO as TIO

import qualified Harakiri.Expr as Expr
import qualified Harakiri.IR as IR

main :: IO ()
main = do
    sourceCode <- SourceCode <$> TIO.readFile src
    case parseFromText src sourceCode of
        Left parseErr -> TIO.putStrLn parseErr
        Right annFuncs -> case typeCheck sourceCode annFuncs of
            Left typeError -> TIO.putStrLn typeError
            Right typedFuncs -> do
                let funcs = map (fmap stripAnnotation) $ getTypedFunctions typedFuncs
                TIO.putStrLn "Successful type checking:"
                mapM_ (TIO.putStrLn . Expr.showFunction) funcs
                case translateToIR typedFuncs of
                    Left err -> TIO.putStrLn err
                    Right transRes -> do
                        TIO.putStrLn "\nSuccessful translation to IR:"
                        mapM_ (TIO.putStrLn . IR.showFunction) (IR.functions transRes)
                        case allocateRegisters arch transRes of
                            Right allocRes -> do
                                TIO.putStrLn "\nSuccessful register allocation:"
                                TIO.putStrLn (compileAllocateResult arch allocRes)
                            Left err -> TIO.putStrLn err
  where src = "test.hk"
        arch = Aarch64
