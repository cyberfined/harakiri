{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Harakiri.Expr
import Harakiri.Parser
import Harakiri.TypeCheck

import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    sourceCode <- TIO.readFile src
    case parseFromText src sourceCode of
        Left parseErr -> TIO.putStrLn parseErr
        Right funcs -> case typeCheck sourceCode funcs of
            Nothing -> do
                TIO.putStrLn "Successful type checking. Syntax tree is:"
                mapM_ (TIO.putStrLn . showFunction . fmap stripAnnotation) funcs
            Just typeError -> TIO.putStrLn typeError
  where src = "test.hk"
