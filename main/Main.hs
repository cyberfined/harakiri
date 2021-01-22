{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Harakiri

code :: Text.Text
code = Text.unlines [ "def fib(n) {"
                    , "    if n <= 2 {"
                    , "        return 1"
                    , "    }"
                    , "    return fib(n-1) + fib(n-2)"
                    , "}"
                    ]

main :: IO ()
main = case parseFromText "main.hk" code of
    Left err    -> putStrLn err
    Right funcs -> TIO.putStr $ Text.unlines $ map (showFunction . fmap stripAnnotation) funcs
