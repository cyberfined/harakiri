module Main (main) where

import Test.HUnit
import qualified ParserTests
import qualified TypeCheckTests

main :: IO ()
main = runTestTTAndExit $ TestList
    [ ParserTests.tests
    , TypeCheckTests.tests
    ]
