module Common
    ( ShowFunctions(..)
    , mkTestLabel
    ) where

import Test.HUnit
import Data.Text (unpack, unlines)
import Prelude hiding (unlines)

import Harakiri.Expr

newtype ShowFunctions = ShowFunctions
    { runShowFunctions :: [Function Expr]
    } deriving Eq

instance Show ShowFunctions where
    show = ('\n':) . unpack . unlines . map showFunction . runShowFunctions

mkTestLabel :: String -> [Assertion] -> Test
mkTestLabel lbl = TestLabel lbl . TestList . map TestCase
