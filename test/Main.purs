module Test.Main where

import Prelude
import qualified Test as Test
import qualified TestAsync as TestAsync

main = do
  Test.main
  TestAsync.main
