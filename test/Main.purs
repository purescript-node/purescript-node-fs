module Test.Main where

import Prelude
import qualified Test as Test
import qualified TestAsync as TestAsync
import Test.Streams as Streams

main = do
  Test.main
  TestAsync.main
  Streams.main
