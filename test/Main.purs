module Test.Main where

import Prelude
import Effect (Effect)
import Test as Test
import TestAsync as TestAsync
import Test.Streams as Streams

main :: Effect Unit
main = do
  Test.main
  TestAsync.main
  Streams.main
