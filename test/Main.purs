module Test.Main where

import Prelude

import Effect (Effect)
import Test as Test
import Test.Streams as Streams
import TestAff as TestAff
import TestAsync as TestAsync

main :: Effect Unit
main = do
  Test.main
  TestAsync.main
  Streams.main
  TestAff.main
