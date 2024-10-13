module Test.Main where

import Prelude

import Effect (Effect)
import Test.Node.FS.Sync as Sync
import Test.Node.FS.Streams as Streams
import Test.Node.FS.Aff as Aff
import Test.Node.FS.Async as Async
import Test.Node.FS.OpendirAndDir as OpendirAndDir

main :: Effect Unit
main = do
  Sync.main
  Async.main
  Streams.main
  Aff.main
  OpendirAndDir.main
