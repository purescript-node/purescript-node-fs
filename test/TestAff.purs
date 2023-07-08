module Test.Main where

import Prelude

import Data.Array (filterA)
import Data.Maybe (maybe)
import Data.String.CodeUnits (charAt, singleton)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.FS.Aff (stat, readdir)
import Node.FS.Stats (isDirectory)

main :: Effect Unit
main = launchAff_ do
  files <- readdir "."
  files' <- flip filterA files \file -> do
    stat <- stat file
    pure $ isDirectory stat
      && (maybe false (singleton >>> (_ /= ".")) $ charAt 0 file)
  liftEffect $ log $ show files'
