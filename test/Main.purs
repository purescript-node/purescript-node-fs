module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Node.Buffer (BUFFER)
import Test as Test
import TestAsync as TestAsync
import Test.Streams as Streams

main::forall e. Eff (fs :: FS , console :: CONSOLE ,
                     exception :: EXCEPTION , buffer :: BUFFER | e) Unit
main = do
  Test.main
  TestAsync.main
  Streams.main
