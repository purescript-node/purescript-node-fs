module Test.Streams where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.Encoding (Encoding(..))
import Node.Path as Path
import Node.Stream as Stream

import Node.FS (FS)
import Node.FS.Stream (createWriteStream, createReadStream)
import Node.FS.Sync as Sync

main::forall e. Eff (fs::FS, console::CONSOLE, exception::EXCEPTION | e) Unit
main = do
  let fp = Path.concat

  _ <- log "Testing streams"

  r <- createReadStream (fp ["test", "Streams.purs"])
  w <- createWriteStream (fp ["tmp", "Streams.purs"])

  _ <- Stream.pipe r w

  Stream.onEnd r do
    src <- Sync.readTextFile UTF8 (fp ["test", "Streams.purs"])
    dst <- Sync.readTextFile UTF8 (fp ["tmp", "Streams.purs"])

    if src == dst
      then log "all good"
      else log "not good"
