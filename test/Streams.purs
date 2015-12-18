module Test.Streams where

import Prelude
import Data.Maybe
import Data.Either
import Control.Apply ((*>))
import Control.Bind ((=<<))
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Console (log)
import Node.Encoding
import Node.Buffer as Buffer
import Node.Path as Path
import Node.Stream as Stream
import Unsafe.Coerce

import Node.FS
import Node.FS.Stats
import Node.FS.Stream
import Node.FS.Sync as Sync

main = do
  let fp = Path.concat

  log "Testing streams"

  r <- createReadStream (fp ["test", "Streams.purs"])
  w <- createWriteStream (fp ["tmp", "Streams.purs"])

  Stream.pipe r w

  Stream.onEnd r do
    src <- Sync.readTextFile UTF8 (fp ["test", "Streams.purs"])
    dst <- Sync.readTextFile UTF8 (fp ["tmp", "Streams.purs"])

    if src == dst
      then log "all good"
      else log "not good"
