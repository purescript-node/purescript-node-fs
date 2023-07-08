module Test.Streams where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.EventEmitter (on_)
import Node.FS.Stream (createWriteStream, createReadStream)
import Node.FS.Sync as Sync
import Node.Path as Path
import Node.Stream as Stream

main :: Effect Unit
main = do
  let fp = Path.concat

  log "Testing streams"

  r <- createReadStream (fp [ "test", "Streams.purs" ])
  w <- createWriteStream (fp [ "tmp", "Streams.purs" ])

  _ <- Stream.pipe r w

  r # on_ Stream.endH do
    src <- Sync.readTextFile UTF8 (fp [ "test", "Streams.purs" ])
    dst <- Sync.readTextFile UTF8 (fp [ "tmp", "Streams.purs" ])

    if src == dst then log "all good"
    else log "not good"
