module Test.Streams where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (message)
import Node.Encoding (Encoding(..))
import Node.Path as Path
import Node.Stream as Stream

import Node.FS.Stream (createWriteStream, createReadStream)
import Node.FS.Sync as Sync

main :: Effect Unit
main = do
  let fp = Path.concat

  _ <- log "Testing streams"

  r <- createReadStream (fp ["test", "Streams.purs"])
  case r of
    (Left e) -> log $ "Got an error creating the stream: " <> message e
    (Right readable) -> do
      w <- createWriteStream (fp ["tmp", "Streams.purs"])
      _ <- Stream.pipe readable w

      Stream.onEnd readable do
        src <- Sync.readTextFile UTF8 (fp ["test", "Streams.purs"])
        dst <- Sync.readTextFile UTF8 (fp ["tmp", "Streams.purs"])

        if src == dst
          then log "all good"
          else log "not good"
