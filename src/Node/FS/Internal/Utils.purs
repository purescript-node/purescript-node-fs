module Node.FS.Internal.Utils where

import Prelude

import Data.DateTime (DateTime)
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Int (round)
import Data.Time.Duration (Milliseconds(..))

datetimeToUnixEpochTimeInSeconds :: DateTime -> Int
datetimeToUnixEpochTimeInSeconds date = ms (toEpochMilliseconds date) / 1000
  where
  ms (Milliseconds n) = round n
  toEpochMilliseconds = unInstant <<< fromDateTime
