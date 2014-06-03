module Node.FS.Sync
  ( rename
  , truncate
  , chown
  , chmod
  , stat
  , readFile
  , readTextFile
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Data.Function
import Node.Buffer (Buffer(..))
import Node.Encoding
import Node.FS
import Node.FS.Stats
import Node.Path (FilePath())
import Global (Error(..))

foreign import fs "var fs = require('fs');" ::
  { renameSync :: Fn2 FilePath FilePath Unit
  , truncateSync :: Fn2 FilePath Number Unit
  , chownSync :: Fn3 FilePath Number Number Unit
  , chmodSync :: Fn2 FilePath Number Unit
  , statSync :: Fn1 FilePath StatsObj
  , readFileSync :: forall a opts. Fn2 FilePath { | opts } a
  }

foreign import mkEff
  "function mkEff(x) {\
  \  return x;\
  \}" :: forall eff a. (Unit -> a) -> Eff eff a

-- |
-- Renames a file.
--
rename :: forall eff. FilePath
                   -> FilePath
                   -> Eff (fs :: FS, err :: Exception Error | eff) Unit

rename oldFile newFile = mkEff $ \_ -> runFn2
  fs.renameSync oldFile newFile

-- |
-- Truncates a file to the specified length.
--
truncate :: forall eff. FilePath
                     -> Number
                     -> Eff (fs :: FS, err :: Exception Error | eff) Unit

truncate file len = mkEff $ \_ -> runFn2
  fs.truncateSync file len

-- |
-- Changes the ownership of a file.
--
chown :: forall eff. FilePath
                  -> Number
                  -> Number
                  -> Eff (fs :: FS, err :: Exception Error | eff) Unit

chown file uid gid = mkEff $ \_ -> runFn3
  fs.chownSync file uid gid

-- |
-- Changes the permissions of a file.
--
chmod :: forall eff. FilePath
                  -> Number
                  -> Eff (fs :: FS, err :: Exception Error | eff) Unit

chmod file mode = mkEff $ \_ -> runFn2
  fs.chmodSync file mode

-- |
-- Gets file statistics.
--
stat :: forall eff. FilePath
                 -> Eff (fs :: FS, err :: Exception Error | eff) Stats

stat file = mkEff $ \_ -> Stats $ runFn1
  fs.statSync file

-- |
-- Reads the entire contents of a file returning the result as a raw buffer.
--
readFile :: forall eff. FilePath
                     -> Eff (fs :: FS, err :: Exception Error | eff) Buffer

readFile file = mkEff $ \_ -> runFn2
  fs.readFileSync file {}

-- |
-- Reads the entire contents of a text file with the specified encoding.
--
readTextFile :: forall eff. Encoding
                         -> FilePath
                         -> Eff (fs :: FS, err :: Exception Error | eff) String

readTextFile encoding file = mkEff $ \_ -> runFn2
  fs.readFileSync file { encoding: show encoding }
