module Node.FS.Sync
  ( rename
  , truncate
  , chown
  , chmod
  , stat
  , link
  , symlink
  , readlink
  , realpath
  , realpath'
  , unlink
  , rmdir
  , mkdir
  , mkdir'
  , readFile
  , readTextFile
  , writeFile
  , writeTextFile
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
  , linkSync :: Fn2 FilePath FilePath Unit
  , symlinkSync :: Fn3 FilePath FilePath String Unit
  , readlinkSync :: Fn1 FilePath FilePath
  , realpathSync :: forall cache. Fn2 FilePath { | cache } FilePath
  , unlinkSync :: Fn1 FilePath Unit
  , rmdirSync :: Fn1 FilePath Unit
  , mkdirSync :: Fn2 FilePath Number Unit
  , readFileSync :: forall a opts. Fn2 FilePath { | opts } a
  , writeFileSync :: forall a opts. Fn3 FilePath a { | opts } Unit
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
-- Creates a link to an existing file.
--
link :: forall eff. FilePath
                 -> FilePath
                 -> Eff (fs :: FS, err :: Exception Error | eff) Unit

link src dst = mkEff $ \_ -> runFn2
  fs.linkSync src dst

-- |
-- Creates a symlink.
--
symlink :: forall eff. FilePath
                    -> FilePath
                    -> SymlinkType
                    -> Eff (fs :: FS, err :: Exception Error | eff) Unit

symlink src dst ty = mkEff $ \_ -> runFn3
  fs.symlinkSync src dst (show ty)

-- |
-- Reads the value of a symlink.
--
readlink :: forall eff. FilePath
                     -> Eff (fs :: FS, err :: Exception Error | eff) FilePath

readlink path = mkEff $ \_ -> runFn1
  fs.readlinkSync path

-- |
-- Find the canonicalized absolute location for a path.
--
realpath :: forall eff. FilePath
                     -> Eff (fs :: FS, err :: Exception Error | eff) FilePath

realpath path = mkEff $ \_ -> runFn2
  fs.realpathSync path {}

-- |
-- Find the canonicalized absolute location for a path using a cache object for
-- already resolved paths.
--
realpath' :: forall eff cache. FilePath
                            -> { | cache }
                            -> Eff (fs :: FS, err :: Exception Error | eff) FilePath

realpath' path cache = mkEff $ \_ -> runFn2
  fs.realpathSync path cache

-- |
-- Deletes a file.
--
unlink :: forall eff. FilePath
                   -> Eff (fs :: FS, err :: Exception Error | eff) Unit

unlink file = mkEff $ \_ -> runFn1
  fs.unlinkSync file

-- |
-- Deletes a directory.
--
rmdir :: forall eff. FilePath
                  -> Eff (fs :: FS, err :: Exception Error | eff) Unit

rmdir file = mkEff $ \_ -> runFn1
  fs.rmdirSync file

-- |
-- Makes a new directory.
--
mkdir :: forall eff. FilePath
                  -> Eff (fs :: FS, err :: Exception Error | eff) Unit

mkdir = flip mkdir' 777

-- |
-- Makes a new directory with the specified permissions.
--
mkdir' :: forall eff. FilePath
                   -> Number
                   -> Eff (fs :: FS, err :: Exception Error | eff) Unit

mkdir' file mode = mkEff $ \_ -> runFn2
  fs.mkdirSync file mode

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

-- |
-- Writes a buffer to a file.
--
writeFile :: forall eff. FilePath
                      -> Buffer
                      -> Eff (fs :: FS, err :: Exception Error | eff) Unit

writeFile file buff = mkEff $ \_ -> runFn3
  fs.writeFileSync file buff {}

-- |
-- Writes text to a file using the specified encoding.
--
writeTextFile :: forall eff. Encoding
                          -> FilePath
                          -> String
                          -> Eff (fs :: FS, err :: Exception Error | eff) Unit

writeTextFile encoding file buff = mkEff $ \_ -> runFn3
  fs.writeFileSync file buff { encoding: show encoding }
