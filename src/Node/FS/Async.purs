module Node.FS.Async
  ( Callback (..)
  , rename
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
  , readdir
  , utimes
  , readFile
  , readTextFile
  , writeFile
  , writeTextFile
  , appendFile
  , appendTextFile
  , exists
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)
import Control.Monad.Eff.Exception
import Data.Date
import Data.Time
import Data.Either
import Data.Function
import Data.Maybe
import Data.Maybe.Unsafe(fromJust)
import Node.Buffer (Buffer(..))
import Node.Encoding
import Node.FS
import Node.FS.Stats
import Node.Path (FilePath())
import Node.FS.Perms

foreign import data Nullable :: * -> *

type JSCallback a = Fn2 (Nullable Error) a Unit

foreign import handleCallbackImpl
  "function handleCallbackImpl(left, right, f) {\
  \  return function(err, value) {\
  \    if (err) f(left(err))();\
  \    else f(right(value))();\
  \  };\
  \}" :: forall eff a. Fn3 (Error -> Either Error a)
                           (a -> Either Error a)
                           (Callback eff a)
                           (JSCallback a)

handleCallback :: forall eff a b. (Callback eff a) -> JSCallback a
handleCallback cb = runFn3 handleCallbackImpl Left Right cb

foreign import fs "var fs = require('fs');" ::
  { rename :: Fn3 FilePath FilePath (JSCallback Unit) Unit
  , truncate :: Fn3 FilePath Number (JSCallback Unit) Unit
  , chown :: Fn4 FilePath Number Number (JSCallback Unit) Unit
  , chmod :: Fn3 FilePath String (JSCallback Unit) Unit
  , stat :: Fn2 FilePath (JSCallback StatsObj) Unit
  , link :: Fn3 FilePath FilePath (JSCallback Unit) Unit
  , symlink :: Fn4 FilePath FilePath String (JSCallback Unit) Unit
  , readlink :: Fn2 FilePath (JSCallback FilePath) Unit
  , realpath :: forall cache. Fn3 FilePath { | cache } (JSCallback FilePath) Unit
  , unlink :: Fn2 FilePath (JSCallback Unit) Unit
  , rmdir :: Fn2 FilePath (JSCallback Unit) Unit
  , mkdir :: Fn3 FilePath String (JSCallback Unit) Unit
  , readdir :: Fn2 FilePath (JSCallback [FilePath]) Unit
  , utimes :: Fn4 FilePath Number Number (JSCallback Unit) Unit
  , readFile :: forall a opts. Fn3 FilePath { | opts } (JSCallback a) Unit
  , writeFile :: forall a opts. Fn4 FilePath a { | opts } (JSCallback Unit) Unit
  , appendFile :: forall a opts. Fn4 FilePath a { | opts } (JSCallback Unit) Unit
  , exists :: forall a. Fn2 FilePath (Boolean -> a) Unit
  }

foreign import mkEff
  "function mkEff(action) {\
  \  return action;\
  \}" :: forall eff a. (Unit -> a) -> Eff eff a

-- |
-- Type synonym for callback functions.
--
type Callback eff a = Either Error a -> Eff (fs :: FS | eff) Unit

-- |
-- Renames a file.
--
rename :: forall eff. FilePath
                   -> FilePath
                   -> Callback eff Unit
                   -> Eff (fs :: FS | eff) Unit

rename oldFile newFile cb = mkEff $ \_ -> runFn3
  fs.rename oldFile newFile (handleCallback cb)

-- |
-- Truncates a file to the specified length.
--
truncate :: forall eff. FilePath
                     -> Number
                     -> Callback eff Unit
                     -> Eff (fs :: FS | eff) Unit

truncate file len cb = mkEff $ \_ -> runFn3
  fs.truncate file len (handleCallback cb)

-- |
-- Changes the ownership of a file.
--
chown :: forall eff. FilePath
                  -> Number
                  -> Number
                  -> Callback eff Unit
                  -> Eff (fs :: FS | eff) Unit

chown file uid gid cb = mkEff $ \_ -> runFn4
  fs.chown file uid gid (handleCallback cb)

-- |
-- Changes the permissions of a file.
--
chmod :: forall eff. FilePath
                  -> Perms
                  -> Callback eff Unit
                  -> Eff (fs :: FS | eff) Unit

chmod file perms cb = mkEff $ \_ -> runFn3
  fs.chmod file (permsToString perms) (handleCallback cb)

-- |
-- Gets file statistics.
--
stat :: forall eff. FilePath
                 -> Callback eff Stats
                 -> Eff (fs :: FS | eff) Unit

stat file cb = mkEff $ \_ -> runFn2
  fs.stat file (handleCallback $ cb <<< (<$>) Stats)

-- |
-- Creates a link to an existing file.
--
link :: forall eff. FilePath
                 -> FilePath
                 -> Callback eff Unit
                 -> Eff (fs :: FS | eff) Unit

link src dst cb = mkEff $ \_ -> runFn3
  fs.link src dst (handleCallback cb)

-- |
-- Creates a symlink.
--
symlink :: forall eff. FilePath
                    -> FilePath
                    -> SymlinkType
                    -> Callback eff Unit
                    -> Eff (fs :: FS | eff) Unit

symlink src dest ty cb = mkEff $ \_ -> runFn4
  fs.symlink src dest (show ty) (handleCallback cb)

-- |
-- Reads the value of a symlink.
--
readlink :: forall eff. FilePath
                     -> Callback eff FilePath
                     -> Eff (fs :: FS | eff) Unit

readlink path cb = mkEff $ \_ -> runFn2
  fs.readlink path (handleCallback cb)

-- |
-- Find the canonicalized absolute location for a path.
--
realpath :: forall eff. FilePath
                     -> Callback eff FilePath
                     -> Eff (fs :: FS | eff) Unit

realpath path cb = mkEff $ \_ -> runFn3
  fs.realpath path {} (handleCallback cb)

-- |
-- Find the canonicalized absolute location for a path using a cache object for
-- already resolved paths.
--
realpath' :: forall eff cache. FilePath
                            -> { | cache }
                            -> Callback eff FilePath
                            -> Eff (fs :: FS | eff) Unit

realpath' path cache cb = mkEff $ \_ -> runFn3
  fs.realpath path cache (handleCallback cb)

-- |
-- Deletes a file.
--
unlink :: forall eff. FilePath
                   -> Callback eff Unit
                   -> Eff (fs :: FS | eff) Unit

unlink file cb = mkEff $ \_ -> runFn2
  fs.unlink file (handleCallback cb)

-- |
-- Deletes a directory.
--
rmdir :: forall eff. FilePath
                   -> Callback eff Unit
                   -> Eff (fs :: FS | eff) Unit

rmdir file cb = mkEff $ \_ -> runFn2
  fs.rmdir file (handleCallback cb)

-- |
-- Makes a new directory.
--
mkdir :: forall eff. FilePath
                  -> Callback eff Unit
                  -> Eff (fs :: FS | eff) Unit

mkdir = flip mkdir' $ mkPerms (r <> w <> x) (r <> w <> x) (r <> w <> x)

-- |
-- Makes a new directory with the specified permissions.
--
mkdir' :: forall eff. FilePath
                   -> Perms
                   -> Callback eff Unit
                   -> Eff (fs :: FS | eff) Unit

mkdir' file perms cb = mkEff $ \_ -> runFn3
  fs.mkdir file (permsToString perms) (handleCallback cb)

-- |
-- Reads the contents of a directory.
--
readdir :: forall eff. FilePath
                    -> Callback eff [FilePath]
                    -> Eff (fs :: FS | eff) Unit

readdir file cb = mkEff $ \_ -> runFn2
  fs.readdir file (handleCallback cb)

-- |
-- Sets the accessed and modified times for the specified file.
--
utimes :: forall eff. FilePath
                   -> Date
                   -> Date
                   -> Callback eff Unit
                   -> Eff (fs :: FS | eff) Unit

utimes file atime mtime cb = mkEff $ \_ -> runFn4
  fs.utimes file
            (ms (toEpochMilliseconds atime) / 1000)
            (ms (toEpochMilliseconds mtime) / 1000)
            (handleCallback cb)
  where
  ms (Milliseconds n) = n

-- |
-- Reads the entire contents of a file returning the result as a raw buffer.
--
readFile :: forall eff. FilePath
                     -> Callback eff Buffer
                     -> Eff (fs :: FS | eff) Unit

readFile file cb = mkEff $ \_ -> runFn3
  fs.readFile file {} (handleCallback cb)

-- |
-- Reads the entire contents of a text file with the specified encoding.
--
readTextFile :: forall eff. Encoding
                         -> FilePath
                         -> Callback eff String
                         -> Eff (fs :: FS | eff) Unit

readTextFile encoding file cb = mkEff $ \_ -> runFn3
  fs.readFile file { encoding: show encoding } (handleCallback cb)

-- |
-- Writes a buffer to a file.
--
writeFile :: forall eff. FilePath
                      -> Buffer
                      -> Callback eff Unit
                      -> Eff (fs :: FS | eff) Unit

writeFile file buff cb = mkEff $ \_ -> runFn4
  fs.writeFile file buff {} (handleCallback cb)

-- |
-- Writes text to a file using the specified encoding.
--
writeTextFile :: forall eff. Encoding
                          -> FilePath
                          -> String
                          -> Callback eff Unit
                          -> Eff (fs :: FS | eff) Unit

writeTextFile encoding file buff cb = mkEff $ \_ -> runFn4
  fs.writeFile file buff { encoding: show encoding } (handleCallback cb)

-- |
-- Appends the contents of a buffer to a file.
--
appendFile :: forall eff. FilePath
                       -> Buffer
                       -> Callback eff Unit
                       -> Eff (fs :: FS | eff) Unit

appendFile file buff cb = mkEff $ \_ -> runFn4
  fs.appendFile file buff {} (handleCallback cb)

-- |
-- Appends text to a file using the specified encoding.
--
appendTextFile :: forall eff. Encoding
                           -> FilePath
                           -> String
                           -> Callback eff Unit
                           -> Eff (fs :: FS | eff) Unit

appendTextFile encoding file buff cb = mkEff $ \_ -> runFn4
  fs.appendFile file buff { encoding: show encoding } (handleCallback cb)

-- |
-- Check if the path exists.
--
exists :: forall eff. FilePath
                   -> (Boolean -> Eff eff Unit)
                   -> Eff (fs :: FS | eff) Unit
exists file cb = mkEff $ \_ -> runFn2
  fs.exists file $ \b -> runPure (unsafeInterleaveEff (cb b))
