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
  , readFile
  , readTextFile
  , writeFile
  , writeTextFile
  ) where

import Control.Monad.Eff
import Data.Either
import Data.Foreign
import Data.Function
import Data.Maybe
import Node.Buffer (Buffer(..))
import Node.Encoding
import Node.FS
import Node.FS.Stats
import Node.Path (FilePath())
import Global (Error(..))

type JSCallback a = Fn2 Foreign a Unit

foreign import runCallbackEff
  "function runCallbackEff (f) {\
  \  return f(); \
  \}" :: forall eff a. Eff eff a -> a

handleCallback :: forall eff a b. (Callback eff a) -> JSCallback a
handleCallback f = mkFn2 $ \err x -> runCallbackEff $ f case parseForeign read err of
  Left err -> Left $ "handleCallback failed: " ++ err
  Right (Just err') -> Left $ show (err' :: Error)
  Right Nothing -> Right x

foreign import fs "var fs = require('fs');" ::
  { rename :: Fn3 FilePath FilePath (JSCallback Unit) Unit
  , truncate :: Fn3 FilePath Number (JSCallback Unit) Unit
  , chown :: Fn4 FilePath Number Number (JSCallback Unit) Unit
  , chmod :: Fn3 FilePath Number (JSCallback Unit) Unit
  , stat :: Fn2 FilePath (JSCallback StatsObj) Unit
  , link :: Fn3 FilePath FilePath (JSCallback Unit) Unit
  , symlink :: Fn4 FilePath FilePath String (JSCallback Unit) Unit
  , readlink :: Fn2 FilePath (JSCallback FilePath) Unit
  , realpath :: forall cache. Fn3 FilePath { | cache } (JSCallback FilePath) Unit
  , unlink :: Fn2 FilePath (JSCallback Unit) Unit
  , rmdir :: Fn2 FilePath (JSCallback Unit) Unit
  , mkdir :: Fn3 FilePath Number (JSCallback Unit) Unit
  , readFile :: forall a opts. Fn3 FilePath { | opts } (JSCallback a) Unit
  , writeFile :: forall a opts. Fn4 FilePath a { | opts } (JSCallback Unit) Unit
  }

-- |
-- Type synonym for callback functions.
--
type Callback eff a = Either String a -> Eff eff Unit

-- |
-- Renames a file.
--
rename :: forall eff. FilePath
                   -> FilePath
                   -> Callback eff Unit
                   -> Eff (fs :: FS | eff) Unit

rename oldFile newFile cb = return $ runFn3
  fs.rename oldFile newFile (handleCallback cb)

-- |
-- Truncates a file to the specified length.
--
truncate :: forall eff. FilePath
                     -> Number
                     -> Callback eff Unit
                     -> Eff (fs :: FS | eff) Unit

truncate file len cb = return $ runFn3
  fs.truncate file len (handleCallback cb)

-- |
-- Changes the ownership of a file.
--
chown :: forall eff. FilePath
                  -> Number
                  -> Number
                  -> Callback eff Unit
                  -> Eff (fs :: FS | eff) Unit

chown file uid gid cb = return $ runFn4
  fs.chown file uid gid (handleCallback cb)

-- |
-- Changes the permissions of a file.
--
chmod :: forall eff. FilePath
                  -> Number
                  -> Callback eff Unit
                  -> Eff (fs :: FS | eff) Unit

chmod file mode cb = return $ runFn3
  fs.chmod file mode (handleCallback cb)

-- |
-- Gets file statistics.
--
stat :: forall eff. FilePath
                 -> Callback eff Stats
                 -> Eff (fs :: FS | eff) Unit

stat file cb = return $ runFn2
  fs.stat file (handleCallback $ cb <<< (<$>) Stats)

-- |
-- Creates a link to an existing file.
--
link :: forall eff. FilePath
                 -> FilePath
                 -> Callback eff Unit
                 -> Eff (fs :: FS | eff) Unit

link src dst cb = return $ runFn3
  fs.link src dst (handleCallback cb)

-- |
-- Creates a symlink.
--
symlink :: forall eff. FilePath
                    -> FilePath
                    -> SymlinkType
                    -> Callback eff Unit
                    -> Eff (fs :: FS | eff) Unit

symlink src dest ty cb = return $ runFn4
  fs.symlink src dest (show ty) (handleCallback cb)

-- |
-- Reads the value of a symlink.
--
readlink :: forall eff. FilePath
                     -> Callback eff FilePath
                     -> Eff (fs :: FS | eff) Unit

readlink path cb = return $ runFn2
  fs.readlink path (handleCallback cb)

-- |
-- Find the canonicalized absolute location for a path.
--
realpath :: forall eff. FilePath
                     -> Callback eff FilePath
                     -> Eff (fs :: FS | eff) Unit

realpath path cb = return $ runFn3
  fs.realpath path {} (handleCallback cb)

-- |
-- Find the canonicalized absolute location for a path using a cache object for
-- already resolved paths.
--
realpath' :: forall eff cache. FilePath
                            -> { | cache }
                            -> Callback eff FilePath
                            -> Eff (fs :: FS | eff) Unit

realpath' path cache cb = return $ runFn3
  fs.realpath path cache (handleCallback cb)

-- |
-- Deletes a file.
--
unlink :: forall eff. FilePath
                   -> Callback eff Unit
                   -> Eff (fs :: FS | eff) Unit

unlink file cb = return $ runFn2
  fs.unlink file (handleCallback cb)

-- |
-- Deletes a directory.
--
rmdir :: forall eff. FilePath
                   -> Callback eff Unit
                   -> Eff (fs :: FS | eff) Unit

rmdir file cb = return $ runFn2
  fs.rmdir file (handleCallback cb)

-- |
-- Makes a new directory.
--
mkdir :: forall eff. FilePath
                  -> Callback eff Unit
                  -> Eff (fs :: FS | eff) Unit

mkdir = flip mkdir' 777

-- |
-- Makes a new directory with the specified permissions.
--
mkdir' :: forall eff. FilePath
                   -> Number
                   -> Callback eff Unit
                   -> Eff (fs :: FS | eff) Unit

mkdir' file mode cb = return $ runFn3
  fs.mkdir file mode (handleCallback cb)

-- |
-- Reads the entire contents of a file returning the result as a raw buffer.
--
readFile :: forall eff. FilePath
                     -> Callback eff Buffer
                     -> Eff (fs :: FS | eff) Unit

readFile file cb = return $ runFn3
  fs.readFile file {} (handleCallback cb)

-- |
-- Reads the entire contents of a text file with the specified encoding.
--
readTextFile :: forall eff. Encoding
                         -> FilePath
                         -> Callback eff String
                         -> Eff (fs :: FS | eff) Unit

readTextFile encoding file cb = return $ runFn3
  fs.readFile file { encoding: show encoding } (handleCallback cb)

-- |
-- Writes a buffer to a file.
--
writeFile :: forall eff. FilePath
                      -> Buffer
                      -> Callback eff Unit
                      -> Eff (fs :: FS | eff) Unit

writeFile file buff cb = return $ runFn4
  fs.writeFile file buff {} (handleCallback cb)

-- |
-- Writes text to a file using the specified encoding.
--
writeTextFile :: forall eff. Encoding
                          -> FilePath
                          -> String
                          -> Callback eff Unit
                          -> Eff (fs :: FS | eff) Unit

writeTextFile encoding file buff cb = return $ runFn4
  fs.writeFile file buff { encoding: show encoding } (handleCallback cb)
