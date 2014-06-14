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
  , readdir
  , utimes
  , readFile
  , readTextFile
  , writeFile
  , writeTextFile
  , appendFile
  , appendTextFile
  ) where

import Control.Monad.Eff
import Data.Date
import Data.Either
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
  , readdirSync :: Fn1 FilePath [FilePath]
  , utimesSync :: Fn3 FilePath Number Number Unit
  , readFileSync :: forall a opts. Fn2 FilePath { | opts } a
  , writeFileSync :: forall a opts. Fn3 FilePath a { | opts } Unit
  , appendFileSync :: forall a opts. Fn3 FilePath a { | opts } Unit
  }

foreign import mkFSAction
  "function mkFSAction(fail) {\
  \  return function (success) {\
  \    return function (f) {\
  \      return function () {\
  \        try {\
  \          return success(f());\
  \        } catch (e) {\
  \          return fail(e);\
  \        }\
  \      };\
  \    };\
  \  };\
  \}" :: forall eff a. (Error -> Either Error a) 
                    -> (a -> Either Error a)
                    -> (Unit -> a)
                    -> Eff eff (Either Error a)

-- |
-- Renames a file.
--
rename :: forall eff. FilePath
                   -> FilePath
                   -> Eff (fs :: FS | eff) (Either Error Unit)

rename oldFile newFile = mkFSAction Left Right $ \_ -> runFn2
  fs.renameSync oldFile newFile

-- |
-- Truncates a file to the specified length.
--
truncate :: forall eff. FilePath
                     -> Number
                     -> Eff (fs :: FS | eff) (Either Error Unit)

truncate file len = mkFSAction Left Right $ \_ -> runFn2
  fs.truncateSync file len

-- |
-- Changes the ownership of a file.
--
chown :: forall eff. FilePath
                  -> Number
                  -> Number
                  -> Eff (fs :: FS | eff) (Either Error Unit)

chown file uid gid = mkFSAction Left Right $ \_ -> runFn3
  fs.chownSync file uid gid

-- |
-- Changes the permissions of a file.
--
chmod :: forall eff. FilePath
                  -> Number
                  -> Eff (fs :: FS | eff) (Either Error Unit)

chmod file mode = mkFSAction Left Right $ \_ -> runFn2
  fs.chmodSync file mode

-- |
-- Gets file statistics.
--
stat :: forall eff. FilePath
                 -> Eff (fs :: FS | eff) (Either Error Stats)

stat file = mkFSAction Left Right $ \_ -> Stats $ runFn1
  fs.statSync file

-- |
-- Creates a link to an existing file.
--
link :: forall eff. FilePath
                 -> FilePath
                 -> Eff (fs :: FS | eff) (Either Error Unit)

link src dst = mkFSAction Left Right $ \_ -> runFn2
  fs.linkSync src dst

-- |
-- Creates a symlink.
--
symlink :: forall eff. FilePath
                    -> FilePath
                    -> SymlinkType
                    -> Eff (fs :: FS | eff) (Either Error Unit)

symlink src dst ty = mkFSAction Left Right $ \_ -> runFn3
  fs.symlinkSync src dst (show ty)

-- |
-- Reads the value of a symlink.
--
readlink :: forall eff. FilePath
                     -> Eff (fs :: FS | eff) (Either Error FilePath)

readlink path = mkFSAction Left Right $ \_ -> runFn1
  fs.readlinkSync path

-- |
-- Find the canonicalized absolute location for a path.
--
realpath :: forall eff. FilePath
                     -> Eff (fs :: FS | eff) (Either Error FilePath)

realpath path = mkFSAction Left Right $ \_ -> runFn2
  fs.realpathSync path {}

-- |
-- Find the canonicalized absolute location for a path using a cache object for
-- already resolved paths.
--
realpath' :: forall eff cache. FilePath
                            -> { | cache }
                            -> Eff (fs :: FS | eff) (Either Error FilePath)

realpath' path cache = mkFSAction Left Right $ \_ -> runFn2
  fs.realpathSync path cache

-- |
-- Deletes a file.
--
unlink :: forall eff. FilePath
                   -> Eff (fs :: FS | eff) (Either Error Unit)

unlink file = mkFSAction Left Right $ \_ -> runFn1
  fs.unlinkSync file

-- |
-- Deletes a directory.
--
rmdir :: forall eff. FilePath
                  -> Eff (fs :: FS | eff) (Either Error Unit)

rmdir file = mkFSAction Left Right $ \_ -> runFn1
  fs.rmdirSync file

-- |
-- Makes a new directory.
--
mkdir :: forall eff. FilePath
                  -> Eff (fs :: FS | eff) (Either Error Unit)

mkdir = flip mkdir' 777

-- |
-- Makes a new directory with the specified permissions.
--
mkdir' :: forall eff. FilePath
                   -> Number
                   -> Eff (fs :: FS | eff) (Either Error Unit)

mkdir' file mode = mkFSAction Left Right $ \_ -> runFn2
  fs.mkdirSync file mode

-- |
-- Reads the contents of a directory.
--
readdir :: forall eff. FilePath
                    -> Eff (fs :: FS | eff) (Either Error [FilePath])

readdir file = mkFSAction Left Right $ \_ -> runFn1
  fs.readdirSync file

-- |
-- Sets the accessed and modified times for the specified file.
--
utimes :: forall eff. FilePath
                   -> Date
                   -> Date
                   -> Eff (fs :: FS | eff) (Either Error Unit)

utimes file atime mtime = mkFSAction Left Right $ \_ -> runFn3
  fs.utimesSync file
                ((toEpochMilliseconds atime) / 1000)
                ((toEpochMilliseconds mtime) / 1000)

-- |
-- Reads the entire contents of a file returning the result as a raw buffer.
--
readFile :: forall eff. FilePath
                     -> Eff (fs :: FS | eff) (Either Error Buffer)

readFile file = mkFSAction Left Right $ \_ -> runFn2
  fs.readFileSync file {}

-- |
-- Reads the entire contents of a text file with the specified encoding.
--
readTextFile :: forall eff. Encoding
                         -> FilePath
                         -> Eff (fs :: FS | eff) (Either Error String)

readTextFile encoding file = mkFSAction Left Right $ \_ -> runFn2
  fs.readFileSync file { encoding: show encoding }

-- |
-- Writes a buffer to a file.
--
writeFile :: forall eff. FilePath
                      -> Buffer
                      -> Eff (fs :: FS | eff) (Either Error Unit)

writeFile file buff = mkFSAction Left Right $ \_ -> runFn3
  fs.writeFileSync file buff {}

-- |
-- Writes text to a file using the specified encoding.
--
writeTextFile :: forall eff. Encoding
                          -> FilePath
                          -> String
                          -> Eff (fs :: FS | eff) (Either Error Unit)

writeTextFile encoding file buff = mkFSAction Left Right $ \_ -> runFn3
  fs.writeFileSync file buff { encoding: show encoding }

-- |
-- Appends the contents of a buffer to a file.
--
appendFile :: forall eff. FilePath
                       -> Buffer
                       -> Eff (fs :: FS | eff) (Either Error Unit)

appendFile file buff = mkFSAction Left Right $ \_ -> runFn3
  fs.appendFileSync file buff {}

-- |
-- Appends text to a file using the specified encoding.
--
appendTextFile :: forall eff. Encoding
                           -> FilePath
                           -> String
                           -> Eff (fs :: FS | eff) (Either Error Unit)

appendTextFile encoding file buff = mkFSAction Left Right $ \_ -> runFn3
  fs.appendFileSync file buff { encoding: show encoding }
