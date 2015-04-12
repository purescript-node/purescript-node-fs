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
  , exists
  , FileDescriptor(..)
  , FileFlags(..)
  , BufferLength(..)
  , BufferOffset(..)
  , ByteCount(..)
  , FileMode(..)
  , FilePosition(..)
  , fdOpen
  , fdRead
  , fdNext
  , fdWrite
  , fdAppend
  , fdFlush
  , fdClose
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Data.Date
import Data.Time
import Data.Either
import Data.Function
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe(fromJust)
import Node.Buffer (Buffer(..), size)
import Node.Encoding
import Node.FS
import Node.FS.Stats
import Node.Path (FilePath())
import Node.FS.Perms

foreign import data FileDescriptor :: *

data FileFlags = R | R_PLUS | RS | RS_PLUS
               | W | WX | W_PLUS | WX_PLUS
               | A | AX | A_PLUS | AX_PLUS

type BufferLength = Number
type BufferOffset = Number
type ByteCount = Number
type FileMode = Number
type FilePosition = Number

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
  , mkdirSync :: Fn2 FilePath String Unit
  , readdirSync :: Fn1 FilePath [FilePath]
  , utimesSync :: Fn3 FilePath Number Number Unit
  , readFileSync :: forall a opts. Fn2 FilePath { | opts } a
  , writeFileSync :: forall a opts. Fn3 FilePath a { | opts } Unit
  , appendFileSync :: forall a opts. Fn3 FilePath a { | opts } Unit
  , existsSync :: FilePath -> Boolean
  , openSync :: Fn2 FilePath String FileDescriptor
  , readSync :: Fn5 FileDescriptor Buffer BufferOffset BufferLength FilePosition ByteCount
  , writeSync :: Fn5 FileDescriptor Buffer BufferOffset BufferLength FilePosition ByteCount
  , fsyncSync :: Fn1 FileDescriptor Unit
  , closeSync :: Fn1 FileDescriptor Unit
  }

foreign import createSync
  """
  var fs = require('fs');
  function createSync(file, flags, mode) {
    return fs.openSync(file, flags, mode);
  }
  """
  :: Fn3 FilePath String FileMode FileDescriptor

foreign import writeSeqSync
  """
  var fs = require('fs');
  function writeSeqSync(fd, buffer, offset, len) {
    return fs.writeSync(fd, buffer, offset, len);
  }
  """
  :: Fn4 FileDescriptor Buffer BufferOffset BufferLength ByteCount

foreign import readSeqSync
  """
  var fs = require('fs');
  function readSeqSync(fd, buffer, offset, len) {
    return fs.readSync(fd, buffer, offset, len);
  }
  """
  :: Fn4 FileDescriptor Buffer BufferOffset BufferLength ByteCount

foreign import mkEff
  "function mkEff(action) {\
  \  return action;\
  \}" :: forall eff a. (Unit -> a)
                    -> Eff eff a

-- |
-- Renames a file.
--
rename :: forall eff. FilePath
                   -> FilePath
                   -> Eff (fs :: FS, err :: Exception | eff) Unit

rename oldFile newFile = mkEff $ \_ -> runFn2
  fs.renameSync oldFile newFile

-- |
-- Truncates a file to the specified length.
--
truncate :: forall eff. FilePath
                     -> Number
                     -> Eff (fs :: FS, err :: Exception | eff) Unit

truncate file len = mkEff $ \_ -> runFn2
  fs.truncateSync file len

-- |
-- Changes the ownership of a file.
--
chown :: forall eff. FilePath
                  -> Number
                  -> Number
                  -> Eff (fs :: FS, err :: Exception | eff) Unit

chown file uid gid = mkEff $ \_ -> runFn3
  fs.chownSync file uid gid

-- |
-- Changes the permissions of a file.
--
chmod :: forall eff. FilePath
                  -> Number
                  -> Eff (fs :: FS, err :: Exception | eff) Unit

chmod file mode = mkEff $ \_ -> runFn2
  fs.chmodSync file mode

-- |
-- Gets file statistics.
--
stat :: forall eff. FilePath
                 -> Eff (fs :: FS, err :: Exception | eff) Stats

stat file = return $ Stats $ runFn1
  fs.statSync file

-- |
-- Creates a link to an existing file.
--
link :: forall eff. FilePath
                 -> FilePath
                 -> Eff (fs :: FS, err :: Exception | eff) Unit

link src dst = mkEff $ \_ -> runFn2
  fs.linkSync src dst

-- |
-- Creates a symlink.
--
symlink :: forall eff. FilePath
                    -> FilePath
                    -> SymlinkType
                    -> Eff (fs :: FS, err :: Exception | eff) Unit

symlink src dst ty = mkEff $ \_ -> runFn3
  fs.symlinkSync src dst (show ty)

-- |
-- Reads the value of a symlink.
--
readlink :: forall eff. FilePath
                     -> Eff (fs :: FS, err :: Exception | eff) FilePath

readlink path = mkEff $ \_ -> runFn1
  fs.readlinkSync path

-- |
-- Find the canonicalized absolute location for a path.
--
realpath :: forall eff. FilePath
                     -> Eff (fs :: FS, err :: Exception | eff) FilePath

realpath path = mkEff $ \_ -> runFn2
  fs.realpathSync path {}

-- |
-- Find the canonicalized absolute location for a path using a cache object for
-- already resolved paths.
--
realpath' :: forall eff cache. FilePath
                            -> { | cache }
                            -> Eff (fs :: FS, err :: Exception | eff) FilePath

realpath' path cache = mkEff $ \_ -> runFn2
  fs.realpathSync path cache

-- |
-- Deletes a file.
--
unlink :: forall eff. FilePath
                   -> Eff (fs :: FS, err :: Exception | eff) Unit

unlink file = mkEff $ \_ -> runFn1
  fs.unlinkSync file

-- |
-- Deletes a directory.
--
rmdir :: forall eff. FilePath
                  -> Eff (fs :: FS, err :: Exception | eff) Unit

rmdir file = mkEff $ \_ -> runFn1
  fs.rmdirSync file

-- |
-- Makes a new directory.
--
mkdir :: forall eff. FilePath
                  -> Eff (fs :: FS, err :: Exception | eff) Unit

mkdir = flip mkdir' (fromJust $ permsFromString "777")

-- |
-- Makes a new directory with the specified permissions.
--
mkdir' :: forall eff. FilePath
                   -> Perms
                   -> Eff (fs :: FS, err :: Exception | eff) Unit

mkdir' file perms = mkEff $ \_ -> runFn2
  fs.mkdirSync file (permsToString perms)

-- |
-- Reads the contents of a directory.
--
readdir :: forall eff. FilePath
                    -> Eff (fs :: FS, err :: Exception | eff) [FilePath]

readdir file = mkEff $ \_ -> runFn1
  fs.readdirSync file

-- |
-- Sets the accessed and modified times for the specified file.
--
utimes :: forall eff. FilePath
                   -> Date
                   -> Date
                   -> Eff (fs :: FS, err :: Exception | eff) Unit

utimes file atime mtime = mkEff $ \_ -> runFn3
  fs.utimesSync file
                (ms (toEpochMilliseconds atime) / 1000)
                (ms (toEpochMilliseconds mtime) / 1000)
  where
  ms (Milliseconds n) = n

-- |
-- Reads the entire contents of a file returning the result as a raw buffer.
--
readFile :: forall eff. FilePath
                     -> Eff (fs :: FS, err :: Exception | eff) Buffer

readFile file = mkEff $ \_ -> runFn2
  fs.readFileSync file {}

-- |
-- Reads the entire contents of a text file with the specified encoding.
--
readTextFile :: forall eff. Encoding
                         -> FilePath
                         -> Eff (fs :: FS, err :: Exception | eff) String

readTextFile encoding file = mkEff $ \_ -> runFn2
  fs.readFileSync file { encoding: show encoding }

-- |
-- Writes a buffer to a file.
--
writeFile :: forall eff. FilePath
                      -> Buffer
                      -> Eff (fs :: FS, err :: Exception | eff) Unit

writeFile file buff = mkEff $ \_ -> runFn3
  fs.writeFileSync file buff {}

-- |
-- Writes text to a file using the specified encoding.
--
writeTextFile :: forall eff. Encoding
                          -> FilePath
                          -> String
                          -> Eff (fs :: FS, err :: Exception | eff) Unit

writeTextFile encoding file text = mkEff $ \_ -> runFn3
  fs.writeFileSync file text { encoding: show encoding }

-- |
-- Appends the contents of a buffer to a file.
--
appendFile :: forall eff. FilePath
                       -> Buffer
                       -> Eff (fs :: FS, err :: Exception | eff) Unit

appendFile file buff = mkEff $ \_ -> runFn3
  fs.appendFileSync file buff {}

-- |
-- Appends text to a file using the specified encoding.
--
appendTextFile :: forall eff. Encoding
                           -> FilePath
                           -> String
                           -> Eff (fs :: FS, err :: Exception | eff) Unit

appendTextFile encoding file buff = mkEff $ \_ -> runFn3
  fs.appendFileSync file buff { encoding: show encoding }

-- |
-- Check if the path exists.
--
exists :: forall eff. FilePath
                   -> Eff (fs :: FS | eff) Boolean
exists file = mkEff $ \_ -> fs.existsSync file

{- Synchronous File Descriptor Functions -}

--|
-- Open a file synchronously.  See <a
-- href="http://nodejs.org/api/fs.html#fs_fs_opensync_path_flags_mode">Node
-- Documentation</a> for details.
--
fdOpen :: forall opts eff.
          FilePath
       -> FileFlags
       -> Maybe FileMode
       -> Eff (err :: Exception, fs :: FS | eff) FileDescriptor
fdOpen file flags mode =
  case mode of
    Nothing  -> mkEff $ \_ -> runFn2 fs.openSync file (toStr flags)
    (Just m) -> mkEff $ \_ -> runFn3 createSync file (toStr flags) m
  where
    toStr R       = "r"
    toStr R_PLUS  = "r+"
    toStr RS      = "rs"
    toStr RS_PLUS = "rs+"
    toStr W       = "w"
    toStr WX      = "wx"
    toStr W_PLUS  = "w+"
    toStr WX_PLUS = "wx+"
    toStr A       = "a"
    toStr AX      = "ax"
    toStr A_PLUS  = "a+"
    toStr AX_PLUS = "ax+"

--|
-- Read to a file synchronously.  See <a
-- href="http://nodejs.org/api/fs.html#fs_fs_readsync_fd_buffer_offset_length_position">Node
-- ocumentation</a> for details.
--
fdRead :: forall eff.
          FileDescriptor
       -> Buffer
       -> BufferOffset
       -> BufferLength
       -> Maybe FilePosition
       -> Eff (err :: Exception, fs :: FS | eff) ByteCount
fdRead fd buff off len Nothing =
  mkEff $ \_ -> runFn4 readSeqSync fd buff off len
fdRead fd buff off len (Just pos) =
  mkEff $ \_ -> runFn5 fs.readSync fd buff off len pos

--|
-- Convienence function to fill the whole buffer from the current
-- file position.
--
fdNext :: forall eff.
          FileDescriptor
       -> Buffer
       -> Eff (err :: Exception, fs :: FS | eff) ByteCount
fdNext fd buff = fdRead fd buff 0 (size buff) Nothing

--|
-- Write to a file synchronously.  See <a
-- href="http://nodejs.org/api/fs.html#fs_fs_writesync_fd_buffer_offset_length_position">Node
-- Documentation</a> for details.
--
fdWrite :: forall eff.
           FileDescriptor
        -> Buffer
        -> BufferOffset
        -> BufferLength
        -> Maybe FilePosition
        -> Eff (err :: Exception, fs :: FS | eff) ByteCount
fdWrite fd buff off len Nothing =
  mkEff $ \_ -> runFn4 writeSeqSync fd buff off len
fdWrite fd buff off len (Just pos) =
  mkEff $ \_ -> runFn5 fs.writeSync fd buff off len pos

--|
-- Convienence function to append the whole buffer to the current
-- file position.
--
fdAppend :: forall eff.
            FileDescriptor
         -> Buffer
         -> Eff (err :: Exception, fs :: FS | eff) ByteCount
fdAppend fd buff = fdWrite fd buff 0 (size buff) Nothing

--|
-- Flush a file synchronously.  See <a
-- href="http://nodejs.org/api/fs.html#fs_fs_fsyncsync_fd">Node
-- Documentation</a> for details.
--
fdFlush :: forall eff.
           FileDescriptor
        -> Eff (err :: Exception, fs :: FS | eff) Unit
fdFlush fd = mkEff $ \_ -> runFn1 fs.fsyncSync fd

--|
-- Close a file synchronously.  See <a
-- href="http://nodejs.org/api/fs.html#fs_fs_closesync_fd">Node
-- Documentation</a> for details.
--
fdClose :: forall eff.
           FileDescriptor
        -> Eff (err :: Exception, fs :: FS | eff) Unit
fdClose fd = mkEff $ \_ -> runFn1 fs.closeSync fd
