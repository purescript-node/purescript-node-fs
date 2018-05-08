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
  , fdOpen
  , fdRead
  , fdNext
  , fdWrite
  , fdAppend
  , fdFlush
  , fdClose
  ) where

import Prelude
import Effect (Effect)
import Data.DateTime (DateTime)
import Data.Time.Duration (Milliseconds(..))
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Function.Uncurried (Fn1, Fn5, Fn3, Fn2,
                                runFn1, runFn5, runFn3, runFn2)
import Data.Nullable (Nullable(), toNullable)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Node.Buffer (Buffer(), size)
import Node.Encoding (Encoding)

import Node.FS (FileDescriptor, ByteCount, FilePosition, BufferLength,
                BufferOffset, FileMode, FileFlags, SymlinkType,
                fileFlagsToNode, symlinkTypeToNode)
import Node.FS.Stats (StatsObj, Stats(..))
import Node.Path (FilePath())
import Node.FS.Perms (Perms, permsToString, all, mkPerms)
import Node.FS.Internal (mkEffect, unsafeRequireFS)

fs ::
  { renameSync :: Fn2 FilePath FilePath Unit
  , truncateSync :: Fn2 FilePath Int Unit
  , chownSync :: Fn3 FilePath Int Int Unit
  , chmodSync :: Fn2 FilePath String Unit
  , statSync :: Fn1 FilePath StatsObj
  , linkSync :: Fn2 FilePath FilePath Unit
  , symlinkSync :: Fn3 FilePath FilePath String Unit
  , readlinkSync :: Fn1 FilePath FilePath
  , realpathSync :: forall cache. Fn2 FilePath { | cache } FilePath
  , unlinkSync :: Fn1 FilePath Unit
  , rmdirSync :: Fn1 FilePath Unit
  , mkdirSync :: Fn2 FilePath String Unit
  , readdirSync :: Fn1 FilePath (Array FilePath)
  , utimesSync :: Fn3 FilePath Int Int Unit
  , readFileSync :: forall a opts. Fn2 FilePath { | opts } a
  , writeFileSync :: forall a opts. Fn3 FilePath a { | opts } Unit
  , appendFileSync :: forall a opts. Fn3 FilePath a { | opts } Unit
  , existsSync :: FilePath -> Boolean
  , openSync :: Fn3 FilePath String (Nullable FileMode) FileDescriptor
  , readSync :: Fn5 FileDescriptor Buffer BufferOffset BufferLength (Nullable FilePosition) ByteCount
  , writeSync :: Fn5 FileDescriptor Buffer BufferOffset BufferLength (Nullable FilePosition) ByteCount
  , fsyncSync :: Fn1 FileDescriptor Unit
  , closeSync :: Fn1 FileDescriptor Unit
  }
fs = unsafeRequireFS

-- | Renames a file.
rename :: FilePath
       -> FilePath
       -> Effect Unit

rename oldFile newFile = mkEffect $ \_ -> runFn2
  fs.renameSync oldFile newFile

-- | Truncates a file to the specified length.
truncate :: FilePath
         -> Int
         -> Effect Unit

truncate file len = mkEffect $ \_ -> runFn2
  fs.truncateSync file len

-- | Changes the ownership of a file.
chown :: FilePath
      -> Int
      -> Int
      -> Effect Unit

chown file uid gid = mkEffect $ \_ -> runFn3
  fs.chownSync file uid gid

-- | Changes the permissions of a file.
chmod :: FilePath
      -> Perms
      -> Effect Unit

chmod file perms = mkEffect $ \_ -> runFn2
  fs.chmodSync file (permsToString perms)

-- | Gets file statistics.
stat :: FilePath
     -> Effect Stats

stat file = map Stats $ mkEffect $ \_ -> runFn1
  fs.statSync file

-- | Creates a link to an existing file.
link :: FilePath
     -> FilePath
     -> Effect Unit

link src dst = mkEffect $ \_ -> runFn2
  fs.linkSync src dst

-- | Creates a symlink.
symlink :: FilePath
        -> FilePath
        -> SymlinkType
        -> Effect Unit

symlink src dst ty = mkEffect $ \_ -> runFn3
  fs.symlinkSync src dst (symlinkTypeToNode ty)

-- | Reads the value of a symlink.
readlink :: FilePath
         -> Effect FilePath

readlink path = mkEffect $ \_ -> runFn1
  fs.readlinkSync path

-- | Find the canonicalized absolute location for a path.
realpath :: FilePath
         -> Effect FilePath

realpath path = mkEffect $ \_ -> runFn2
  fs.realpathSync path {}

-- | Find the canonicalized absolute location for a path using a cache object for
-- | already resolved paths.
realpath' :: forall  cache. FilePath
                            -> { | cache }
                            -> Effect FilePath

realpath' path cache = mkEffect $ \_ -> runFn2
  fs.realpathSync path cache

-- | Deletes a file.
unlink :: FilePath
       -> Effect Unit

unlink file = mkEffect $ \_ -> runFn1
  fs.unlinkSync file

-- | Deletes a directory.
rmdir :: FilePath
      -> Effect Unit

rmdir file = mkEffect $ \_ -> runFn1
  fs.rmdirSync file

-- | Makes a new directory.
mkdir :: FilePath
      -> Effect Unit

mkdir = flip mkdir' $ mkPerms all all all

-- | Makes a new directory with the specified permissions.
mkdir' :: FilePath
       -> Perms
       -> Effect Unit

mkdir' file perms = mkEffect $ \_ -> runFn2
  fs.mkdirSync file (permsToString perms)

-- | Reads the contents of a directory.
readdir :: FilePath
        -> Effect (Array FilePath)

readdir file = mkEffect $ \_ -> runFn1
  fs.readdirSync file

-- | Sets the accessed and modified times for the specified file.
utimes :: FilePath
       -> DateTime
       -> DateTime
       -> Effect Unit

utimes file atime mtime = mkEffect $ \_ -> runFn3
  fs.utimesSync file
                (fromDate atime)
                (fromDate mtime)
  where
  fromDate date = ms (toEpochMilliseconds date) / 1000
  ms (Milliseconds n) = round n
  toEpochMilliseconds = unInstant <<< fromDateTime

-- | Reads the entire contents of a file returning the result as a raw buffer.
readFile :: FilePath
         -> Effect Buffer

readFile file = mkEffect $ \_ -> runFn2
  fs.readFileSync file {}

-- | Reads the entire contents of a text file with the specified encoding.
readTextFile :: Encoding
             -> FilePath
             -> Effect String

readTextFile encoding file = mkEffect $ \_ -> runFn2
  fs.readFileSync file { encoding: show encoding }

-- | Writes a buffer to a file.
writeFile :: FilePath
          -> Buffer
          -> Effect Unit

writeFile file buff = mkEffect $ \_ -> runFn3
  fs.writeFileSync file buff {}

-- | Writes text to a file using the specified encoding.
writeTextFile :: Encoding
              -> FilePath
              -> String
              -> Effect Unit

writeTextFile encoding file text = mkEffect $ \_ -> runFn3
  fs.writeFileSync file text { encoding: show encoding }

-- | Appends the contents of a buffer to a file.
appendFile :: FilePath
           -> Buffer
           -> Effect Unit

appendFile file buff = mkEffect $ \_ -> runFn3
  fs.appendFileSync file buff {}

-- | Appends text to a file using the specified encoding.
appendTextFile :: Encoding
               -> FilePath
               -> String
               -> Effect Unit

appendTextFile encoding file buff = mkEffect $ \_ -> runFn3
  fs.appendFileSync file buff { encoding: show encoding }

-- | Check if the path exists.
exists :: FilePath
       -> Effect Boolean
exists file = mkEffect $ \_ -> fs.existsSync file

-- | Open a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_opensync_path_flags_mode)
-- | for details.
fdOpen :: FilePath
       -> FileFlags
       -> Maybe FileMode
       -> Effect FileDescriptor
fdOpen file flags mode = mkEffect $ \_ ->
  runFn3 fs.openSync file (fileFlagsToNode flags) (toNullable mode)

-- | Read from a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_readsync_fd_buffer_offset_length_position)
-- | for details.
fdRead :: FileDescriptor
       -> Buffer
       -> BufferOffset
       -> BufferLength
       -> Maybe FilePosition
       -> Effect ByteCount
fdRead fd buff off len pos =
  mkEffect $ \_ -> runFn5 fs.readSync fd buff off len (toNullable pos)

-- | Convenience function to fill the whole buffer from the current
-- | file position.
fdNext :: FileDescriptor
       -> Buffer
       -> Effect ByteCount
fdNext fd buff = do
  sz <- size buff
  fdRead fd buff 0 sz Nothing

-- | Write to a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_writesync_fd_buffer_offset_length_position)
-- | for details.
fdWrite :: FileDescriptor
        -> Buffer
        -> BufferOffset
        -> BufferLength
        -> Maybe FilePosition
        -> Effect ByteCount
fdWrite fd buff off len pos =
  mkEffect $ \_ -> runFn5 fs.writeSync fd buff off len (toNullable pos)

-- | Convenience function to append the whole buffer to the current
-- | file position.
fdAppend :: FileDescriptor
         -> Buffer
         -> Effect ByteCount
fdAppend fd buff = do
  sz <- size buff
  fdWrite fd buff 0 sz Nothing

-- | Flush a file synchronously.  See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_fsyncsync_fd)
-- | for details.
fdFlush :: FileDescriptor
        -> Effect Unit
fdFlush fd = mkEffect $ \_ -> runFn1 fs.fsyncSync fd

-- | Close a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_closesync_fd)
-- | for details.
fdClose :: FileDescriptor
        -> Effect Unit
fdClose fd = mkEffect $ \_ -> runFn1 fs.closeSync fd
