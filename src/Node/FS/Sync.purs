module Node.FS.Sync
  ( rename
  , truncate
  , chown
  , chmod
  , stat
  , lstat
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
import Node.FS.Internal (mkEffect)

foreign import renameSyncImpl :: Fn2 FilePath FilePath Unit
foreign import truncateSyncImpl :: Fn2 FilePath Int Unit
foreign import chownSyncImpl :: Fn3 FilePath Int Int Unit
foreign import chmodSyncImpl :: Fn2 FilePath String Unit
foreign import statSyncImpl :: Fn1 FilePath StatsObj
foreign import lstatSyncImpl :: Fn1 FilePath StatsObj
foreign import linkSyncImpl :: Fn2 FilePath FilePath Unit
foreign import symlinkSyncImpl :: Fn3 FilePath FilePath String Unit
foreign import readlinkSyncImpl :: Fn1 FilePath FilePath
foreign import realpathSyncImpl :: forall cache. Fn2 FilePath { | cache } FilePath
foreign import unlinkSyncImpl :: Fn1 FilePath Unit
foreign import rmdirSyncImpl :: Fn1 FilePath Unit
foreign import mkdirSyncImpl :: Fn2 FilePath { recursive :: Boolean, mode :: String } Unit
foreign import readdirSyncImpl :: Fn1 FilePath (Array FilePath)
foreign import utimesSyncImpl :: Fn3 FilePath Int Int Unit
foreign import readFileSyncImpl :: forall a opts. Fn2 FilePath { | opts } a
foreign import writeFileSyncImpl :: forall a opts. Fn3 FilePath a { | opts } Unit
foreign import appendFileSyncImpl :: forall a opts. Fn3 FilePath a { | opts } Unit
foreign import existsSyncImpl :: FilePath -> Boolean
foreign import openSyncImpl :: Fn3 FilePath String (Nullable FileMode) FileDescriptor
foreign import readSyncImpl :: Fn5 FileDescriptor Buffer BufferOffset BufferLength (Nullable FilePosition) ByteCount
foreign import writeSyncImpl :: Fn5 FileDescriptor Buffer BufferOffset BufferLength (Nullable FilePosition) ByteCount
foreign import fsyncSyncImpl :: Fn1 FileDescriptor Unit
foreign import closeSyncImpl :: Fn1 FileDescriptor Unit

-- | Renames a file.
rename :: FilePath
       -> FilePath
       -> Effect Unit

rename oldFile newFile = mkEffect $ \_ -> runFn2
  renameSyncImpl oldFile newFile

-- | Truncates a file to the specified length.
truncate :: FilePath
         -> Int
         -> Effect Unit

truncate file len = mkEffect $ \_ -> runFn2
  truncateSyncImpl file len

-- | Changes the ownership of a file.
chown :: FilePath
      -> Int
      -> Int
      -> Effect Unit

chown file uid gid = mkEffect $ \_ -> runFn3
  chownSyncImpl file uid gid

-- | Changes the permissions of a file.
chmod :: FilePath
      -> Perms
      -> Effect Unit

chmod file perms = mkEffect $ \_ -> runFn2
  chmodSyncImpl file (permsToString perms)

-- | Gets file statistics.
stat :: FilePath
     -> Effect Stats

stat file = map Stats $ mkEffect $ \_ -> runFn1
  statSyncImpl file

-- | Gets file or symlink statistics. `lstat` is identical to `stat`, except
-- | that if the `FilePath` is a symbolic link, then the link itself is stat-ed,
-- | not the file that it refers to.
lstat :: FilePath
     -> Effect Stats

lstat file = map Stats $ mkEffect $ \_ -> runFn1
  lstatSyncImpl file

-- | Creates a link to an existing file.
link :: FilePath
     -> FilePath
     -> Effect Unit

link src dst = mkEffect $ \_ -> runFn2
  linkSyncImpl src dst

-- | Creates a symlink.
symlink :: FilePath
        -> FilePath
        -> SymlinkType
        -> Effect Unit

symlink src dst ty = mkEffect $ \_ -> runFn3
  symlinkSyncImpl src dst (symlinkTypeToNode ty)

-- | Reads the value of a symlink.
readlink :: FilePath
         -> Effect FilePath

readlink path = mkEffect $ \_ -> runFn1
  readlinkSyncImpl path

-- | Find the canonicalized absolute location for a path.
realpath :: FilePath
         -> Effect FilePath

realpath path = mkEffect $ \_ -> runFn2
  realpathSyncImpl path {}

-- | Find the canonicalized absolute location for a path using a cache object for
-- | already resolved paths.
realpath' :: forall  cache. FilePath
                            -> { | cache }
                            -> Effect FilePath

realpath' path cache = mkEffect $ \_ -> runFn2
  realpathSyncImpl path cache

-- | Deletes a file.
unlink :: FilePath
       -> Effect Unit

unlink file = mkEffect $ \_ -> runFn1
  unlinkSyncImpl file

-- | Deletes a directory.
rmdir :: FilePath
      -> Effect Unit

rmdir file = mkEffect $ \_ -> runFn1
  rmdirSyncImpl file

-- | Makes a new directory.
mkdir :: FilePath
      -> Effect Unit
mkdir path = mkdir' path { recursive: false, mode: mkPerms all all all }

-- | Makes a new directory with the specified permissions.
mkdir'
  :: FilePath
  -> { recursive :: Boolean, mode :: Perms }
  -> Effect Unit
mkdir' file { recursive, mode: perms } = mkEffect $ \_ -> runFn2
  mkdirSyncImpl file { recursive, mode: permsToString perms }

-- | Reads the contents of a directory.
readdir :: FilePath
        -> Effect (Array FilePath)

readdir file = mkEffect $ \_ -> runFn1
  readdirSyncImpl file

-- | Sets the accessed and modified times for the specified file.
utimes :: FilePath
       -> DateTime
       -> DateTime
       -> Effect Unit

utimes file atime mtime = mkEffect $ \_ -> runFn3
  utimesSyncImpl file
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
  readFileSyncImpl file {}

-- | Reads the entire contents of a text file with the specified encoding.
readTextFile :: Encoding
             -> FilePath
             -> Effect String

readTextFile encoding file = mkEffect $ \_ -> runFn2
  readFileSyncImpl file { encoding: show encoding }

-- | Writes a buffer to a file.
writeFile :: FilePath
          -> Buffer
          -> Effect Unit

writeFile file buff = mkEffect $ \_ -> runFn3
  writeFileSyncImpl file buff {}

-- | Writes text to a file using the specified encoding.
writeTextFile :: Encoding
              -> FilePath
              -> String
              -> Effect Unit

writeTextFile encoding file text = mkEffect $ \_ -> runFn3
  writeFileSyncImpl file text { encoding: show encoding }

-- | Appends the contents of a buffer to a file.
appendFile :: FilePath
           -> Buffer
           -> Effect Unit

appendFile file buff = mkEffect $ \_ -> runFn3
  appendFileSyncImpl file buff {}

-- | Appends text to a file using the specified encoding.
appendTextFile :: Encoding
               -> FilePath
               -> String
               -> Effect Unit

appendTextFile encoding file buff = mkEffect $ \_ -> runFn3
  appendFileSyncImpl file buff { encoding: show encoding }

-- | Check if the path exists.
exists :: FilePath
       -> Effect Boolean
exists file = mkEffect $ \_ -> existsSyncImpl file

-- | Open a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_opensync_path_flags_mode)
-- | for details.
fdOpen :: FilePath
       -> FileFlags
       -> Maybe FileMode
       -> Effect FileDescriptor
fdOpen file flags mode = mkEffect $ \_ ->
  runFn3 openSyncImpl file (fileFlagsToNode flags) (toNullable mode)

-- | Read from a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_readsync_fd_buffer_offset_length_position)
-- | for details.
fdRead :: FileDescriptor
       -> Buffer
       -> BufferOffset
       -> BufferLength
       -> Maybe FilePosition
       -> Effect ByteCount
fdRead fd buff off len pos =
  mkEffect $ \_ -> runFn5 readSyncImpl fd buff off len (toNullable pos)

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
  mkEffect $ \_ -> runFn5 writeSyncImpl fd buff off len (toNullable pos)

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
fdFlush fd = mkEffect $ \_ -> runFn1 fsyncSyncImpl fd

-- | Close a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_closesync_fd)
-- | for details.
fdClose :: FileDescriptor
        -> Effect Unit
fdClose fd = mkEffect $ \_ -> runFn1 closeSyncImpl fd
