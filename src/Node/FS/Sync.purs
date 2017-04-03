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
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.DateTime (DateTime)
import Data.Time.Duration (Milliseconds(..))
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Function.Uncurried (Fn1, Fn5, Fn3, Fn2,
                                runFn1, runFn5, runFn3, runFn2)
import Data.Nullable (Nullable(), toNullable)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Node.Buffer (Buffer(), BUFFER(), size)
import Node.Encoding (Encoding)

import Node.FS (FS, FileDescriptor, ByteCount, FilePosition, BufferLength,
                BufferOffset, FileMode, FileFlags, SymlinkType,
                fileFlagsToNode, symlinkTypeToNode)
import Node.FS.Stats (StatsObj, Stats(..))
import Node.Path (FilePath())
import Node.FS.Perms (Perms, permsToString, all, mkPerms)
import Node.FS.Internal (mkEff, unsafeRequireFS)

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
rename :: forall eff. FilePath
                   -> FilePath
                   -> Eff (fs :: FS, exception :: EXCEPTION | eff) Unit

rename oldFile newFile = mkEff $ \_ -> runFn2
  fs.renameSync oldFile newFile

-- | Truncates a file to the specified length.
truncate :: forall eff. FilePath
                     -> Int
                     -> Eff (fs :: FS, exception :: EXCEPTION | eff) Unit

truncate file len = mkEff $ \_ -> runFn2
  fs.truncateSync file len

-- | Changes the ownership of a file.
chown :: forall eff. FilePath
                  -> Int
                  -> Int
                  -> Eff (fs :: FS, exception :: EXCEPTION | eff) Unit

chown file uid gid = mkEff $ \_ -> runFn3
  fs.chownSync file uid gid

-- | Changes the permissions of a file.
chmod :: forall eff. FilePath
                  -> Perms
                  -> Eff (fs :: FS, exception :: EXCEPTION | eff) Unit

chmod file perms = mkEff $ \_ -> runFn2
  fs.chmodSync file (permsToString perms)

-- | Gets file statistics.
stat :: forall eff. FilePath
                 -> Eff (fs :: FS, exception :: EXCEPTION | eff) Stats

stat file = map Stats $ mkEff $ \_ -> runFn1
  fs.statSync file

-- | Creates a link to an existing file.
link :: forall eff. FilePath
                 -> FilePath
                 -> Eff (fs :: FS, exception :: EXCEPTION | eff) Unit

link src dst = mkEff $ \_ -> runFn2
  fs.linkSync src dst

-- | Creates a symlink.
symlink :: forall eff. FilePath
                    -> FilePath
                    -> SymlinkType
                    -> Eff (fs :: FS, exception :: EXCEPTION | eff) Unit

symlink src dst ty = mkEff $ \_ -> runFn3
  fs.symlinkSync src dst (symlinkTypeToNode ty)

-- | Reads the value of a symlink.
readlink :: forall eff. FilePath
                     -> Eff (fs :: FS, exception :: EXCEPTION | eff) FilePath

readlink path = mkEff $ \_ -> runFn1
  fs.readlinkSync path

-- | Find the canonicalized absolute location for a path.
realpath :: forall eff. FilePath
                     -> Eff (fs :: FS, exception :: EXCEPTION | eff) FilePath

realpath path = mkEff $ \_ -> runFn2
  fs.realpathSync path {}

-- | Find the canonicalized absolute location for a path using a cache object for
-- | already resolved paths.
realpath' :: forall eff cache. FilePath
                            -> { | cache }
                            -> Eff (fs :: FS, exception :: EXCEPTION | eff) FilePath

realpath' path cache = mkEff $ \_ -> runFn2
  fs.realpathSync path cache

-- | Deletes a file.
unlink :: forall eff. FilePath
                   -> Eff (fs :: FS, exception :: EXCEPTION | eff) Unit

unlink file = mkEff $ \_ -> runFn1
  fs.unlinkSync file

-- | Deletes a directory.
rmdir :: forall eff. FilePath
                  -> Eff (fs :: FS, exception :: EXCEPTION | eff) Unit

rmdir file = mkEff $ \_ -> runFn1
  fs.rmdirSync file

-- | Makes a new directory.
mkdir :: forall eff. FilePath
                  -> Eff (fs :: FS, exception :: EXCEPTION | eff) Unit

mkdir = flip mkdir' $ mkPerms all all all

-- | Makes a new directory with the specified permissions.
mkdir' :: forall eff. FilePath
                   -> Perms
                   -> Eff (fs :: FS, exception :: EXCEPTION | eff) Unit

mkdir' file perms = mkEff $ \_ -> runFn2
  fs.mkdirSync file (permsToString perms)

-- | Reads the contents of a directory.
readdir :: forall eff. FilePath
                    -> Eff (fs :: FS, exception :: EXCEPTION | eff) (Array FilePath)

readdir file = mkEff $ \_ -> runFn1
  fs.readdirSync file

-- | Sets the accessed and modified times for the specified file.
utimes :: forall eff. FilePath
                   -> DateTime
                   -> DateTime
                   -> Eff (fs :: FS, exception :: EXCEPTION | eff) Unit

utimes file atime mtime = mkEff $ \_ -> runFn3
  fs.utimesSync file
                (fromDate atime)
                (fromDate mtime)
  where
  fromDate date = ms (toEpochMilliseconds date) / 1000
  ms (Milliseconds n) = round n
  toEpochMilliseconds = unInstant <<< fromDateTime

-- | Reads the entire contents of a file returning the result as a raw buffer.
readFile :: forall eff. FilePath
                     -> Eff (fs :: FS, exception :: EXCEPTION | eff) Buffer

readFile file = mkEff $ \_ -> runFn2
  fs.readFileSync file {}

-- | Reads the entire contents of a text file with the specified encoding.
readTextFile :: forall eff. Encoding
                         -> FilePath
                         -> Eff (fs :: FS, exception :: EXCEPTION | eff) String

readTextFile encoding file = mkEff $ \_ -> runFn2
  fs.readFileSync file { encoding: show encoding }

-- | Writes a buffer to a file.
writeFile :: forall eff. FilePath
                      -> Buffer
                      -> Eff (buffer :: BUFFER, fs :: FS, exception :: EXCEPTION | eff) Unit

writeFile file buff = mkEff $ \_ -> runFn3
  fs.writeFileSync file buff {}

-- | Writes text to a file using the specified encoding.
writeTextFile :: forall eff. Encoding
                          -> FilePath
                          -> String
                          -> Eff (fs :: FS, exception :: EXCEPTION | eff) Unit

writeTextFile encoding file text = mkEff $ \_ -> runFn3
  fs.writeFileSync file text { encoding: show encoding }

-- | Appends the contents of a buffer to a file.
appendFile :: forall eff. FilePath
                       -> Buffer
                       -> Eff (buffer :: BUFFER, fs :: FS, exception :: EXCEPTION | eff) Unit

appendFile file buff = mkEff $ \_ -> runFn3
  fs.appendFileSync file buff {}

-- | Appends text to a file using the specified encoding.
appendTextFile :: forall eff. Encoding
                           -> FilePath
                           -> String
                           -> Eff (fs :: FS, exception :: EXCEPTION | eff) Unit

appendTextFile encoding file buff = mkEff $ \_ -> runFn3
  fs.appendFileSync file buff { encoding: show encoding }

-- | Check if the path exists.
exists :: forall eff. FilePath
                   -> Eff (fs :: FS | eff) Boolean
exists file = mkEff $ \_ -> fs.existsSync file

-- | Open a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_opensync_path_flags_mode)
-- | for details.
fdOpen :: forall eff.
          FilePath
       -> FileFlags
       -> Maybe FileMode
       -> Eff (exception :: EXCEPTION, fs :: FS | eff) FileDescriptor
fdOpen file flags mode = mkEff $ \_ ->
  runFn3 fs.openSync file (fileFlagsToNode flags) (toNullable mode)

-- | Read from a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_readsync_fd_buffer_offset_length_position)
-- | for details.
fdRead :: forall eff.
          FileDescriptor
       -> Buffer
       -> BufferOffset
       -> BufferLength
       -> Maybe FilePosition
       -> Eff (buffer :: BUFFER, exception :: EXCEPTION, fs :: FS | eff) ByteCount
fdRead fd buff off len pos =
  mkEff $ \_ -> runFn5 fs.readSync fd buff off len (toNullable pos)

-- | Convenience function to fill the whole buffer from the current
-- | file position.
fdNext :: forall eff.
          FileDescriptor
       -> Buffer
       -> Eff (buffer :: BUFFER, exception :: EXCEPTION, fs :: FS | eff) ByteCount
fdNext fd buff = do
  sz <- size buff
  fdRead fd buff 0 sz Nothing

-- | Write to a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_writesync_fd_buffer_offset_length_position)
-- | for details.
fdWrite :: forall eff.
           FileDescriptor
        -> Buffer
        -> BufferOffset
        -> BufferLength
        -> Maybe FilePosition
        -> Eff (buffer :: BUFFER, exception :: EXCEPTION, fs :: FS | eff) ByteCount
fdWrite fd buff off len pos =
  mkEff $ \_ -> runFn5 fs.writeSync fd buff off len (toNullable pos)

-- | Convenience function to append the whole buffer to the current
-- | file position.
fdAppend :: forall eff.
            FileDescriptor
         -> Buffer
         -> Eff (buffer :: BUFFER, exception :: EXCEPTION, fs :: FS | eff) ByteCount
fdAppend fd buff = do
  sz <- size buff
  fdWrite fd buff 0 sz Nothing

-- | Flush a file synchronously.  See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_fsyncsync_fd)
-- | for details.
fdFlush :: forall eff.
           FileDescriptor
        -> Eff (exception :: EXCEPTION, fs :: FS | eff) Unit
fdFlush fd = mkEff $ \_ -> runFn1 fs.fsyncSync fd

-- | Close a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_closesync_fd)
-- | for details.
fdClose :: forall eff.
           FileDescriptor
        -> Eff (exception :: EXCEPTION, fs :: FS | eff) Unit
fdClose fd = mkEff $ \_ -> runFn1 fs.closeSync fd
