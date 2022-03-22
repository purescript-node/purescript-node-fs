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
  , mkdirRecursive
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
  , fdClose
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn6, Fn4, Fn3, runFn2, runFn6, runFn4, runFn3)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Unsafe (unsafePerformEffect)
import Node.Buffer (Buffer, size)
import Node.Encoding (Encoding)
import Node.FS (FileDescriptor, ByteCount, FilePosition, BufferLength, BufferOffset, FileMode, FileFlags, SymlinkType, fileFlagsToNode, symlinkTypeToNode)
import Node.FS.Internal (mkEffect)
import Node.FS.Perms (Perms, permsToString, all, mkPerms)
import Node.FS.Stats (StatsObj, Stats(..))
import Node.Path (FilePath)

type JSCallback a = Fn2 (Nullable Error) a Unit

foreign import handleCallbackImpl ::
  forall a. Fn3 (Error -> Either Error a)
                (a -> Either Error a)
                (Callback a)
                (JSCallback a)

handleCallback :: forall a. (Callback a) -> JSCallback a
handleCallback cb = runFn3 handleCallbackImpl Left Right cb


-- | Type synonym for callback functions.
type Callback a = Either Error a -> Effect Unit

foreign import rename_ :: Fn3 FilePath FilePath (JSCallback Unit) Unit
foreign import truncate_ :: Fn3 FilePath Int (JSCallback Unit) Unit
foreign import chown_ :: Fn4 FilePath Int Int (JSCallback Unit) Unit
foreign import chmod_ :: Fn3 FilePath String (JSCallback Unit) Unit
foreign import stat_ :: Fn2 FilePath (JSCallback StatsObj) Unit
foreign import link_ :: Fn3 FilePath FilePath (JSCallback Unit) Unit
foreign import symlink_ :: Fn4 FilePath FilePath String (JSCallback Unit) Unit
foreign import readlink_ :: Fn2 FilePath (JSCallback FilePath) Unit
foreign import realpath_ :: forall cache. Fn3 FilePath { | cache } (JSCallback FilePath) Unit
foreign import unlink_ :: Fn2 FilePath (JSCallback Unit) Unit
foreign import rmdir_ :: Fn2 FilePath (JSCallback Unit) Unit
foreign import mkdir_ :: Fn3 FilePath { recursive :: Boolean, mode :: String } (JSCallback Unit) Unit
foreign import readdir_ :: Fn2 FilePath (JSCallback (Array FilePath)) Unit
foreign import utimes_ :: Fn4 FilePath Int Int (JSCallback Unit) Unit
foreign import readFile_ :: forall a opts. Fn3 FilePath { | opts } (JSCallback a) Unit
foreign import writeFile_ :: forall a opts. Fn4 FilePath a { | opts } (JSCallback Unit) Unit
foreign import appendFile_ :: forall a opts. Fn4 FilePath a { | opts } (JSCallback Unit) Unit
foreign import exists_ :: forall a. Fn2 FilePath (Boolean -> a) Unit
foreign import open_ :: Fn4 FilePath String (Nullable FileMode) (JSCallback FileDescriptor) Unit
foreign import read_ :: Fn6 FileDescriptor Buffer BufferOffset BufferLength (Nullable FilePosition) (JSCallback ByteCount) Unit
foreign import write_ :: Fn6 FileDescriptor Buffer BufferOffset BufferLength (Nullable FilePosition) (JSCallback ByteCount) Unit
foreign import close_ :: Fn2 FileDescriptor (JSCallback Unit) Unit


-- | Renames a file.
rename :: FilePath
       -> FilePath
       -> Callback Unit
       -> Effect Unit
rename oldFile newFile cb = mkEffect $ \_ -> runFn3
  rename_ oldFile newFile (handleCallback cb)

-- | Truncates a file to the specified length.
truncate :: FilePath
         -> Int
         -> Callback Unit
         -> Effect Unit

truncate file len cb = mkEffect $ \_ -> runFn3
  truncate_ file len (handleCallback cb)

-- | Changes the ownership of a file.
chown :: FilePath
      -> Int
      -> Int
      -> Callback Unit
      -> Effect Unit

chown file uid gid cb = mkEffect $ \_ -> runFn4
  chown_ file uid gid (handleCallback cb)

-- | Changes the permissions of a file.
chmod :: FilePath
      -> Perms
      -> Callback Unit
      -> Effect Unit

chmod file perms cb = mkEffect $ \_ -> runFn3
  chmod_ file (permsToString perms) (handleCallback cb)

-- | Gets file statistics.
stat :: FilePath
     -> Callback Stats
     -> Effect Unit

stat file cb = mkEffect $ \_ -> runFn2
  stat_ file (handleCallback $ cb <<< (<$>) Stats)

-- | Creates a link to an existing file.
link :: FilePath
     -> FilePath
     -> Callback Unit
     -> Effect Unit

link src dst cb = mkEffect $ \_ -> runFn3
  link_ src dst (handleCallback cb)

-- | Creates a symlink.
symlink :: FilePath
        -> FilePath
        -> SymlinkType
        -> Callback Unit
        -> Effect Unit

symlink src dest ty cb = mkEffect $ \_ -> runFn4
  symlink_ src dest (symlinkTypeToNode ty) (handleCallback cb)

-- | Reads the value of a symlink.
readlink :: FilePath
         -> Callback FilePath
         -> Effect Unit

readlink path cb = mkEffect $ \_ -> runFn2
  readlink_ path (handleCallback cb)

-- | Find the canonicalized absolute location for a path.
realpath :: FilePath
         -> Callback FilePath
         -> Effect Unit

realpath path cb = mkEffect $ \_ -> runFn3
  realpath_ path {} (handleCallback cb)

-- | Find the canonicalized absolute location for a path using a cache object
-- | for already resolved paths.
realpath' :: forall cache. FilePath
          -> { | cache }
          -> Callback FilePath
          -> Effect Unit

realpath' path cache cb = mkEffect $ \_ -> runFn3
  realpath_ path cache (handleCallback cb)

-- | Deletes a file.
unlink :: FilePath
       -> Callback Unit
       -> Effect Unit

unlink file cb = mkEffect $ \_ -> runFn2
  unlink_ file (handleCallback cb)

-- | Deletes a directory.
rmdir :: FilePath
      -> Callback Unit
      -> Effect Unit

rmdir file cb = mkEffect $ \_ -> runFn2
  rmdir_ file (handleCallback cb)

-- | Makes a new directory.
mkdir :: FilePath
      -> Callback Unit
      -> Effect Unit

mkdir path = mkdir' path (mkPerms all all all)

-- | Makes a new directory and any directories that don't exist
-- | in the path. Similar to `mkdir -p`.
mkdirRecursive :: FilePath
               -> Callback Unit
               -> Effect Unit

mkdirRecursive path = mkdirRecursive' path (mkPerms all all all)

-- | Makes a new directory (and any directories that don't exist
-- | in the path) with the specified permissions.
mkdirRecursive'
  :: FilePath
  -> Perms
  -> Callback Unit
  -> Effect Unit
mkdirRecursive' file perms cb = mkEffect $ \_ -> runFn3
  mkdir_ file { recursive: true, mode: permsToString perms } (handleCallback cb)

-- | Makes a new directory with the specified permissions.
mkdir' :: FilePath
       -> Perms
       -> Callback Unit
       -> Effect Unit
mkdir' file perms cb = mkEffect $ \_ -> runFn3
  mkdir_ file { recursive: false, mode: permsToString perms } (handleCallback cb)

-- | Reads the contents of a directory.
readdir :: FilePath
        -> Callback (Array FilePath)
        -> Effect Unit

readdir file cb = mkEffect $ \_ -> runFn2
  readdir_ file (handleCallback cb)

-- | Sets the accessed and modified times for the specified file.
utimes :: FilePath
       -> DateTime
       -> DateTime
       -> Callback Unit
       -> Effect Unit

utimes file atime mtime cb = mkEffect $ \_ -> runFn4
  utimes_ file
            (fromDate atime)
            (fromDate mtime)
            (handleCallback cb)
  where
  fromDate date = ms (toEpochMilliseconds date) / 1000
  ms (Milliseconds n) = round n
  toEpochMilliseconds = unInstant <<< fromDateTime

-- | Reads the entire contents of a file returning the result as a raw buffer.
readFile :: FilePath
         -> Callback Buffer
         -> Effect Unit

readFile file cb = mkEffect $ \_ -> runFn3
  readFile_ file {} (handleCallback cb)

-- | Reads the entire contents of a text file with the specified encoding.
readTextFile ::  Encoding
             -> FilePath
             -> Callback String
             -> Effect Unit

readTextFile encoding file cb = mkEffect $ \_ -> runFn3
  readFile_ file { encoding: show encoding } (handleCallback cb)

-- | Writes a buffer to a file.
writeFile :: FilePath
          -> Buffer
          -> Callback Unit
          -> Effect Unit

writeFile file buff cb = mkEffect $ \_ -> runFn4
  writeFile_ file buff {} (handleCallback cb)

-- | Writes text to a file using the specified encoding.
writeTextFile ::  Encoding
              -> FilePath
              -> String
              -> Callback Unit
              -> Effect Unit

writeTextFile encoding file buff cb = mkEffect $ \_ -> runFn4
  writeFile_ file buff { encoding: show encoding } (handleCallback cb)

-- | Appends the contents of a buffer to a file.
appendFile :: FilePath
           -> Buffer
           -> Callback Unit
           -> Effect Unit

appendFile file buff cb = mkEffect $ \_ -> runFn4
  appendFile_ file buff {} (handleCallback cb)

-- | Appends text to a file using the specified encoding.
appendTextFile ::  Encoding
               -> FilePath
               -> String
               -> Callback Unit
               -> Effect Unit

appendTextFile encoding file buff cb = mkEffect $ \_ -> runFn4
  appendFile_ file buff { encoding: show encoding } (handleCallback cb)

-- | Check if the path exists.
exists :: FilePath
       -> (Boolean -> Effect Unit)
       -> Effect Unit
exists file cb = mkEffect $ \_ -> runFn2
  exists_ file $ \b -> unsafePerformEffect (cb b)

-- | Open a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_open_path_flags_mode_callback)
-- | for details.
fdOpen :: FilePath
       -> FileFlags
       -> Maybe FileMode
       -> Callback FileDescriptor
       -> Effect Unit
fdOpen file flags mode cb = mkEffect $ \_ -> runFn4 open_ file (fileFlagsToNode flags) (toNullable mode) (handleCallback cb)

-- | Read from a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_read_fd_buffer_offset_length_position_callback)
-- | for details.
fdRead :: FileDescriptor
       -> Buffer
       -> BufferOffset
       -> BufferLength
       -> Maybe FilePosition
       -> Callback ByteCount
       -> Effect Unit
fdRead fd buff off len pos cb =  mkEffect $ \_ -> runFn6 read_ fd buff off len (toNullable pos) (handleCallback cb)

-- | Convenience function to fill the whole buffer from the current
-- | file position.
fdNext :: FileDescriptor
       -> Buffer
       -> Callback ByteCount
       -> Effect Unit
fdNext fd buff cb = do
  sz <- size buff
  fdRead fd buff 0 sz Nothing cb

-- | Write to a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_write_fd_buffer_offset_length_position_callback)
-- | for details.
fdWrite :: FileDescriptor
        -> Buffer
        -> BufferOffset
        -> BufferLength
        -> Maybe FilePosition
        -> Callback ByteCount
        -> Effect Unit
fdWrite fd buff off len pos cb = mkEffect $ \_ -> runFn6 write_ fd buff off len (toNullable pos) (handleCallback cb)

-- | Convenience function to append the whole buffer to the current
-- | file position.
fdAppend :: FileDescriptor
         -> Buffer
         -> Callback ByteCount
         -> Effect Unit
fdAppend fd buff cb = do
  sz <- size buff
  fdWrite fd buff 0 sz Nothing cb

-- | Close a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_close_fd_callback)
-- | for details.
fdClose :: FileDescriptor
        -> Callback Unit
        -> Effect Unit
fdClose fd cb = mkEffect $ \_ -> runFn2 close_ fd (handleCallback cb)
