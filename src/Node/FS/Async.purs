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

foreign import renameImpl :: Fn3 FilePath FilePath (JSCallback Unit) Unit
foreign import truncateImpl :: Fn3 FilePath Int (JSCallback Unit) Unit
foreign import chownImpl :: Fn4 FilePath Int Int (JSCallback Unit) Unit
foreign import chmodImpl :: Fn3 FilePath String (JSCallback Unit) Unit
foreign import statImpl :: Fn2 FilePath (JSCallback StatsObj) Unit
foreign import linkImpl :: Fn3 FilePath FilePath (JSCallback Unit) Unit
foreign import symlinkImpl :: Fn4 FilePath FilePath String (JSCallback Unit) Unit
foreign import readlinkImpl :: Fn2 FilePath (JSCallback FilePath) Unit
foreign import realpathImpl :: forall cache. Fn3 FilePath { | cache } (JSCallback FilePath) Unit
foreign import unlinkImpl :: Fn2 FilePath (JSCallback Unit) Unit
foreign import rmdirImpl :: Fn2 FilePath (JSCallback Unit) Unit
foreign import mkdirImpl :: Fn3 FilePath { recursive :: Boolean, mode :: String } (JSCallback Unit) Unit
foreign import readdirImpl :: Fn2 FilePath (JSCallback (Array FilePath)) Unit
foreign import utimesImpl :: Fn4 FilePath Int Int (JSCallback Unit) Unit
foreign import readFileImpl :: forall a opts. Fn3 FilePath { | opts } (JSCallback a) Unit
foreign import writeFileImpl :: forall a opts. Fn4 FilePath a { | opts } (JSCallback Unit) Unit
foreign import appendFileImpl :: forall a opts. Fn4 FilePath a { | opts } (JSCallback Unit) Unit
foreign import openImpl :: Fn4 FilePath String (Nullable FileMode) (JSCallback FileDescriptor) Unit
foreign import readImpl :: Fn6 FileDescriptor Buffer BufferOffset BufferLength (Nullable FilePosition) (JSCallback ByteCount) Unit
foreign import writeImpl :: Fn6 FileDescriptor Buffer BufferOffset BufferLength (Nullable FilePosition) (JSCallback ByteCount) Unit
foreign import closeImpl :: Fn2 FileDescriptor (JSCallback Unit) Unit


-- | Renames a file.
rename :: FilePath
       -> FilePath
       -> Callback Unit
       -> Effect Unit
rename oldFile newFile cb = mkEffect $ \_ -> runFn3
  renameImpl oldFile newFile (handleCallback cb)

-- | Truncates a file to the specified length.
truncate :: FilePath
         -> Int
         -> Callback Unit
         -> Effect Unit

truncate file len cb = mkEffect $ \_ -> runFn3
  truncateImpl file len (handleCallback cb)

-- | Changes the ownership of a file.
chown :: FilePath
      -> Int
      -> Int
      -> Callback Unit
      -> Effect Unit

chown file uid gid cb = mkEffect $ \_ -> runFn4
  chownImpl file uid gid (handleCallback cb)

-- | Changes the permissions of a file.
chmod :: FilePath
      -> Perms
      -> Callback Unit
      -> Effect Unit

chmod file perms cb = mkEffect $ \_ -> runFn3
  chmodImpl file (permsToString perms) (handleCallback cb)

-- | Gets file statistics.
stat :: FilePath
     -> Callback Stats
     -> Effect Unit

stat file cb = mkEffect $ \_ -> runFn2
  statImpl file (handleCallback $ cb <<< (<$>) Stats)

-- | Creates a link to an existing file.
link :: FilePath
     -> FilePath
     -> Callback Unit
     -> Effect Unit

link src dst cb = mkEffect $ \_ -> runFn3
  linkImpl src dst (handleCallback cb)

-- | Creates a symlink.
symlink :: FilePath
        -> FilePath
        -> SymlinkType
        -> Callback Unit
        -> Effect Unit

symlink src dest ty cb = mkEffect $ \_ -> runFn4
  symlinkImpl src dest (symlinkTypeToNode ty) (handleCallback cb)

-- | Reads the value of a symlink.
readlink :: FilePath
         -> Callback FilePath
         -> Effect Unit

readlink path cb = mkEffect $ \_ -> runFn2
  readlinkImpl path (handleCallback cb)

-- | Find the canonicalized absolute location for a path.
realpath :: FilePath
         -> Callback FilePath
         -> Effect Unit

realpath path cb = mkEffect $ \_ -> runFn3
  realpathImpl path {} (handleCallback cb)

-- | Find the canonicalized absolute location for a path using a cache object
-- | for already resolved paths.
realpath' :: forall cache. FilePath
          -> { | cache }
          -> Callback FilePath
          -> Effect Unit

realpath' path cache cb = mkEffect $ \_ -> runFn3
  realpathImpl path cache (handleCallback cb)

-- | Deletes a file.
unlink :: FilePath
       -> Callback Unit
       -> Effect Unit

unlink file cb = mkEffect $ \_ -> runFn2
  unlinkImpl file (handleCallback cb)

-- | Deletes a directory.
rmdir :: FilePath
      -> Callback Unit
      -> Effect Unit

rmdir file cb = mkEffect $ \_ -> runFn2
  rmdirImpl file (handleCallback cb)

-- | Makes a new directory.
mkdir :: FilePath
      -> Callback Unit
      -> Effect Unit
mkdir path = mkdir' path { recursive: false, mode: mkPerms all all all }

-- | Makes a new directory with the specified permissions.
mkdir'
  :: FilePath
  -> { recursive :: Boolean, mode :: Perms }
  -> Callback Unit
  -> Effect Unit
mkdir' file { recursive, mode: perms } cb = mkEffect $ \_ -> runFn3
  mkdirImpl file { recursive, mode: permsToString perms } (handleCallback cb)

-- | Reads the contents of a directory.
readdir :: FilePath
        -> Callback (Array FilePath)
        -> Effect Unit

readdir file cb = mkEffect $ \_ -> runFn2
  readdirImpl file (handleCallback cb)

-- | Sets the accessed and modified times for the specified file.
utimes :: FilePath
       -> DateTime
       -> DateTime
       -> Callback Unit
       -> Effect Unit

utimes file atime mtime cb = mkEffect $ \_ -> runFn4
  utimesImpl file
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
  readFileImpl file {} (handleCallback cb)

-- | Reads the entire contents of a text file with the specified encoding.
readTextFile ::  Encoding
             -> FilePath
             -> Callback String
             -> Effect Unit

readTextFile encoding file cb = mkEffect $ \_ -> runFn3
  readFileImpl file { encoding: show encoding } (handleCallback cb)

-- | Writes a buffer to a file.
writeFile :: FilePath
          -> Buffer
          -> Callback Unit
          -> Effect Unit

writeFile file buff cb = mkEffect $ \_ -> runFn4
  writeFileImpl file buff {} (handleCallback cb)

-- | Writes text to a file using the specified encoding.
writeTextFile ::  Encoding
              -> FilePath
              -> String
              -> Callback Unit
              -> Effect Unit

writeTextFile encoding file buff cb = mkEffect $ \_ -> runFn4
  writeFileImpl file buff { encoding: show encoding } (handleCallback cb)

-- | Appends the contents of a buffer to a file.
appendFile :: FilePath
           -> Buffer
           -> Callback Unit
           -> Effect Unit

appendFile file buff cb = mkEffect $ \_ -> runFn4
  appendFileImpl file buff {} (handleCallback cb)

-- | Appends text to a file using the specified encoding.
appendTextFile ::  Encoding
               -> FilePath
               -> String
               -> Callback Unit
               -> Effect Unit

appendTextFile encoding file buff cb = mkEffect $ \_ -> runFn4
  appendFileImpl file buff { encoding: show encoding } (handleCallback cb)


-- | Open a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_open_path_flags_mode_callback)
-- | for details.
fdOpen :: FilePath
       -> FileFlags
       -> Maybe FileMode
       -> Callback FileDescriptor
       -> Effect Unit
fdOpen file flags mode cb = mkEffect $ \_ -> runFn4 openImpl file (fileFlagsToNode flags) (toNullable mode) (handleCallback cb)

-- | Read from a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_read_fd_buffer_offset_length_position_callback)
-- | for details.
fdRead :: FileDescriptor
       -> Buffer
       -> BufferOffset
       -> BufferLength
       -> Maybe FilePosition
       -> Callback ByteCount
       -> Effect Unit
fdRead fd buff off len pos cb =  mkEffect $ \_ -> runFn6 readImpl fd buff off len (toNullable pos) (handleCallback cb)

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
fdWrite fd buff off len pos cb = mkEffect $ \_ -> runFn6 writeImpl fd buff off len (toNullable pos) (handleCallback cb)

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
fdClose fd cb = mkEffect $ \_ -> runFn2 closeImpl fd (handleCallback cb)
