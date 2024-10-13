module Node.FS.Sync
  ( access
  , access'
  , copyFile
  , copyFile'
  , mkdtemp
  , mkdtemp'
  , rename
  , truncate
  , chown
  , chmod
  , lstat
  , stat
  , link
  , symlink
  , readlink
  , readlinkBuffer
  , realpath
  , realpath'
  , unlink
  , rmdir
  , rmdir'
  , rm
  , rm'
  , mkdir
  , mkdir'
  , readdir
  , readdir'
  , readdirBuffer
  , readdirBuffer'
  , readdirDirent
  , readdirDirent'
  , readdirDirentBuffer
  , readdirDirentBuffer'
  , utimes
  , readFile
  , readFile'
  , readTextFile
  , readTextFile'
  , writeFile
  , writeFile'
  , writeTextFile
  , writeTextFile'
  , appendFile
  , appendFile'
  , appendTextFile
  , exists
  , fdOpen
  , fdRead
  , fdRead'
  , fdNext
  , fdWrite
  , fdWrite'
  , fdWriteString
  , fdAppend
  , fdClose
  , cpFile
  , cpFile'
  , cpDir
  , cpDir'
  , fchmod
  , fchown
  , fdatasync
  , fstat
  , fsync
  , ftruncate
  , futimes
  , glob
  , glob'
  , globDirent
  , globDirent'
  , lchmod
  , lchown
  , lutimes
  -- , openAsBlob
  , opendir
  , opendir'
  , readv
  , statfs
  -- , unwatchFile
  -- , watch
  -- , watchFile
  , writev
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.Either (blush)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Exception (Error, try)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5)
import Node.Buffer (Buffer, size)
import Node.Encoding (Encoding(..), encodingToNode)
import Node.FS.Constants (AccessMode, CopyMode, FileFlags, defaultAccessMode, defaultCopyMode, fileFlagsToNode)
import Node.FS.Dir (Dir)
import Node.FS.Dirent (Dirent, DirentNameBuffer, DirentNameString)
import Node.FS.Internal.Utils (datetimeToUnixEpochTimeInSeconds)
import Node.FS.Options (AppendFileBufferOptions, AppendFileOptionsInternal, AppendFileStringOptions, CpDirOptions, CpFileOptions, CpOptionsInternal, FdReadOptions, FdReadOptionsInternal, FdWriteOptions, FdWriteOptionsInternal, GlobDirentOptions, GlobFilePathOptions, GlobOptionsInternal, MkdirOptions, MkdirOptionsInternal, OpendirOptions, OpendirOptionsInternal, ReadFileBufferOptions, ReadFileOptionsInternal, ReadFileStringOptions, ReaddirBufferOptions, ReaddirDirentBufferOptions, ReaddirDirentOptions, ReaddirFilePathOptions, ReaddirOptionsInternal, RealpathOptions, RealpathOptionsInternal, RmOptions, RmdirOptions, WriteFileBufferOptions, WriteFileOptionsInternal, WriteFileStringOptions, appendFileBufferOptionsDefault, appendFileBufferOptionsToInternal, appendFileStringOptionsDefault, appendFileStringOptionsToInternal, cpDirOptionsDefault, cpDirOptionsToCpOptionsInternal, cpFileOptionsDefault, cpFileOptionsToCpOptionsInternal, fdReadOptionsToInternal, fdWriteOptionsToInternal, globDirentOptionsDefault, globDirentOptionsToInternal, globFilePathOptionsDefault, globFilePathOptionsToInternal, mkdirOptionsDefault, mkdirOptionsToInternal, opendirOptionsDefault, opendirOptionsToInternal, readFileBufferOptionsDefault, readFileBufferOptionsToInternal, readFileStringOptionsDefault, readFileStringOptionsToInternal, readdirBufferOptionsDefault, readdirBufferOptionsToInternal, readdirDirentBufferOptionsDefault, readdirDirentBufferOptionsToInternal, readdirDirentOptionsDefault, readdirDirentOptionsToInternal, readdirFilePathOptionsDefault, readdirFilePathOptionsToInternal, realpathOptionsDefault, realpathOptionsToInternal, rmOptionsDefault, rmdirOptionsDefault, writeFileBufferOptionsDefault, writeFileBufferOptionsToInternal, writeFileStringOptionsDefault, writeFileStringOptionsToInternal)
import Node.FS.Perms (Perms, permsToString)
import Node.FS.Stats (Stats)
import Node.FS.Types (BufferLength, BufferOffset, ByteCount, FileDescriptor, FileMode, FilePosition, SymlinkType, EncodingString, symlinkTypeToNode)
import Node.Path (FilePath)
import Unsafe.Coerce (unsafeCoerce)

foreign import accessSyncImpl :: EffectFn2 FilePath AccessMode Unit
foreign import copyFileSyncImpl :: EffectFn3 FilePath FilePath CopyMode Unit
foreign import mkdtempSyncImpl :: EffectFn2 FilePath FilePath FilePath
foreign import renameSyncImpl :: EffectFn2 FilePath FilePath Unit
foreign import truncateSyncImpl :: EffectFn2 FilePath Int Unit
foreign import chownSyncImpl :: EffectFn3 FilePath Int Int Unit
foreign import chmodSyncImpl :: EffectFn2 FilePath String Unit
foreign import statSyncImpl :: EffectFn1 FilePath Stats
foreign import lstatSyncImpl :: EffectFn1 FilePath Stats
foreign import linkSyncImpl :: EffectFn2 FilePath FilePath Unit
foreign import symlinkSyncImpl :: EffectFn3 FilePath FilePath (Nullable String) Unit
foreign import readlinkSyncImpl :: EffectFn1 FilePath FilePath

readlinkBufferSyncImpl :: EffectFn2 FilePath String Buffer
readlinkBufferSyncImpl = unsafeCoerce readlinkSyncImpl

foreign import realpathSyncImpl :: EffectFn2 FilePath RealpathOptionsInternal FilePath
foreign import unlinkSyncImpl :: EffectFn1 FilePath Unit
foreign import rmdirSyncImpl :: EffectFn2 FilePath RmdirOptions Unit
foreign import rmSyncImpl :: EffectFn2 FilePath RmOptions Unit
foreign import mkdirSyncImpl :: EffectFn2 FilePath MkdirOptionsInternal Unit
foreign import readdirSyncImpl :: forall filepathOrDirentOrBuffer. EffectFn2 FilePath ReaddirOptionsInternal (Array filepathOrDirentOrBuffer)
foreign import utimesSyncImpl :: EffectFn3 FilePath Int Int Unit
foreign import readFileSyncImpl :: forall stringOrBuffer. EffectFn2 FilePath ReadFileOptionsInternal stringOrBuffer
foreign import writeFileSyncImpl :: forall stringOrBuffer. EffectFn3 FilePath stringOrBuffer WriteFileOptionsInternal Unit
foreign import appendFileSyncImpl :: forall stringOrBuffer. EffectFn3 FilePath stringOrBuffer AppendFileOptionsInternal Unit
foreign import existsSyncImpl :: EffectFn1 FilePath Boolean
foreign import openSyncImpl :: EffectFn3 FilePath String (Nullable FileMode) FileDescriptor
foreign import readSyncImpl :: EffectFn5 FileDescriptor Buffer BufferOffset BufferLength (Nullable FilePosition) ByteCount

-- https://nodejs.org/docs/latest/api/fs.html#fsreadfd-options-callback
readWithOptionsSyncImpl :: EffectFn2 FileDescriptor FdReadOptionsInternal ByteCount
readWithOptionsSyncImpl = unsafeCoerce readSyncImpl

foreign import writeSyncImpl :: EffectFn5 FileDescriptor Buffer BufferOffset BufferLength (Nullable FilePosition) ByteCount

-- https://nodejs.org/docs/latest/api/fs.html#fsreadfd-options-callback
writeBufferWithOptionsSyncImpl :: EffectFn3 FileDescriptor Buffer FdWriteOptionsInternal ByteCount
writeBufferWithOptionsSyncImpl = unsafeCoerce writeSyncImpl

-- https://nodejs.org/docs/latest/api/fs.html#fsreadfd-options-callback
writeStringSyncImpl :: EffectFn4 FileDescriptor String (Nullable FilePosition) EncodingString ByteCount
writeStringSyncImpl = unsafeCoerce writeSyncImpl

foreign import closeSyncImpl :: EffectFn1 FileDescriptor Unit
foreign import cpSyncImpl :: EffectFn3 FilePath FilePath CpOptionsInternal Unit
foreign import fchmodSyncImpl :: EffectFn2 FileDescriptor String Unit
foreign import fchownSyncImpl :: EffectFn3 FileDescriptor Int Int Unit
foreign import fdatasyncSyncImpl :: EffectFn1 FileDescriptor Unit
foreign import fstatSyncImpl :: EffectFn1 FileDescriptor Stats
foreign import fsyncSyncImpl :: EffectFn1 FileDescriptor Unit
foreign import ftruncateSyncImpl :: EffectFn2 FileDescriptor Int Unit
foreign import futimesSyncImpl :: EffectFn3 FileDescriptor Int Int Unit
foreign import globSyncImpl :: forall filepathOrDirent. EffectFn2 (Array FilePath) (GlobOptionsInternal filepathOrDirent) (Array filepathOrDirent)
foreign import lchmodSyncImpl :: EffectFn2 FilePath String Unit
foreign import lchownSyncImpl :: EffectFn3 FilePath Int Int Unit
foreign import lutimesSyncImpl :: EffectFn3 FilePath Int Int Unit
foreign import opendirSyncImpl :: EffectFn2 FilePath OpendirOptionsInternal Dir
foreign import readvSyncImpl :: EffectFn3 FileDescriptor (Array Buffer) (Nullable FilePosition) ByteCount
foreign import statfsSyncImpl :: EffectFn1 FilePath Stats
foreign import writevSyncImpl :: EffectFn3 FileDescriptor (Array Buffer) (Nullable FilePosition) ByteCount

access :: FilePath -> Effect (Maybe Error)
access = flip access' defaultAccessMode

access' :: FilePath -> AccessMode -> Effect (Maybe Error)
access' path mode = map blush $ try $ runEffectFn2 accessSyncImpl path mode

copyFile :: FilePath -> FilePath -> Effect Unit
copyFile src dest = runEffectFn3 copyFileSyncImpl src dest defaultCopyMode

copyFile' :: FilePath -> FilePath -> CopyMode -> Effect Unit
copyFile' src dest mode = runEffectFn3 copyFileSyncImpl src dest mode

mkdtemp :: String -> Effect String
mkdtemp prefix = mkdtemp' prefix UTF8

mkdtemp' :: String -> Encoding -> Effect String
mkdtemp' prefix encoding = runEffectFn2 mkdtempSyncImpl prefix (encodingToNode encoding)

-- | Renames a file.
rename :: FilePath -> FilePath -> Effect Unit
rename oldFile newFile = runEffectFn2 renameSyncImpl oldFile newFile

-- | Truncates a file to the specified length.
truncate
  :: FilePath
  -> Int
  -> Effect Unit
truncate file len = runEffectFn2 truncateSyncImpl file len

-- | Changes the ownership of a file.
chown
  :: FilePath
  -> Int
  -> Int
  -> Effect Unit
chown file uid gid = runEffectFn3 chownSyncImpl file uid gid

-- | Changes the permissions of a file.
chmod
  :: FilePath
  -> Perms
  -> Effect Unit
chmod file perms = runEffectFn2 chmodSyncImpl file (permsToString perms)

-- | Gets file statistics.
stat
  :: FilePath
  -> Effect Stats
stat file = runEffectFn1 statSyncImpl file

-- | Gets file or symlink statistics. `lstat` is identical to `stat`, except
-- | that if the `FilePath` is a symbolic link, then the link itself is stat-ed,
-- | not the file that it refers to.
lstat
  :: FilePath
  -> Effect Stats
lstat file = runEffectFn1 lstatSyncImpl file

-- | Creates a link to an existing file.
link
  :: FilePath
  -> FilePath
  -> Effect Unit
link src dst = runEffectFn2 linkSyncImpl src dst

-- | Creates a symlink.
symlink
  :: FilePath
  -> FilePath
  -> SymlinkType
  -> Effect Unit
symlink src dst ty = runEffectFn3 symlinkSyncImpl src dst (symlinkTypeToNode ty)

-- | Reads the value of a symlink.
readlink
  :: FilePath
  -> Effect FilePath
readlink path = runEffectFn1 readlinkSyncImpl path

readlinkBuffer :: FilePath -> Effect Buffer
readlinkBuffer path = runEffectFn2 readlinkBufferSyncImpl path "buffer"

-- | Find the canonicalized absolute location for a path.
realpath
  :: FilePath
  -> Effect FilePath
realpath path = realpath' path realpathOptionsDefault

-- | Find the canonicalized absolute location for a path using a cache object for
-- | already resolved paths.
realpath'
  :: FilePath
  -> RealpathOptions
  -> Effect FilePath
realpath' path options = runEffectFn2 realpathSyncImpl path (realpathOptionsToInternal options)

-- | Deletes a file.
unlink
  :: FilePath
  -> Effect Unit
unlink file = runEffectFn1 unlinkSyncImpl file

-- | Deletes a directory.
rmdir
  :: FilePath
  -> Effect Unit
rmdir path = rmdir' path rmdirOptionsDefault

-- | Deletes a directory with options.
rmdir'
  :: FilePath
  -> RmdirOptions
  -> Effect Unit
rmdir' path opts = runEffectFn2 rmdirSyncImpl path opts

-- | Deletes a file or directory.
rm
  :: FilePath
  -> Effect Unit
rm path = rm' path rmOptionsDefault

-- | Deletes a file or directory with options.
rm'
  :: FilePath
  -> RmOptions
  -> Effect Unit
rm' path opts = runEffectFn2 rmSyncImpl path opts

-- | Makes a new directory.
mkdir
  :: FilePath
  -> Effect Unit
mkdir path = mkdir' path mkdirOptionsDefault

-- | Makes a new directory with the specified permissions.
mkdir'
  :: FilePath
  -> MkdirOptions
  -> Effect Unit
mkdir' file opts = runEffectFn2 mkdirSyncImpl file (mkdirOptionsToInternal opts)

-- | Reads the contents of a directory.
readdir
  :: FilePath
  -> Effect (Array FilePath)
readdir file = readdir' file readdirFilePathOptionsDefault

-- | Reads the contents of a directory.
readdir'
  :: FilePath
  -> ReaddirFilePathOptions
  -> Effect (Array FilePath)
readdir' file options = runEffectFn2 readdirSyncImpl file (readdirFilePathOptionsToInternal options)

-- | Reads the contents of a directory.
readdirBuffer
  :: FilePath
  -> Effect (Array Buffer)
readdirBuffer file = readdirBuffer' file readdirBufferOptionsDefault

-- | Reads the contents of a directory.
readdirBuffer'
  :: FilePath
  -> ReaddirBufferOptions
  -> Effect (Array Buffer)
readdirBuffer' file options = runEffectFn2 readdirSyncImpl file (readdirBufferOptionsToInternal options)

-- | Reads the contents of a directory.
readdirDirent
  :: FilePath
  -> Effect (Array (Dirent DirentNameString))
readdirDirent file = readdirDirent' file readdirDirentOptionsDefault

-- | Reads the contents of a directory.
readdirDirent'
  :: FilePath
  -> ReaddirDirentOptions
  -> Effect (Array (Dirent DirentNameString))
readdirDirent' file options = runEffectFn2 readdirSyncImpl file (readdirDirentOptionsToInternal options)

-- | Reads the contents of a directory.
readdirDirentBuffer
  :: FilePath
  -> Effect (Array (Dirent DirentNameBuffer))
readdirDirentBuffer file = readdirDirentBuffer' file readdirDirentBufferOptionsDefault

-- | Reads the contents of a directory.
readdirDirentBuffer'
  :: FilePath
  -> ReaddirDirentBufferOptions
  -> Effect (Array (Dirent DirentNameBuffer))
readdirDirentBuffer' file options = runEffectFn2 readdirSyncImpl file (readdirDirentBufferOptionsToInternal options)

-- | Sets the accessed and modified times for the specified file.
utimes
  :: FilePath
  -> DateTime
  -> DateTime
  -> Effect Unit
utimes file atime mtime = runEffectFn3 utimesSyncImpl file (datetimeToUnixEpochTimeInSeconds atime) (datetimeToUnixEpochTimeInSeconds mtime)

-- | Reads the entire contents of a text file with the specified encoding.
readTextFile
  :: Encoding
  -> FilePath
  -> Effect String
readTextFile encoding file = readTextFile' file (readFileStringOptionsDefault { encoding = encoding })

readTextFile'
  :: FilePath
  -> ReadFileStringOptions
  -> Effect String
readTextFile' file options = runEffectFn2 readFileSyncImpl file (readFileStringOptionsToInternal options)

-- | Reads the entire contents of a file returning the result as a raw buffer.
readFile
  :: FilePath
  -> Effect Buffer
readFile file = readFile' file readFileBufferOptionsDefault

readFile'
  :: FilePath
  -> ReadFileBufferOptions
  -> Effect Buffer
readFile' file options = runEffectFn2 readFileSyncImpl file (readFileBufferOptionsToInternal options)

-- | Writes text to a file using the specified encoding.
writeTextFile
  :: Encoding
  -> FilePath
  -> String
  -> Effect Unit
writeTextFile encoding file buff = writeTextFile' file buff (writeFileStringOptionsDefault { encoding = encoding })

writeTextFile'
  :: FilePath
  -> String
  -> WriteFileStringOptions
  -> Effect Unit
writeTextFile' file buff options = runEffectFn3 writeFileSyncImpl file buff (writeFileStringOptionsToInternal options)

-- | Writes a buffer to a file.
writeFile
  :: FilePath
  -> Buffer
  -> Effect Unit
writeFile file buff = writeFile' file buff writeFileBufferOptionsDefault

writeFile'
  :: FilePath
  -> Buffer
  -> WriteFileBufferOptions
  -> Effect Unit
writeFile' file buff options = runEffectFn3 writeFileSyncImpl file buff (writeFileBufferOptionsToInternal options)

-- | Appends text to a file using the specified encoding.
appendTextFile
  :: Encoding
  -> FilePath
  -> String
  -> Effect Unit
appendTextFile encoding file buff = appendTextFile' file buff (appendFileStringOptionsDefault { encoding = encoding })

appendTextFile'
  :: FilePath
  -> String
  -> AppendFileStringOptions
  -> Effect Unit
appendTextFile' file buff options = runEffectFn3 appendFileSyncImpl file buff (appendFileStringOptionsToInternal options)

-- | Appends a buffer to a file.
appendFile
  :: FilePath
  -> Buffer
  -> Effect Unit
appendFile file buff = appendFile' file buff appendFileBufferOptionsDefault

appendFile'
  :: FilePath
  -> Buffer
  -> AppendFileBufferOptions
  -> Effect Unit
appendFile' file buff options = runEffectFn3 appendFileSyncImpl file buff (appendFileBufferOptionsToInternal options)

-- | Check if the path exists.
exists :: FilePath -> Effect Boolean
exists file = runEffectFn1 existsSyncImpl file

-- | Open a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_opensync_path_flags_mode)
-- | for details.
fdOpen
  :: FilePath
  -> FileFlags
  -> Maybe FileMode
  -> Effect FileDescriptor
fdOpen file flags mode = runEffectFn3 openSyncImpl file (fileFlagsToNode flags) (toNullable mode)

-- | Read from a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_readsync_fd_buffer_offset_length_position)
-- | for details.
fdRead
  :: FileDescriptor
  -> Buffer
  -> BufferOffset
  -> BufferLength
  -> Maybe FilePosition
  -> Effect ByteCount
fdRead fd buff off len pos =
  runEffectFn5 readSyncImpl fd buff off len (toNullable pos)

-- | Read from a file synchronously. See the [Node Documentation](https://nodejs.org/docs/latest/api/fs.html#fsreadsyncfd-buffer-offset-length-position)
-- | for details.
fdRead'
  :: FileDescriptor
  -> FdReadOptions
  -> Effect ByteCount
fdRead' fd options = runEffectFn2 readWithOptionsSyncImpl fd (fdReadOptionsToInternal options)

-- | Convenience function to fill the whole buffer from the current
-- | file position.
fdNext
  :: FileDescriptor
  -> Buffer
  -> Effect ByteCount
fdNext fd buff = do
  sz <- size buff
  fdRead fd buff 0 sz Nothing

-- | Write to a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_writesync_fd_buffer_offset_length_position)
-- | for details.
fdWrite
  :: FileDescriptor
  -> Buffer
  -> BufferOffset
  -> BufferLength
  -> Maybe FilePosition
  -> Effect ByteCount
fdWrite fd buff off len pos =
  runEffectFn5 writeSyncImpl fd buff off len (toNullable pos)

-- | Write from a file synchronously. See the [Node Documentation](https://nodejs.org/docs/latest/api/fs.html#fswritefd-options-callback)
-- | for details.
fdWrite'
  :: FileDescriptor
  -> Buffer
  -> FdWriteOptions
  -> Effect ByteCount
fdWrite' fd buffer options = runEffectFn3 writeBufferWithOptionsSyncImpl fd buffer (fdWriteOptionsToInternal options)

-- It is unsafe to use fs.write() multiple times on the same file without waiting for the callback. For this scenario, fs.createWriteStream() is recommended.
fdWriteString
  :: FileDescriptor
  -> String
  -> Maybe FilePosition
  -> Encoding
  -> Effect ByteCount
fdWriteString fd string pos encoding = runEffectFn4 writeStringSyncImpl fd string (toNullable pos) (encodingToNode encoding)

-- | Convenience function to append the whole buffer to the current
-- | file position.
fdAppend
  :: FileDescriptor
  -> Buffer
  -> Effect ByteCount
fdAppend fd buff = do
  sz <- size buff
  fdWrite fd buff 0 sz Nothing

-- | Close a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_closesync_fd)
-- | for details.
fdClose :: FileDescriptor -> Effect Unit
fdClose fd = runEffectFn1 closeSyncImpl fd

-- | Copy a file synchronously using a `cp` command.
-- | See the [Node Documentation](https://nodejs.org/api/fs.html#fscpsyncsrc-dest-options)
-- | for details.
cpFile :: FilePath -> FilePath -> Effect Unit
cpFile src dest = cpFile' src dest cpFileOptionsDefault

cpFile' :: FilePath -> FilePath -> CpFileOptions -> Effect Unit
cpFile' src dest opts = runEffectFn3 cpSyncImpl src dest (cpFileOptionsToCpOptionsInternal opts)

-- | Copy a directory synchronously using a `cp` command with option `recursive = true`.
-- | See the [Node Documentation](https://nodejs.org/api/fs.html#fscpsyncsrc-dest-options)
-- | for details.
cpDir :: FilePath -> FilePath -> Effect Unit
cpDir src dest = cpDir' src dest cpDirOptionsDefault

cpDir' :: FilePath -> FilePath -> CpDirOptions -> Effect Unit
cpDir' src dest opts = runEffectFn3 cpSyncImpl src dest (cpDirOptionsToCpOptionsInternal opts)

-- | Change permissions on a file descriptor. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fchmod_fd_mode_callback)
-- | for details.
fchmod :: FileDescriptor -> Perms -> Effect Unit
fchmod fd perms = runEffectFn2 fchmodSyncImpl fd (permsToString perms)

-- | Change ownership of a file descriptor. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fchown_fd_uid_gid_callback)
-- | for details.
fchown :: FileDescriptor -> Int -> Int -> Effect Unit
fchown fd uid gid = runEffectFn3 fchownSyncImpl fd uid gid

-- | Synchronize a file's in-core state with storage. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fdatasync_fd_callback)
-- | for details.
fdatasync :: FileDescriptor -> Effect Unit
fdatasync fd = runEffectFn1 fdatasyncSyncImpl fd

-- | Get file status information. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fstat_fd_callback)
-- | for details.
fstat :: FileDescriptor -> Effect Stats
fstat fd = runEffectFn1 fstatSyncImpl fd

-- | Flush a file synchronously.  See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_fsyncsync_fd)
-- | for details.
fsync :: FileDescriptor -> Effect Unit
fsync fd = runEffectFn1 fsyncSyncImpl fd

-- | Truncate a file to a specified length. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_ftruncate_fd_len_callback)
-- | for details.
ftruncate :: FileDescriptor -> Int -> Effect Unit
ftruncate fd len = runEffectFn2 ftruncateSyncImpl fd len

-- | Change file timestamps for a file descriptor. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_futimes_fd_atime_mtime_callback)
-- | for details.
futimes :: FileDescriptor -> DateTime -> DateTime -> Effect Unit
futimes fd atime mtime = runEffectFn3 futimesSyncImpl fd (datetimeToUnixEpochTimeInSeconds atime) (datetimeToUnixEpochTimeInSeconds mtime)

-- | Perform pattern matching in file paths. See the [Node Documentation](https://nodejs.org/api/glob.html#globglob_pattern_options_callback)
-- | for details.
glob :: Array FilePath -> Effect (Array FilePath)
glob pattern = glob' pattern globFilePathOptionsDefault

glob' :: Array FilePath -> GlobFilePathOptions -> Effect (Array FilePath)
glob' pattern options = runEffectFn2 globSyncImpl pattern (globFilePathOptionsToInternal options)

globDirent :: Array FilePath -> Effect (Array (Dirent DirentNameString))
globDirent pattern = globDirent' pattern globDirentOptionsDefault

globDirent' :: Array FilePath -> GlobDirentOptions -> Effect (Array (Dirent DirentNameString))
globDirent' pattern options = runEffectFn2 globSyncImpl pattern (globDirentOptionsToInternal options)

-- | Change permissions on a symbolic link. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_lchmod_path_mode_callback)
-- | for details.
lchmod :: FilePath -> Perms -> Effect Unit
lchmod path perms = runEffectFn2 lchmodSyncImpl path (permsToString perms)

-- | Change ownership of a symbolic link. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_lchown_path_uid_gid_callback)
-- | for details.
lchown :: FilePath -> Int -> Int -> Effect Unit
lchown path uid gid = runEffectFn3 lchownSyncImpl path uid gid

-- | Change timestamps for a symbolic link. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_lutimes_path_atime_mtime_callback)
-- | for details.
lutimes
  :: FilePath
  -> DateTime
  -> DateTime
  -> Effect Unit
lutimes file atime mtime = runEffectFn3 lutimesSyncImpl file (datetimeToUnixEpochTimeInSeconds atime) (datetimeToUnixEpochTimeInSeconds mtime)

-- | TODO: path - Buffer Url, returns Promise
-- | Open a file as a blob. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_class_filehandle)
-- | for details.
-- openAsBlob :: FilePath -> Promise Blob -> Effect Unit
-- openAsBlob path = runEffectFn2 openAsBlobSyncImpl path

-- | Open a directory. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_opendir_path_options_callback)
-- | for details.
opendir' :: FilePath -> OpendirOptions -> Effect Dir
opendir' path options = runEffectFn2 opendirSyncImpl path (opendirOptionsToInternal options)

-- | Open a directory. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_opendir_path_options_callback)
-- | for details.
-- | NOTE: encoding: 'buffer' is not supported, will throw error "TypeError [ERR_INVALID_ARG_TYPE]: The "path" argument must be of type string. Received an instance of Buffer"
opendir :: FilePath -> Effect Dir
opendir path = opendir' path opendirOptionsDefault

-- | Read from a file descriptor into a buffer array. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_readv_fd_buffers_position_callback)
-- | for details.
readv :: FileDescriptor -> Array Buffer -> Maybe FilePosition -> Effect ByteCount
readv fd buffers position = runEffectFn3 readvSyncImpl fd buffers (toNullable position)

-- | Get file system statistics. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_statfs_path_callback)
-- | for details.
statfs :: FilePath -> Effect Stats
statfs = runEffectFn1 statfsSyncImpl

-- | Write from an array of buffers to a file descriptor. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_writev_fd_buffers_position_callback)
-- | for details.
writev :: FileDescriptor -> Array Buffer -> Maybe FilePosition -> Effect ByteCount
writev fd buffers position = runEffectFn3 writevSyncImpl fd buffers (toNullable position)
