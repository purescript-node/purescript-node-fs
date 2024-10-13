module Node.FS.Aff
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
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Effect.Aff (Aff, Error, makeAff, nonCanceler)
import Node.Buffer (Buffer)
import Node.Encoding (Encoding)
import Node.FS.Types (BufferLength, BufferOffset, ByteCount, FileDescriptor, FileMode, FilePosition, SymlinkType)
import Node.FS.Internal.ToAff (toAff1, toAff2, toAff3, toAff4, toAff5)
import Node.FS.Options (AppendFileBufferOptions, CpDirOptions, CpFileOptions, FdReadOptions, FdWriteOptions, GlobDirentOptions, GlobFilePathOptions, MkdirOptions, OpendirOptions, ReadFileBufferOptions, ReadFileStringOptions, ReaddirBufferOptions, ReaddirDirentBufferOptions, ReaddirDirentOptions, ReaddirFilePathOptions, RealpathOptions, RmOptions, RmdirOptions, WriteFileBufferOptions, WriteFileStringOptions)
import Node.FS.Constants (AccessMode, CopyMode, FileFlags)
import Node.FS.Async as A
import Node.FS.Dir (Dir)
import Node.FS.Dirent (Dirent, DirentNameBuffer, DirentNameString)
import Node.FS.Perms (Perms)
import Node.FS.Stats (Stats)
import Node.Path (FilePath)

access :: FilePath -> Aff (Maybe Error)
access path = makeAff \k -> do
  A.access path (k <<< Right)
  pure nonCanceler

access' :: FilePath -> AccessMode -> Aff (Maybe Error)
access' path mode = makeAff \k -> do
  A.access' path mode (k <<< Right)
  pure nonCanceler

copyFile :: FilePath -> FilePath -> Aff Unit
copyFile = toAff2 A.copyFile

copyFile' :: FilePath -> FilePath -> CopyMode -> Aff Unit
copyFile' = toAff3 A.copyFile'

mkdtemp :: FilePath -> Aff FilePath
mkdtemp = toAff1 A.mkdtemp

mkdtemp' :: FilePath -> Encoding -> Aff FilePath
mkdtemp' = toAff2 A.mkdtemp'

-- |
-- | Rename a file.
-- |
rename :: FilePath -> FilePath -> Aff Unit
rename = toAff2 A.rename

-- |
-- | Truncates a file to the specified length.
-- |
truncate :: FilePath -> Int -> Aff Unit
truncate = toAff2 A.truncate

-- |
-- | Changes the ownership of a file.
-- |
chown :: FilePath -> Int -> Int -> Aff Unit
chown = toAff3 A.chown

-- |
-- | Changes the permissions of a file.
-- |
chmod :: FilePath -> Perms -> Aff Unit
chmod = toAff2 A.chmod

-- |
-- | Gets file statistics.
-- |
stat :: FilePath -> Aff Stats
stat = toAff1 A.stat

-- | Gets file or symlink statistics. `lstat` is identical to `stat`, except
-- | that if the `FilePath` is a symbolic link, then the link itself is stat-ed,
-- | not the file that it refers to.
lstat :: FilePath -> Aff Stats
lstat = toAff1 A.lstat

-- |
-- | Creates a link to an existing file.
-- |
link :: FilePath -> FilePath -> Aff Unit
link = toAff2 A.link

-- |
-- | Creates a symlink.
-- |
symlink
  :: FilePath
  -> FilePath
  -> SymlinkType
  -> Aff Unit
symlink = toAff3 A.symlink

-- |
-- | Reads the value of a symlink.
-- |
readlink :: FilePath -> Aff FilePath
readlink = toAff1 A.readlink

readlinkBuffer :: FilePath -> Aff Buffer
readlinkBuffer = toAff1 A.readlinkBuffer

-- |
-- | Find the canonicalized absolute location for a path.
-- |
realpath :: FilePath -> Aff FilePath
realpath = toAff1 A.realpath

-- |
-- | Find the canonicalized absolute location for a path using a cache object
-- | for already resolved paths.
-- |
realpath' :: FilePath -> RealpathOptions -> Aff FilePath
realpath' = toAff2 A.realpath'

-- |
-- | Deletes a file.
-- |
unlink :: FilePath -> Aff Unit
unlink = toAff1 A.unlink

-- |
-- | Deletes a directory.
-- |
rmdir :: FilePath -> Aff Unit
rmdir = toAff1 A.rmdir

-- |
-- | Deletes a directory with options.
-- |
rmdir' :: FilePath -> RmdirOptions -> Aff Unit
rmdir' = toAff2 A.rmdir'

-- |
-- | Deletes a file or directory.
-- |
rm :: FilePath -> Aff Unit
rm = toAff1 A.rm

-- |
-- | Deletes a file or directory with options.
-- |
rm' :: FilePath -> RmOptions -> Aff Unit
rm' = toAff2 A.rm'

-- |
-- | Makes a new directory.
-- |
mkdir :: FilePath -> Aff Unit
mkdir = toAff1 A.mkdir

-- |
-- | Makes a new directory with all of its options.
-- |
mkdir' :: FilePath -> MkdirOptions -> Aff Unit
mkdir' = toAff2 A.mkdir'

-- |
-- | Reads the contents of a directory.
-- |
readdir :: FilePath -> Aff (Array FilePath)
readdir = toAff1 A.readdir

-- | Reads the contents of a directory with options.
readdir' :: FilePath -> ReaddirFilePathOptions -> Aff (Array FilePath)
readdir' = toAff2 A.readdir'

-- | Reads the contents of a directory and returns an Aff (Array Buffer).
readdirBuffer :: FilePath -> Aff (Array Buffer)
readdirBuffer = toAff1 A.readdirBuffer

-- | Reads the contents of a directory with options and returns Aff (Array Buffer).
readdirBuffer' :: FilePath -> ReaddirBufferOptions -> Aff (Array Buffer)
readdirBuffer' = toAff2 A.readdirBuffer'

-- | Reads the contents of a directory and returns an Aff (Array (Dirent DirentNameString)).
readdirDirent :: FilePath -> Aff (Array (Dirent DirentNameString))
readdirDirent = toAff1 A.readdirDirent

-- | Reads the contents of a directory with options and returns Aff (Array (Dirent DirentNameString)).
readdirDirent' :: FilePath -> ReaddirDirentOptions -> Aff (Array (Dirent DirentNameString))
readdirDirent' = toAff2 A.readdirDirent'

-- | Reads the contents of a directory.
readdirDirentBuffer
  :: FilePath
  -> Aff (Array (Dirent DirentNameBuffer))
readdirDirentBuffer = toAff1 A.readdirDirentBuffer

-- | Reads the contents of a directory.
readdirDirentBuffer'
  :: FilePath
  -> ReaddirDirentBufferOptions
  -> Aff (Array (Dirent DirentNameBuffer))
readdirDirentBuffer' = toAff2 A.readdirDirentBuffer'

-- |
-- | Sets the accessed and modified times for the specified file.
-- |
utimes :: FilePath -> DateTime -> DateTime -> Aff Unit
utimes = toAff3 A.utimes

-- |
-- | Reads the entire contents of a file returning the result as a raw buffer.
-- |
readFile :: FilePath -> Aff Buffer
readFile = toAff1 A.readFile

readFile' :: FilePath -> ReadFileBufferOptions -> Aff Buffer
readFile' = toAff2 A.readFile'

-- |
-- | Reads the entire contents of a text file with the specified encoding.
-- |
readTextFile :: Encoding -> FilePath -> Aff String
readTextFile = toAff2 A.readTextFile

readTextFile' :: FilePath -> ReadFileStringOptions -> Aff String
readTextFile' = toAff2 A.readTextFile'

-- |
-- | Writes a buffer to a file.
-- |
writeFile :: FilePath -> Buffer -> Aff Unit
writeFile = toAff2 A.writeFile

writeFile' :: FilePath -> Buffer -> WriteFileBufferOptions -> Aff Unit
writeFile' = toAff3 A.writeFile'

-- |
-- | Writes text to a file using the specified encoding.
-- |
writeTextFile :: Encoding -> FilePath -> String -> Aff Unit
writeTextFile = toAff3 A.writeTextFile

writeTextFile' :: FilePath -> String -> WriteFileStringOptions -> Aff Unit
writeTextFile' = toAff3 A.writeTextFile'

-- |
-- | Appends the contents of a buffer to a file.
-- |
appendFile :: FilePath -> Buffer -> Aff Unit
appendFile = toAff2 A.appendFile

appendFile' :: FilePath -> Buffer -> AppendFileBufferOptions -> Aff Unit
appendFile' = toAff3 A.appendFile'

-- |
-- | Appends text to a file using the specified encoding.
-- |
appendTextFile :: Encoding -> FilePath -> String -> Aff Unit
appendTextFile = toAff3 A.appendTextFile

-- | Open a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_open_path_flags_mode_callback)
-- | for details.
fdOpen
  :: FilePath
  -> FileFlags
  -> Maybe FileMode
  -> Aff FileDescriptor
fdOpen = toAff3 A.fdOpen

-- | Read from a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_read_fd_buffer_offset_length_position_callback)
-- | for details.
fdRead
  :: FileDescriptor
  -> Buffer
  -> BufferOffset
  -> BufferLength
  -> Maybe FilePosition
  -> Aff ByteCount
fdRead = toAff5 A.fdRead

-- | Read from a file asynchronously. See the [Node Documentation](https://nodejs.org/docs/latest/api/fs.html#fsreadfd-options-callback)
-- | for details.
fdRead'
  :: FileDescriptor
  -> FdReadOptions
  -> Aff (Tuple ByteCount Buffer)
fdRead' = toAff2 A.fdRead'

-- | Convenience function to fill the whole buffer from the current
-- | file position.
fdNext :: FileDescriptor -> Buffer -> Aff ByteCount
fdNext = toAff2 A.fdNext

-- | Write to a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_write_fd_buffer_offset_length_position_callback)
-- | for details.
fdWrite
  :: FileDescriptor
  -> Buffer
  -> BufferOffset
  -> BufferLength
  -> Maybe FilePosition
  -> Aff ByteCount
fdWrite = toAff5 A.fdWrite

-- | Write from a file asynchronously. See the [Node Documentation](https://nodejs.org/docs/latest/api/fs.html#fswritefd-options-callback)
-- | for details.
fdWrite'
  :: FileDescriptor
  -> Buffer
  -> FdWriteOptions
  -> Aff (Tuple ByteCount Buffer)
fdWrite' = toAff3 A.fdWrite'

-- It is unsafe to use fs.write() multiple times on the same file without waiting for the callback. For this scenario, fs.createWriteStream() is recommended.
fdWriteString
  :: FileDescriptor
  -> String
  -> Maybe FilePosition
  -> Encoding
  -> Aff (Tuple ByteCount String)
fdWriteString = toAff4 A.fdWriteString

-- | Convenience function to append the whole buffer to the current
-- | file position.
fdAppend :: FileDescriptor -> Buffer -> Aff ByteCount
fdAppend = toAff2 A.fdAppend

-- | Close a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_close_fd_callback)
-- | for details.
fdClose :: FileDescriptor -> Aff Unit
fdClose = toAff1 A.fdClose

-- | Copy a file asynchronously using a `cp` command.
-- | See the [Node Documentation](https://nodejs.org/api/fs.html#fscpsrc-dest-options-callback)
-- | for details.
cpFile :: FilePath -> FilePath -> Aff Unit
cpFile = toAff2 A.cpFile

cpFile' :: FilePath -> FilePath -> CpFileOptions -> Aff Unit
cpFile' = toAff3 A.cpFile'

-- | Copy a directory asynchronously using a `cp` command with option `recursive = true`.
-- | See the [Node Documentation](https://nodejs.org/api/fs.html#fscpsrc-dest-options-callback)
-- | for details.
cpDir :: FilePath -> FilePath -> Aff Unit
cpDir = toAff2 A.cpDir

cpDir' :: FilePath -> FilePath -> CpDirOptions -> Aff Unit
cpDir' = toAff3 A.cpDir'

-- | Change permissions on a file descriptor. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fchmod_fd_mode_callback)
-- | for details.
fchmod :: FileDescriptor -> Perms -> Aff Unit
fchmod = toAff2 A.fchmod

-- | Change ownership of a file descriptor. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fchown_fd_uid_gid_callback)
-- | for details.
fchown :: FileDescriptor -> Int -> Int -> Aff Unit
fchown = toAff3 A.fchown

-- | Synchronize a file's in-core state with storage. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fdatasync_fd_callback)
-- | for details.
fdatasync :: FileDescriptor -> Aff Unit
fdatasync = toAff1 A.fdatasync

-- | Get file status information. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fstat_fd_callback)
-- | for details.
fstat :: FileDescriptor -> Aff Stats
fstat = toAff1 A.fstat

-- | Flushes a file descriptor to disk. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_fsync_fd_callback)
-- | for details.
fsync :: FileDescriptor -> Aff Unit
fsync = toAff1 A.fsync

-- | Truncate a file to a specified length. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_ftruncate_fd_len_callback)
-- | for details.
ftruncate :: FileDescriptor -> Int -> Aff Unit
ftruncate = toAff2 A.ftruncate

-- | Change file timestamps for a file descriptor. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_futimes_fd_atime_mtime_callback)
-- | for details.
futimes :: FileDescriptor -> DateTime -> DateTime -> Aff Unit
futimes = toAff3 A.futimes

-- | Perform pattern matching in file paths. See the [Node Documentation](https://nodejs.org/api/glob.html#globglob_pattern_options_callback)
-- | for details.
glob :: Array FilePath -> Aff (Array FilePath)
glob = toAff1 A.glob

glob' :: Array FilePath -> GlobFilePathOptions -> Aff (Array FilePath)
glob' = toAff2 A.glob'

globDirent :: Array FilePath -> Aff (Array (Dirent DirentNameString))
globDirent = toAff1 A.globDirent

globDirent' :: Array FilePath -> GlobDirentOptions -> Aff (Array (Dirent DirentNameString))
globDirent' = toAff2 A.globDirent'

-- | Change permissions on a symbolic link. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_lchmod_path_mode_callback)
-- | for details.
lchmod :: FilePath -> Perms -> Aff Unit
lchmod = toAff2 A.lchmod

-- | Change ownership of a symbolic link. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_lchown_path_uid_gid_callback)
-- | for details.
lchown :: FilePath -> Int -> Int -> Aff Unit
lchown = toAff3 A.lchown

-- | Change timestamps for a symbolic link. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_lutimes_path_atime_mtime_callback)
-- | for details.
lutimes :: FilePath -> DateTime -> DateTime -> Aff Unit
lutimes = toAff3 A.lutimes

-- | Open a file as a blob. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_class_filehandle)
-- | for details.
-- openAsBlob :: FilePath -> Aff Blob
-- openAsBlob = toAff1 A.openAsBlob

-- | Open a directory. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_opendir_path_options_callback)
-- | for details.
opendir :: FilePath -> Aff Dir
opendir = toAff1 A.opendir

-- | Open a directory. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_opendir_path_options_callback)
-- | for details.
opendir' :: FilePath -> OpendirOptions -> Aff Dir
opendir' = toAff2 A.opendir'

-- | Read from a file descriptor into a buffer array. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_readv_fd_buffers_position_callback)
-- | for details.
readv :: FileDescriptor -> Array Buffer -> Maybe FilePosition -> Aff (Tuple ByteCount (Array Buffer))
readv = toAff3 A.readv

-- | TODO: bigint, path Buffer Url
-- | Get file system statistics. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_statfs_path_callback)
-- | for details.
statfs :: FilePath -> Aff Stats
statfs = toAff1 A.statfs

-- | TODO: implement
-- | Stop watching a file for changes. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_unwatchfile_filename_listener)
-- | for details.
-- unwatchFile :: FilePath -> Effect Unit
-- unwatchFile = toAff1 A.unwatchFile

-- | Watch for changes in a file or directory. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_watch_filename_options_listener)
-- | for details.
-- watch :: FilePath -> (String -> Effect Unit) -> Effect Unit
-- watch = toAff2 A.watch

-- | Watch for changes in a file and trigger a callback. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_watchfile_filename_options_listener)
-- | for details.
-- watchFile :: FilePath -> (Stats -> Stats -> Effect Unit) -> Effect Unit
-- watchFile = toAff2 A.watchFile

-- | Write from an array of buffers to a file descriptor. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_writev_fd_buffers_position_callback)
-- | for details.
writev :: FileDescriptor -> Array Buffer -> Maybe FilePosition -> Aff (Tuple ByteCount (Array Buffer))
writev = toAff3 A.writev
