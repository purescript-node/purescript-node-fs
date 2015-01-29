# Module Documentation

## Module Node.FS

### Types

     |
     Effect type for file system usage.
     

    data FS :: !

     |
     Symlink varieties.
     

    data SymlinkType where
      FileLink :: SymlinkType
      DirLink :: SymlinkType
      JunctionLink :: SymlinkType


### Type Class Instances


    instance eqSymlinkType :: Eq SymlinkType


    instance showSymlinkType :: Show SymlinkType


## Module Node.FS.Async

### Types

     |
     Type synonym for callback functions.

    type Callback eff a = Either Error a -> Eff (fs :: FS | eff) Unit


### Values

     |
     Appends the contents of a buffer to a file.

    appendFile :: forall eff. FilePath -> Buffer -> Callback eff Unit -> Eff (fs :: FS | eff) Unit

     |
     Appends text to a file using the specified encoding.

    appendTextFile :: forall eff. Encoding -> FilePath -> String -> Callback eff Unit -> Eff (fs :: FS | eff) Unit

     |
     Changes the permissions of a file.

    chmod :: forall eff. FilePath -> Number -> Callback eff Unit -> Eff (fs :: FS | eff) Unit

     |
     Changes the ownership of a file.

    chown :: forall eff. FilePath -> Number -> Number -> Callback eff Unit -> Eff (fs :: FS | eff) Unit

     |
     Check if the path exists.

    exists :: forall eff. FilePath -> (Boolean -> Eff eff Unit) -> Eff (fs :: FS | eff) Unit

     |
     Creates a link to an existing file.

    link :: forall eff. FilePath -> FilePath -> Callback eff Unit -> Eff (fs :: FS | eff) Unit

     |
     Makes a new directory.

    mkdir :: forall eff. FilePath -> Callback eff Unit -> Eff (fs :: FS | eff) Unit

     |
     Makes a new directory with the specified permissions.

    mkdir' :: forall eff. FilePath -> Number -> Callback eff Unit -> Eff (fs :: FS | eff) Unit

     |
     Reads the entire contents of a file returning the result as a raw buffer.

    readFile :: forall eff. FilePath -> Callback eff Buffer -> Eff (fs :: FS | eff) Unit

     |
     Reads the entire contents of a text file with the specified encoding.

    readTextFile :: forall eff. Encoding -> FilePath -> Callback eff String -> Eff (fs :: FS | eff) Unit

     |
     Reads the contents of a directory.

    readdir :: forall eff. FilePath -> Callback eff [FilePath] -> Eff (fs :: FS | eff) Unit

     |
     Reads the value of a symlink.

    readlink :: forall eff. FilePath -> Callback eff FilePath -> Eff (fs :: FS | eff) Unit

     |
     Find the canonicalized absolute location for a path.

    realpath :: forall eff. FilePath -> Callback eff FilePath -> Eff (fs :: FS | eff) Unit

     |
     Find the canonicalized absolute location for a path using a cache object for
     already resolved paths.

    realpath' :: forall eff cache. FilePath -> {  | cache } -> Callback eff FilePath -> Eff (fs :: FS | eff) Unit

     |
     Renames a file.

    rename :: forall eff. FilePath -> FilePath -> Callback eff Unit -> Eff (fs :: FS | eff) Unit

     |
     Deletes a directory.

    rmdir :: forall eff. FilePath -> Callback eff Unit -> Eff (fs :: FS | eff) Unit

     |
     Gets file statistics.

    stat :: forall eff. FilePath -> Callback eff Stats -> Eff (fs :: FS | eff) Unit

     |
     Creates a symlink.

    symlink :: forall eff. FilePath -> FilePath -> SymlinkType -> Callback eff Unit -> Eff (fs :: FS | eff) Unit

     |
     Truncates a file to the specified length.

    truncate :: forall eff. FilePath -> Number -> Callback eff Unit -> Eff (fs :: FS | eff) Unit

     |
     Deletes a file.

    unlink :: forall eff. FilePath -> Callback eff Unit -> Eff (fs :: FS | eff) Unit

     |
     Sets the accessed and modified times for the specified file.

    utimes :: forall eff. FilePath -> Date -> Date -> Callback eff Unit -> Eff (fs :: FS | eff) Unit

     |
     Writes a buffer to a file.

    writeFile :: forall eff. FilePath -> Buffer -> Callback eff Unit -> Eff (fs :: FS | eff) Unit

     |
     Writes text to a file using the specified encoding.

    writeTextFile :: forall eff. Encoding -> FilePath -> String -> Callback eff Unit -> Eff (fs :: FS | eff) Unit


## Module Node.FS.Stats

### Types

     |
     Stats wrapper to provide a usable interface to the underlying properties and methods.

    data Stats where
      Stats :: StatsObj -> Stats


    type StatsObj = { isSocket :: Fn0 Boolean, isFIFO :: Fn0 Boolean, isCharacterDevice :: Fn0 Boolean, isBlockDevice :: Fn0 Boolean, isDirectory :: Fn0 Boolean, isFile :: Fn0 Boolean, ctime :: JSDate, mtime :: JSDate, atime :: JSDate, size :: Number, ino :: Number, rdev :: Number, gid :: Number, uid :: Number, nlink :: Number, mode :: Number, dev :: Number }


### Type Class Instances


    instance showStats :: Show Stats


### Values


    accessedTime :: Stats -> Date


    isBlockDevice :: Stats -> Boolean


    isCharacterDevice :: Stats -> Boolean


    isDirectory :: Stats -> Boolean


    isFIFO :: Stats -> Boolean


    isFile :: Stats -> Boolean


    isSocket :: Stats -> Boolean


    isSymbolicLink :: Stats -> Boolean


    modifiedTime :: Stats -> Date


    statusChangedTime :: Stats -> Date


## Module Node.FS.Sync

### Types


    type BufferLength = Number


    type BufferOffset = Number


    type ByteCount = Number


    data FileDescriptor :: *


    data FileFlags where
      R :: FileFlags
      R_PLUS :: FileFlags
      RS :: FileFlags
      RS_PLUS :: FileFlags
      W :: FileFlags
      WX :: FileFlags
      W_PLUS :: FileFlags
      WX_PLUS :: FileFlags
      A :: FileFlags
      AX :: FileFlags
      A_PLUS :: FileFlags
      AX_PLUS :: FileFlags


    type FileMode = Number


    type FilePosition = Number


### Values

     |
     Appends the contents of a buffer to a file.

    appendFile :: forall eff. FilePath -> Buffer -> Eff (err :: Exception, fs :: FS | eff) Unit

     |
     Appends text to a file using the specified encoding.

    appendTextFile :: forall eff. Encoding -> FilePath -> String -> Eff (err :: Exception, fs :: FS | eff) Unit

     |
     Changes the permissions of a file.

    chmod :: forall eff. FilePath -> Number -> Eff (err :: Exception, fs :: FS | eff) Unit

     |
     Changes the ownership of a file.

    chown :: forall eff. FilePath -> Number -> Number -> Eff (err :: Exception, fs :: FS | eff) Unit

     |
     Check if the path exists.

    exists :: forall eff. FilePath -> Eff (fs :: FS | eff) Boolean

    |
     Convienence function to append the whole buffer to the current
     file position.

    fdAppend :: forall eff. FileDescriptor -> Buffer -> Eff (fs :: FS, err :: Exception | eff) ByteCount

    |
     Close a file synchronously.  See <a
     href="http://nodejs.org/api/fs.html#fs_fs_closesync_fd">Node
     Documentation</a> for details.

    fdClose :: forall eff. FileDescriptor -> Eff (fs :: FS, err :: Exception | eff) Unit

    |
     Flush a file synchronously.  See <a
     href="http://nodejs.org/api/fs.html#fs_fs_fsyncsync_fd">Node
     Documentation</a> for details.

    fdFlush :: forall eff. FileDescriptor -> Eff (fs :: FS, err :: Exception | eff) Unit

    |
     Convienence function to fill the whole buffer from the current
     file position.

    fdNext :: forall eff. FileDescriptor -> Buffer -> Eff (fs :: FS, err :: Exception | eff) ByteCount

     Synchronous File Descriptor Functions 
    |
     Open a file synchronously.  See <a
     href="http://nodejs.org/api/fs.html#fs_fs_opensync_path_flags_mode">Node
     Documentation</a> for details.

    fdOpen :: forall opts eff. FilePath -> FileFlags -> Maybe FileMode -> Eff (fs :: FS, err :: Exception | eff) FileDescriptor

    |
     Read to a file synchronously.  See <a
     href="http://nodejs.org/api/fs.html#fs_fs_readsync_fd_buffer_offset_length_position">Node
     ocumentation</a> for details.

    fdRead :: forall eff. FileDescriptor -> Buffer -> BufferOffset -> BufferLength -> Maybe FilePosition -> Eff (fs :: FS, err :: Exception | eff) ByteCount

    |
     Write to a file synchronously.  See <a
     href="http://nodejs.org/api/fs.html#fs_fs_writesync_fd_buffer_offset_length_position">Node
     Documentation</a> for details.

    fdWrite :: forall eff. FileDescriptor -> Buffer -> BufferOffset -> BufferLength -> Maybe FilePosition -> Eff (fs :: FS, err :: Exception | eff) ByteCount

     |
     Creates a link to an existing file.

    link :: forall eff. FilePath -> FilePath -> Eff (err :: Exception, fs :: FS | eff) Unit

     |
     Makes a new directory.

    mkdir :: forall eff. FilePath -> Eff (err :: Exception, fs :: FS | eff) Unit

     |
     Makes a new directory with the specified permissions.

    mkdir' :: forall eff. FilePath -> Number -> Eff (err :: Exception, fs :: FS | eff) Unit

     |
     Reads the entire contents of a file returning the result as a raw buffer.

    readFile :: forall eff. FilePath -> Eff (err :: Exception, fs :: FS | eff) Buffer

     |
     Reads the entire contents of a text file with the specified encoding.

    readTextFile :: forall eff. Encoding -> FilePath -> Eff (err :: Exception, fs :: FS | eff) String

     |
     Reads the contents of a directory.

    readdir :: forall eff. FilePath -> Eff (err :: Exception, fs :: FS | eff) [FilePath]

     |
     Reads the value of a symlink.

    readlink :: forall eff. FilePath -> Eff (err :: Exception, fs :: FS | eff) FilePath

     |
     Find the canonicalized absolute location for a path.

    realpath :: forall eff. FilePath -> Eff (err :: Exception, fs :: FS | eff) FilePath

     |
     Find the canonicalized absolute location for a path using a cache object for
     already resolved paths.

    realpath' :: forall eff cache. FilePath -> {  | cache } -> Eff (err :: Exception, fs :: FS | eff) FilePath

     |
     Renames a file.

    rename :: forall eff. FilePath -> FilePath -> Eff (err :: Exception, fs :: FS | eff) Unit

     |
     Deletes a directory.

    rmdir :: forall eff. FilePath -> Eff (err :: Exception, fs :: FS | eff) Unit

     |
     Gets file statistics.

    stat :: forall eff. FilePath -> Eff (err :: Exception, fs :: FS | eff) Stats

     |
     Creates a symlink.

    symlink :: forall eff. FilePath -> FilePath -> SymlinkType -> Eff (err :: Exception, fs :: FS | eff) Unit

     |
     Truncates a file to the specified length.

    truncate :: forall eff. FilePath -> Number -> Eff (err :: Exception, fs :: FS | eff) Unit

     |
     Deletes a file.

    unlink :: forall eff. FilePath -> Eff (err :: Exception, fs :: FS | eff) Unit

     |
     Sets the accessed and modified times for the specified file.

    utimes :: forall eff. FilePath -> Date -> Date -> Eff (err :: Exception, fs :: FS | eff) Unit

     |
     Writes a buffer to a file.

    writeFile :: forall eff. FilePath -> Buffer -> Eff (err :: Exception, fs :: FS | eff) Unit

     |
     Writes text to a file using the specified encoding.

    writeTextFile :: forall eff. Encoding -> FilePath -> String -> Eff (err :: Exception, fs :: FS | eff) Unit