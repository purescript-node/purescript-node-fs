## Module Node.FS.Sync

#### `rename`

``` purescript
rename :: forall eff. FilePath -> FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

Renames a file.

#### `truncate`

``` purescript
truncate :: forall eff. FilePath -> Int -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

Truncates a file to the specified length.

#### `chown`

``` purescript
chown :: forall eff. FilePath -> Int -> Int -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

Changes the ownership of a file.

#### `chmod`

``` purescript
chmod :: forall eff. FilePath -> Perms -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

Changes the permissions of a file.

#### `stat`

``` purescript
stat :: forall eff. FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) Stats
```

Gets file statistics.

#### `link`

``` purescript
link :: forall eff. FilePath -> FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

Creates a link to an existing file.

#### `symlink`

``` purescript
symlink :: forall eff. FilePath -> FilePath -> SymlinkType -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

Creates a symlink.

#### `readlink`

``` purescript
readlink :: forall eff. FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) FilePath
```

Reads the value of a symlink.

#### `realpath`

``` purescript
realpath :: forall eff. FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) FilePath
```

Find the canonicalized absolute location for a path.

#### `realpath'`

``` purescript
realpath' :: forall eff cache. FilePath -> {  | cache } -> Eff (fs :: FS, err :: EXCEPTION | eff) FilePath
```

Find the canonicalized absolute location for a path using a cache object for
already resolved paths.

#### `unlink`

``` purescript
unlink :: forall eff. FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

Deletes a file.

#### `rmdir`

``` purescript
rmdir :: forall eff. FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

Deletes a directory.

#### `mkdir`

``` purescript
mkdir :: forall eff. FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

Makes a new directory.

#### `mkdir'`

``` purescript
mkdir' :: forall eff. FilePath -> Perms -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

Makes a new directory with the specified permissions.

#### `readdir`

``` purescript
readdir :: forall eff. FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) (Array FilePath)
```

Reads the contents of a directory.

#### `utimes`

``` purescript
utimes :: forall eff. FilePath -> Date -> Date -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

Sets the accessed and modified times for the specified file.

#### `readFile`

``` purescript
readFile :: forall eff. FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) Buffer
```

Reads the entire contents of a file returning the result as a raw buffer.

#### `readTextFile`

``` purescript
readTextFile :: forall eff. Encoding -> FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) String
```

Reads the entire contents of a text file with the specified encoding.

#### `writeFile`

``` purescript
writeFile :: forall eff. FilePath -> Buffer -> Eff (buffer :: BUFFER, fs :: FS, err :: EXCEPTION | eff) Unit
```

Writes a buffer to a file.

#### `writeTextFile`

``` purescript
writeTextFile :: forall eff. Encoding -> FilePath -> String -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

Writes text to a file using the specified encoding.

#### `appendFile`

``` purescript
appendFile :: forall eff. FilePath -> Buffer -> Eff (buffer :: BUFFER, fs :: FS, err :: EXCEPTION | eff) Unit
```

Appends the contents of a buffer to a file.

#### `appendTextFile`

``` purescript
appendTextFile :: forall eff. Encoding -> FilePath -> String -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

Appends text to a file using the specified encoding.

#### `exists`

``` purescript
exists :: forall eff. FilePath -> Eff (fs :: FS | eff) Boolean
```

Check if the path exists.

#### `fdOpen`

``` purescript
fdOpen :: forall eff. FilePath -> FileFlags -> Maybe FileMode -> Eff (err :: EXCEPTION, fs :: FS | eff) FileDescriptor
```

Open a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_opensync_path_flags_mode)
for details.

#### `fdRead`

``` purescript
fdRead :: forall eff. FileDescriptor -> Buffer -> BufferOffset -> BufferLength -> Maybe FilePosition -> Eff (buffer :: BUFFER, err :: EXCEPTION, fs :: FS | eff) ByteCount
```

Read from a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_readsync_fd_buffer_offset_length_position)
for details.

#### `fdNext`

``` purescript
fdNext :: forall eff. FileDescriptor -> Buffer -> Eff (buffer :: BUFFER, err :: EXCEPTION, fs :: FS | eff) ByteCount
```

Convenience function to fill the whole buffer from the current
file position.

#### `fdWrite`

``` purescript
fdWrite :: forall eff. FileDescriptor -> Buffer -> BufferOffset -> BufferLength -> Maybe FilePosition -> Eff (buffer :: BUFFER, err :: EXCEPTION, fs :: FS | eff) ByteCount
```

Write to a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_writesync_fd_buffer_offset_length_position)
for details.

#### `fdAppend`

``` purescript
fdAppend :: forall eff. FileDescriptor -> Buffer -> Eff (buffer :: BUFFER, err :: EXCEPTION, fs :: FS | eff) ByteCount
```

Convenience function to append the whole buffer to the current
file position.

#### `fdFlush`

``` purescript
fdFlush :: forall eff. FileDescriptor -> Eff (err :: EXCEPTION, fs :: FS | eff) Unit
```

Flush a file synchronously.  See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_fsyncsync_fd)
for details.

#### `fdClose`

``` purescript
fdClose :: forall eff. FileDescriptor -> Eff (err :: EXCEPTION, fs :: FS | eff) Unit
```

Close a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_closesync_fd)
for details.


