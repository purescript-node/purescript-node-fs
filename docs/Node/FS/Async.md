## Module Node.FS.Async

#### `Callback`

``` purescript
type Callback eff a = Either Error a -> Eff (fs :: FS | eff) Unit
```

Type synonym for callback functions.

#### `rename`

``` purescript
rename :: forall eff. FilePath -> FilePath -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

Renames a file.

#### `truncate`

``` purescript
truncate :: forall eff. FilePath -> Int -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

Truncates a file to the specified length.

#### `chown`

``` purescript
chown :: forall eff. FilePath -> Int -> Int -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

Changes the ownership of a file.

#### `chmod`

``` purescript
chmod :: forall eff. FilePath -> Perms -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

Changes the permissions of a file.

#### `stat`

``` purescript
stat :: forall eff. FilePath -> Callback eff Stats -> Eff (fs :: FS | eff) Unit
```

Gets file statistics.

#### `link`

``` purescript
link :: forall eff. FilePath -> FilePath -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

Creates a link to an existing file.

#### `symlink`

``` purescript
symlink :: forall eff. FilePath -> FilePath -> SymlinkType -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

Creates a symlink.

#### `readlink`

``` purescript
readlink :: forall eff. FilePath -> Callback eff FilePath -> Eff (fs :: FS | eff) Unit
```

Reads the value of a symlink.

#### `realpath`

``` purescript
realpath :: forall eff. FilePath -> Callback eff FilePath -> Eff (fs :: FS | eff) Unit
```

Find the canonicalized absolute location for a path.

#### `realpath'`

``` purescript
realpath' :: forall eff cache. FilePath -> {  | cache } -> Callback eff FilePath -> Eff (fs :: FS | eff) Unit
```

Find the canonicalized absolute location for a path using a cache object
for already resolved paths.

#### `unlink`

``` purescript
unlink :: forall eff. FilePath -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

Deletes a file.

#### `rmdir`

``` purescript
rmdir :: forall eff. FilePath -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

Deletes a directory.

#### `mkdir`

``` purescript
mkdir :: forall eff. FilePath -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

Makes a new directory.

#### `mkdir'`

``` purescript
mkdir' :: forall eff. FilePath -> Perms -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

Makes a new directory with the specified permissions.

#### `readdir`

``` purescript
readdir :: forall eff. FilePath -> Callback eff (Array FilePath) -> Eff (fs :: FS | eff) Unit
```

Reads the contents of a directory.

#### `utimes`

``` purescript
utimes :: forall eff. FilePath -> Date -> Date -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

Sets the accessed and modified times for the specified file.

#### `readFile`

``` purescript
readFile :: forall eff. FilePath -> Callback (buffer :: BUFFER | eff) Buffer -> Eff (buffer :: BUFFER, fs :: FS | eff) Unit
```

Reads the entire contents of a file returning the result as a raw buffer.

#### `readTextFile`

``` purescript
readTextFile :: forall eff. Encoding -> FilePath -> Callback eff String -> Eff (fs :: FS | eff) Unit
```

Reads the entire contents of a text file with the specified encoding.

#### `writeFile`

``` purescript
writeFile :: forall eff. FilePath -> Buffer -> Callback (buffer :: BUFFER | eff) Unit -> Eff (buffer :: BUFFER, fs :: FS | eff) Unit
```

Writes a buffer to a file.

#### `writeTextFile`

``` purescript
writeTextFile :: forall eff. Encoding -> FilePath -> String -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

Writes text to a file using the specified encoding.

#### `appendFile`

``` purescript
appendFile :: forall eff. FilePath -> Buffer -> Callback (buffer :: BUFFER | eff) Unit -> Eff (buffer :: BUFFER, fs :: FS | eff) Unit
```

Appends the contents of a buffer to a file.

#### `appendTextFile`

``` purescript
appendTextFile :: forall eff. Encoding -> FilePath -> String -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

Appends text to a file using the specified encoding.

#### `exists`

``` purescript
exists :: forall eff. FilePath -> (Boolean -> Eff (fs :: FS | eff) Unit) -> Eff (fs :: FS | eff) Unit
```

Check if the path exists.

#### `fdOpen`

``` purescript
fdOpen :: forall eff. FilePath -> FileFlags -> Maybe FileMode -> Callback eff FileDescriptor -> Eff (fs :: FS | eff) Unit
```

Open a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_open_path_flags_mode_callback)
for details.

#### `fdRead`

``` purescript
fdRead :: forall eff. FileDescriptor -> Buffer -> BufferOffset -> BufferLength -> Maybe FilePosition -> Callback (buffer :: BUFFER | eff) ByteCount -> Eff (buffer :: BUFFER, fs :: FS | eff) Unit
```

Read from a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_read_fd_buffer_offset_length_position_callback)
for details.

#### `fdNext`

``` purescript
fdNext :: forall eff. FileDescriptor -> Buffer -> Callback (buffer :: BUFFER | eff) ByteCount -> Eff (buffer :: BUFFER, fs :: FS | eff) Unit
```

Convenience function to fill the whole buffer from the current
file position.

#### `fdWrite`

``` purescript
fdWrite :: forall eff. FileDescriptor -> Buffer -> BufferOffset -> BufferLength -> Maybe FilePosition -> Callback (buffer :: BUFFER | eff) ByteCount -> Eff (buffer :: BUFFER, fs :: FS | eff) Unit
```

Write to a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_write_fd_buffer_offset_length_position_callback)
for details.

#### `fdAppend`

``` purescript
fdAppend :: forall eff. FileDescriptor -> Buffer -> Callback (buffer :: BUFFER | eff) ByteCount -> Eff (buffer :: BUFFER, fs :: FS | eff) Unit
```

Convenience function to append the whole buffer to the current
file position.

#### `fdClose`

``` purescript
fdClose :: forall eff. FileDescriptor -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

Close a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_close_fd_callback)
for details.


