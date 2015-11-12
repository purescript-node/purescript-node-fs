## Module Node.FS.Sync

#### `rename`

``` purescript
rename :: forall eff. FilePath -> FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

#### `truncate`

``` purescript
truncate :: forall eff. FilePath -> Int -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

#### `chown`

``` purescript
chown :: forall eff. FilePath -> Int -> Int -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

#### `chmod`

``` purescript
chmod :: forall eff. FilePath -> Perms -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

#### `stat`

``` purescript
stat :: forall eff. FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) Stats
```

#### `link`

``` purescript
link :: forall eff. FilePath -> FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

#### `symlink`

``` purescript
symlink :: forall eff. FilePath -> FilePath -> SymlinkType -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

#### `readlink`

``` purescript
readlink :: forall eff. FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) FilePath
```

#### `realpath`

``` purescript
realpath :: forall eff. FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) FilePath
```

#### `realpath'`

``` purescript
realpath' :: forall eff cache. FilePath -> {  | cache } -> Eff (fs :: FS, err :: EXCEPTION | eff) FilePath
```

#### `unlink`

``` purescript
unlink :: forall eff. FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

#### `rmdir`

``` purescript
rmdir :: forall eff. FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

#### `mkdir`

``` purescript
mkdir :: forall eff. FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

#### `mkdir'`

``` purescript
mkdir' :: forall eff. FilePath -> Perms -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

#### `readdir`

``` purescript
readdir :: forall eff. FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) (Array FilePath)
```

#### `utimes`

``` purescript
utimes :: forall eff. FilePath -> Date -> Date -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

#### `readFile`

``` purescript
readFile :: forall eff. FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) Buffer
```

#### `readTextFile`

``` purescript
readTextFile :: forall eff. Encoding -> FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) String
```

#### `writeFile`

``` purescript
writeFile :: forall eff. FilePath -> Buffer -> Eff (buffer :: BUFFER, fs :: FS, err :: EXCEPTION | eff) Unit
```

#### `writeTextFile`

``` purescript
writeTextFile :: forall eff. Encoding -> FilePath -> String -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

#### `appendFile`

``` purescript
appendFile :: forall eff. FilePath -> Buffer -> Eff (buffer :: BUFFER, fs :: FS, err :: EXCEPTION | eff) Unit
```

#### `appendTextFile`

``` purescript
appendTextFile :: forall eff. Encoding -> FilePath -> String -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
```

#### `exists`

``` purescript
exists :: forall eff. FilePath -> Eff (fs :: FS | eff) Boolean
```

#### `fdOpen`

``` purescript
fdOpen :: forall eff. FilePath -> FileFlags -> Maybe FileMode -> Eff (err :: EXCEPTION, fs :: FS | eff) FileDescriptor
```

#### `fdRead`

``` purescript
fdRead :: forall eff. FileDescriptor -> Buffer -> BufferOffset -> BufferLength -> Maybe FilePosition -> Eff (buffer :: BUFFER, err :: EXCEPTION, fs :: FS | eff) ByteCount
```

#### `fdNext`

``` purescript
fdNext :: forall eff. FileDescriptor -> Buffer -> Eff (buffer :: BUFFER, err :: EXCEPTION, fs :: FS | eff) ByteCount
```

#### `fdWrite`

``` purescript
fdWrite :: forall eff. FileDescriptor -> Buffer -> BufferOffset -> BufferLength -> Maybe FilePosition -> Eff (buffer :: BUFFER, err :: EXCEPTION, fs :: FS | eff) ByteCount
```

#### `fdAppend`

``` purescript
fdAppend :: forall eff. FileDescriptor -> Buffer -> Eff (buffer :: BUFFER, err :: EXCEPTION, fs :: FS | eff) ByteCount
```

#### `fdFlush`

``` purescript
fdFlush :: forall eff. FileDescriptor -> Eff (err :: EXCEPTION, fs :: FS | eff) Unit
```

#### `fdClose`

``` purescript
fdClose :: forall eff. FileDescriptor -> Eff (err :: EXCEPTION, fs :: FS | eff) Unit
```


