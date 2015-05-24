# Module Documentation

## Module Node.FS

#### `FS`

``` purescript
data FS :: !
```

#### `SymlinkType`

``` purescript
data SymlinkType
  = FileLink 
  | DirLink 
  | JunctionLink 
```

#### `showSymlinkType`

``` purescript
instance showSymlinkType :: Show SymlinkType
```


#### `eqSymlinkType`

``` purescript
instance eqSymlinkType :: Eq SymlinkType
```



## Module Node.FS.Async

#### `Callback`

``` purescript
type Callback eff a = Either Error a -> Eff (fs :: FS | eff) Unit
```

#### `rename`

``` purescript
rename :: forall eff. FilePath -> FilePath -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

#### `truncate`

``` purescript
truncate :: forall eff. FilePath -> Number -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

#### `chown`

``` purescript
chown :: forall eff. FilePath -> Number -> Number -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

#### `chmod`

``` purescript
chmod :: forall eff. FilePath -> Perms -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

#### `stat`

``` purescript
stat :: forall eff. FilePath -> Callback eff Stats -> Eff (fs :: FS | eff) Unit
```

#### `link`

``` purescript
link :: forall eff. FilePath -> FilePath -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

#### `symlink`

``` purescript
symlink :: forall eff. FilePath -> FilePath -> SymlinkType -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

#### `readlink`

``` purescript
readlink :: forall eff. FilePath -> Callback eff FilePath -> Eff (fs :: FS | eff) Unit
```

#### `realpath`

``` purescript
realpath :: forall eff. FilePath -> Callback eff FilePath -> Eff (fs :: FS | eff) Unit
```

#### `realpath'`

``` purescript
realpath' :: forall eff cache. FilePath -> {  | cache } -> Callback eff FilePath -> Eff (fs :: FS | eff) Unit
```

#### `unlink`

``` purescript
unlink :: forall eff. FilePath -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

#### `rmdir`

``` purescript
rmdir :: forall eff. FilePath -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

#### `mkdir`

``` purescript
mkdir :: forall eff. FilePath -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

#### `mkdir'`

``` purescript
mkdir' :: forall eff. FilePath -> Perms -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

#### `readdir`

``` purescript
readdir :: forall eff. FilePath -> Callback eff [FilePath] -> Eff (fs :: FS | eff) Unit
```

#### `utimes`

``` purescript
utimes :: forall eff. FilePath -> Date -> Date -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

#### `readFile`

``` purescript
readFile :: forall eff. FilePath -> Callback eff Buffer -> Eff (fs :: FS | eff) Unit
```

#### `readTextFile`

``` purescript
readTextFile :: forall eff. Encoding -> FilePath -> Callback eff String -> Eff (fs :: FS | eff) Unit
```

#### `writeFile`

``` purescript
writeFile :: forall eff. FilePath -> Buffer -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

#### `writeTextFile`

``` purescript
writeTextFile :: forall eff. Encoding -> FilePath -> String -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

#### `appendFile`

``` purescript
appendFile :: forall eff. FilePath -> Buffer -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

#### `appendTextFile`

``` purescript
appendTextFile :: forall eff. Encoding -> FilePath -> String -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
```

#### `exists`

``` purescript
exists :: forall eff. FilePath -> (Boolean -> Eff eff Unit) -> Eff (fs :: FS | eff) Unit
```


## Module Node.FS.Perms

#### `Perm`

``` purescript
newtype Perm
```

A `Perm` value specifies what is allowed to be done with a particular
file by a particular class of user &mdash; that is, whether it is
readable, writable, and/or executable. It has a semigroup instance, which
allows you to combine permissions; for example, `read <> write` means
"readable and writable".

#### `eqPerm`

``` purescript
instance eqPerm :: Eq Perm
```


#### `ordPerm`

``` purescript
instance ordPerm :: Ord Perm
```


#### `showPerm`

``` purescript
instance showPerm :: Show Perm
```


#### `semigroupPerm`

``` purescript
instance semigroupPerm :: Semigroup Perm
```


#### `none`

``` purescript
none :: Perm
```

No permissions. This is the identity of the `Semigroup` operation `(<>)`
for `Perm`.

#### `read`

``` purescript
read :: Perm
```

The "readable" permission.

#### `write`

``` purescript
write :: Perm
```

The "writable" permission.

#### `execute`

``` purescript
execute :: Perm
```

The "executable" permission.

#### `all`

``` purescript
all :: Perm
```

All permissions: readable, writable, and executable.

#### `Perms`

``` purescript
newtype Perms
```

A `Perms` value includes all the permissions information about a
particular file or directory, by storing a `Perm` value for each of the
file owner, the group, and any other users.

#### `eqPerms`

``` purescript
instance eqPerms :: Eq Perms
```


#### `ordPerms`

``` purescript
instance ordPerms :: Ord Perms
```


#### `showPerms`

``` purescript
instance showPerms :: Show Perms
```


#### `permsFromString`

``` purescript
permsFromString :: String -> Maybe Perms
```

Attempt to parse a `Perms` value from a `String` containing an octal
integer. For example,
`permsFromString "0644" == Just (mkPerms (read <> write) read read)`.

#### `mkPerms`

``` purescript
mkPerms :: Perm -> Perm -> Perm -> Perms
```

Create a `Perms` value. The arguments represent the owner's, group's, and
other users' permission sets, respectively.

#### `permsToString`

``` purescript
permsToString :: Perms -> String
```

Convert a `Perms` value to an octal string, in a format similar to that
accepted by `chmod`. For example:

* `permsToString (mkPerms (r <> w) r r) == "0644"`

#### `permsToInt`

``` purescript
permsToInt :: Perms -> Int
```

Convert a `Perms` value to an `Int`, via `permsToString`.


## Module Node.FS.Stats

#### `StatsObj`

``` purescript
type StatsObj = { isSocket :: Fn0 Boolean, isFIFO :: Fn0 Boolean, isCharacterDevice :: Fn0 Boolean, isBlockDevice :: Fn0 Boolean, isDirectory :: Fn0 Boolean, isFile :: Fn0 Boolean, ctime :: JSDate, mtime :: JSDate, atime :: JSDate, size :: Number, ino :: Number, rdev :: Number, gid :: Number, uid :: Number, nlink :: Number, mode :: Number, dev :: Number }
```


#### `Stats`

``` purescript
data Stats
  = Stats StatsObj
```

#### `showStats`

``` purescript
instance showStats :: Show Stats
```


#### `isFile`

``` purescript
isFile :: Stats -> Boolean
```


#### `isDirectory`

``` purescript
isDirectory :: Stats -> Boolean
```


#### `isBlockDevice`

``` purescript
isBlockDevice :: Stats -> Boolean
```


#### `isCharacterDevice`

``` purescript
isCharacterDevice :: Stats -> Boolean
```


#### `isFIFO`

``` purescript
isFIFO :: Stats -> Boolean
```


#### `isSocket`

``` purescript
isSocket :: Stats -> Boolean
```


#### `isSymbolicLink`

``` purescript
isSymbolicLink :: Stats -> Boolean
```


#### `accessedTime`

``` purescript
accessedTime :: Stats -> Date
```


#### `modifiedTime`

``` purescript
modifiedTime :: Stats -> Date
```


#### `statusChangedTime`

``` purescript
statusChangedTime :: Stats -> Date
```



## Module Node.FS.Sync

#### `FileDescriptor`

``` purescript
data FileDescriptor :: *
```


#### `FileFlags`

``` purescript
data FileFlags
  = R 
  | R_PLUS 
  | RS 
  | RS_PLUS 
  | W 
  | WX 
  | W_PLUS 
  | WX_PLUS 
  | A 
  | AX 
  | A_PLUS 
  | AX_PLUS 
```


#### `BufferLength`

``` purescript
type BufferLength = Number
```


#### `BufferOffset`

``` purescript
type BufferOffset = Number
```


#### `ByteCount`

``` purescript
type ByteCount = Number
```


#### `FileMode`

``` purescript
type FileMode = Number
```


#### `FilePosition`

``` purescript
type FilePosition = Number
```


#### `rename`

``` purescript
rename :: forall eff. FilePath -> FilePath -> Eff (err :: Exception, fs :: FS | eff) Unit
```

#### `truncate`

``` purescript
truncate :: forall eff. FilePath -> Number -> Eff (err :: Exception, fs :: FS | eff) Unit
```

#### `chown`

``` purescript
chown :: forall eff. FilePath -> Number -> Number -> Eff (err :: Exception, fs :: FS | eff) Unit
```

#### `chmod`

``` purescript
chmod :: forall eff. FilePath -> Perms -> Eff (err :: Exception, fs :: FS | eff) Unit
```

#### `stat`

``` purescript
stat :: forall eff. FilePath -> Eff (err :: Exception, fs :: FS | eff) Stats
```

#### `link`

``` purescript
link :: forall eff. FilePath -> FilePath -> Eff (err :: Exception, fs :: FS | eff) Unit
```

#### `symlink`

``` purescript
symlink :: forall eff. FilePath -> FilePath -> SymlinkType -> Eff (err :: Exception, fs :: FS | eff) Unit
```

#### `readlink`

``` purescript
readlink :: forall eff. FilePath -> Eff (err :: Exception, fs :: FS | eff) FilePath
```

#### `realpath`

``` purescript
realpath :: forall eff. FilePath -> Eff (err :: Exception, fs :: FS | eff) FilePath
```

#### `realpath'`

``` purescript
realpath' :: forall eff cache. FilePath -> {  | cache } -> Eff (err :: Exception, fs :: FS | eff) FilePath
```

#### `unlink`

``` purescript
unlink :: forall eff. FilePath -> Eff (err :: Exception, fs :: FS | eff) Unit
```

#### `rmdir`

``` purescript
rmdir :: forall eff. FilePath -> Eff (err :: Exception, fs :: FS | eff) Unit
```

#### `mkdir`

``` purescript
mkdir :: forall eff. FilePath -> Eff (err :: Exception, fs :: FS | eff) Unit
```

#### `mkdir'`

``` purescript
mkdir' :: forall eff. FilePath -> Perms -> Eff (err :: Exception, fs :: FS | eff) Unit
```

#### `readdir`

``` purescript
readdir :: forall eff. FilePath -> Eff (err :: Exception, fs :: FS | eff) [FilePath]
```

#### `utimes`

``` purescript
utimes :: forall eff. FilePath -> Date -> Date -> Eff (err :: Exception, fs :: FS | eff) Unit
```

#### `readFile`

``` purescript
readFile :: forall eff. FilePath -> Eff (err :: Exception, fs :: FS | eff) Buffer
```

#### `readTextFile`

``` purescript
readTextFile :: forall eff. Encoding -> FilePath -> Eff (err :: Exception, fs :: FS | eff) String
```

#### `writeFile`

``` purescript
writeFile :: forall eff. FilePath -> Buffer -> Eff (err :: Exception, fs :: FS | eff) Unit
```

#### `writeTextFile`

``` purescript
writeTextFile :: forall eff. Encoding -> FilePath -> String -> Eff (err :: Exception, fs :: FS | eff) Unit
```

#### `appendFile`

``` purescript
appendFile :: forall eff. FilePath -> Buffer -> Eff (err :: Exception, fs :: FS | eff) Unit
```

#### `appendTextFile`

``` purescript
appendTextFile :: forall eff. Encoding -> FilePath -> String -> Eff (err :: Exception, fs :: FS | eff) Unit
```

#### `exists`

``` purescript
exists :: forall eff. FilePath -> Eff (fs :: FS | eff) Boolean
```

#### `fdOpen`

``` purescript
fdOpen :: forall opts eff. FilePath -> FileFlags -> Maybe FileMode -> Eff (fs :: FS, err :: Exception | eff) FileDescriptor
```

#### `fdRead`

``` purescript
fdRead :: forall eff. FileDescriptor -> Buffer -> BufferOffset -> BufferLength -> Maybe FilePosition -> Eff (fs :: FS, err :: Exception | eff) ByteCount
```

#### `fdNext`

``` purescript
fdNext :: forall eff. FileDescriptor -> Buffer -> Eff (fs :: FS, err :: Exception | eff) ByteCount
```

#### `fdWrite`

``` purescript
fdWrite :: forall eff. FileDescriptor -> Buffer -> BufferOffset -> BufferLength -> Maybe FilePosition -> Eff (fs :: FS, err :: Exception | eff) ByteCount
```

#### `fdAppend`

``` purescript
fdAppend :: forall eff. FileDescriptor -> Buffer -> Eff (fs :: FS, err :: Exception | eff) ByteCount
```

#### `fdFlush`

``` purescript
fdFlush :: forall eff. FileDescriptor -> Eff (fs :: FS, err :: Exception | eff) Unit
```

#### `fdClose`

``` purescript
fdClose :: forall eff. FileDescriptor -> Eff (fs :: FS, err :: Exception | eff) Unit
```