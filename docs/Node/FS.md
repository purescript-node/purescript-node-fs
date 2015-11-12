## Module Node.FS

#### `FS`

``` purescript
data FS :: !
```

Effect type for file system usage.

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

##### Instances
``` purescript
instance showFileFlags :: Show FileFlags
instance eqFileFlags :: Eq FileFlags
```

#### `fileFlagsToNode`

``` purescript
fileFlagsToNode :: FileFlags -> String
```

Convert a `FileFlags` to a `String` in the format expected by the Node.js
filesystem API.

#### `FileMode`

``` purescript
type FileMode = Int
```

#### `FilePosition`

``` purescript
type FilePosition = Int
```

#### `BufferLength`

``` purescript
type BufferLength = Int
```

#### `BufferOffset`

``` purescript
type BufferOffset = Int
```

#### `ByteCount`

``` purescript
type ByteCount = Int
```

#### `SymlinkType`

``` purescript
data SymlinkType
  = FileLink
  | DirLink
  | JunctionLink
```

Symlink varieties.

##### Instances
``` purescript
instance showSymlinkType :: Show SymlinkType
instance eqSymlinkType :: Eq SymlinkType
```

#### `symlinkTypeToNode`

``` purescript
symlinkTypeToNode :: SymlinkType -> String
```

Convert a `SymlinkType` to a `String` in the format expected by the
Node.js filesystem API.


