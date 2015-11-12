## Module Node.FS

#### `FS`

``` purescript
data FS :: !
```

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

#### `fileFlagsToNode`

``` purescript
fileFlagsToNode :: FileFlags -> String
```

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

##### Instances
``` purescript
instance showSymlinkType :: Show SymlinkType
instance eqSymlinkType :: Eq SymlinkType
```

#### `symlinkTypeToNode`

``` purescript
symlinkTypeToNode :: SymlinkType -> String
```


