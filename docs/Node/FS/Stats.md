## Module Node.FS.Stats

#### `StatsObj`

``` purescript
type StatsObj = { dev :: Number, mode :: Number, nlink :: Number, uid :: Number, gid :: Number, rdev :: Number, ino :: Number, size :: Number, atime :: JSDate, mtime :: JSDate, ctime :: JSDate, isFile :: Fn0 Boolean, isDirectory :: Fn0 Boolean, isBlockDevice :: Fn0 Boolean, isCharacterDevice :: Fn0 Boolean, isFIFO :: Fn0 Boolean, isSocket :: Fn0 Boolean }
```

#### `Stats`

``` purescript
data Stats
  = Stats StatsObj
```

Stats wrapper to provide a usable interface to the underlying properties and methods.

##### Instances
``` purescript
Show Stats
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


