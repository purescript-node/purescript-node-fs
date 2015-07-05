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
readdir :: forall eff. FilePath -> Callback eff (Array FilePath) -> Eff (fs :: FS | eff) Unit
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


