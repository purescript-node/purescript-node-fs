# Module Documentation

## Module Node.FS

### Types

    data FS :: !


## Module Node.FS.Async

### Types

    type Callback eff a = Either String a -> Eff eff Unit


### Values

    readFile :: forall eff. FilePath -> Callback eff Buffer -> Eff (fs :: FS | eff) Unit

    readTextFile :: forall eff. Encoding -> FilePath -> Callback eff String -> Eff (fs :: FS | eff) Unit

    rename :: forall eff. FilePath -> FilePath -> Callback eff Unit -> Eff (fs :: FS | eff) Unit

    stat :: forall eff. FilePath -> Callback eff Stats -> Eff (fs :: FS | eff) Unit

    truncate :: forall eff. FilePath -> Number -> Callback eff Unit -> Eff (fs :: FS | eff) Unit

    writeFile :: forall eff. FilePath -> Buffer -> Callback eff Unit -> Eff (fs :: FS | eff) Unit

    writeTextFile :: forall eff. Encoding -> FilePath -> String -> Callback eff Unit -> Eff (fs :: FS | eff) Unit


## Module Node.FS.Stats

### Types

    data Stats where
      Stats :: StatsObj -> Stats

    type StatsObj  = { isSocket :: Fn0 Boolean, isFIFO :: Fn0 Boolean, isCharacterDevice :: Fn0 Boolean, isBlockDevice :: Fn0 Boolean, isDirectory :: Fn0 Boolean, isFile :: Fn0 Boolean, ctime :: JSDate, mtime :: JSDate, atime :: JSDate, size :: Number, ino :: Number, rdev :: Number, gid :: Number, uid :: Number, nlink :: Number, mode :: Number, dev :: Number }


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

### Values

    readFile :: forall eff. FilePath -> Eff (err :: Exception Error, fs :: FS | eff) Buffer

    readTextFile :: forall eff. Encoding -> FilePath -> Eff (err :: Exception Error, fs :: FS | eff) String

    rename :: forall eff. FilePath -> FilePath -> Eff (fs :: FS | eff) Unit

    truncate :: forall eff. FilePath -> Number -> Eff (fs :: FS | eff) Unit