# Module Documentation

## Module Node.FS

### Types

    data FS :: !


## Module Node.FS.Async

### Types

    type Callback eff a = Either Prim.String a -> Eff eff Unit


### Values

    readFile :: forall eff. FilePath -> Callback eff Buffer -> Eff (fs :: FS | eff) Unit

    readTextFile :: forall eff. Encoding -> FilePath -> Callback eff Prim.String -> Eff (fs :: FS | eff) Unit

    stat :: forall eff. FilePath -> Callback eff Stats -> Eff (fs :: FS | eff) Unit

    writeFile :: forall eff. FilePath -> Buffer -> Callback eff Unit -> Eff (fs :: FS | eff) Unit

    writeTextFile :: forall eff. Encoding -> FilePath -> Prim.String -> Callback eff Unit -> Eff (fs :: FS | eff) Unit


## Module Node.FS.Stats

### Types

    data Stats  where

    type StatsObj  = { isSocket :: Fn0 Prim.Boolean, isFIFO :: Fn0 Prim.Boolean, isCharacterDevice :: Fn0 Prim.Boolean, isBlockDevice :: Fn0 Prim.Boolean, isDirectory :: Fn0 Prim.Boolean, isFile :: Fn0 Prim.Boolean, size :: Prim.Number, ino :: Prim.Number, rdev :: Prim.Number, gid :: Prim.Number, uid :: Prim.Number, nlink :: Prim.Number, mode :: Prim.Number, dev :: Prim.Number }


### Type Class Instances

    instance showStats :: Show Stats


### Values

    isBlockDevice :: Stats -> Prim.Boolean

    isCharacterDevice :: Stats -> Prim.Boolean

    isDirectory :: Stats -> Prim.Boolean

    isFIFO :: Stats -> Prim.Boolean

    isFile :: Stats -> Prim.Boolean

    isSocket :: Stats -> Prim.Boolean