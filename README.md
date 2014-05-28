# Module Documentation

## Module Node.FS

### Types

    type Callback eff a = Either Prim.String a -> Eff eff Unit

    data FS :: !

    data Stats  where


### Type Class Instances

    instance showStats :: Show Stats


### Values

    isBlockDevice :: Stats -> Prim.Boolean

    isCharacterDevice :: Stats -> Prim.Boolean

    isDirectory :: Stats -> Prim.Boolean

    isFIFO :: Stats -> Prim.Boolean

    isFile :: Stats -> Prim.Boolean

    isSocket :: Stats -> Prim.Boolean

    readFile :: forall eff. FilePath -> Callback eff Buffer -> Eff (fs :: FS | eff) Unit

    readTextFile :: forall eff. Encoding -> FilePath -> Callback eff Prim.String -> Eff (fs :: FS | eff) Unit

    stat :: forall eff. FilePath -> Callback eff Stats -> Eff (fs :: FS | eff) Unit

    writeFile :: forall eff. FilePath -> Buffer -> Callback eff Unit -> Eff (fs :: FS | eff) Unit

    writeTextFile :: forall eff. Encoding -> FilePath -> Prim.String -> Callback eff Unit -> Eff (fs :: FS | eff) Unit