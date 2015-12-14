## Module Node.FS.Stream

#### `createWriteStream`

``` purescript
createWriteStream :: forall eff. FilePath -> Eff (fs :: FS | eff) (Writable () (fs :: FS | eff))
```

Create a Writable stream which writes data to the specified file, using
the default options.

#### `fdCreateWriteStream`

``` purescript
fdCreateWriteStream :: forall eff. FileDescriptor -> Eff (fs :: FS | eff) (Writable () (fs :: FS | eff))
```

Create a Writable stream which writes data to the specified file
descriptor, using the default options.

#### `WriteStreamOptions`

``` purescript
type WriteStreamOptions = { flags :: FileFlags, perms :: Perms }
```

#### `defaultWriteStreamOptions`

``` purescript
defaultWriteStreamOptions :: WriteStreamOptions
```

#### `createWriteStreamWith`

``` purescript
createWriteStreamWith :: forall eff. WriteStreamOptions -> FilePath -> Eff (fs :: FS | eff) (Writable () (fs :: FS | eff))
```

Like `createWriteStream`, but allows you to pass options.

#### `fdCreateWriteStreamWith`

``` purescript
fdCreateWriteStreamWith :: forall eff. WriteStreamOptions -> FileDescriptor -> Eff (fs :: FS | eff) (Writable () (fs :: FS | eff))
```

Like `fdCreateWriteStream`, but allows you to pass options.

#### `createReadStream`

``` purescript
createReadStream :: forall eff. FilePath -> Eff (fs :: FS | eff) (Readable () (fs :: FS | eff))
```

Create a Readable stream which reads data to the specified file, using
the default options.

#### `fdCreateReadStream`

``` purescript
fdCreateReadStream :: forall eff. FileDescriptor -> Eff (fs :: FS | eff) (Readable () (fs :: FS | eff))
```

Create a Readable stream which reads data to the specified file
descriptor, using the default options.

#### `ReadStreamOptions`

``` purescript
type ReadStreamOptions = { flags :: FileFlags, perms :: Perms, autoClose :: Boolean }
```

#### `defaultReadStreamOptions`

``` purescript
defaultReadStreamOptions :: ReadStreamOptions
```

#### `createReadStreamWith`

``` purescript
createReadStreamWith :: forall eff. ReadStreamOptions -> FilePath -> Eff (fs :: FS | eff) (Readable () (fs :: FS | eff))
```

Create a Readable stream which reads data from the specified file.

#### `fdCreateReadStreamWith`

``` purescript
fdCreateReadStreamWith :: forall eff. ReadStreamOptions -> FileDescriptor -> Eff (fs :: FS | eff) (Readable () (fs :: FS | eff))
```

Create a Readable stream which reads data from the specified file descriptor.


