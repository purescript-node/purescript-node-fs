module Node.FS.Dirent
  ( Dirent
  , DirentName
  , DirentNameString
  , DirentNameBuffer
  , DirentType(..)
  , getType
  , class IsDirentNameForDirent
  , name
  , parentPath
  ) where

import Prelude

import Node.Buffer (Buffer)
import Node.Path (FilePath)
import Partial.Unsafe (unsafeCrashWith)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data DirentName

foreign import data DirentNameString :: DirentName
foreign import data DirentNameBuffer :: DirentName

foreign import data Dirent :: DirentName -> Type

foreign import isBlockDevice :: forall direntnametype. Dirent direntnametype -> Boolean
foreign import isCharacterDevice :: forall direntnametype. Dirent direntnametype -> Boolean
foreign import isDirectory :: forall direntnametype. Dirent direntnametype -> Boolean
foreign import isFIFO :: forall direntnametype. Dirent direntnametype -> Boolean
foreign import isFile :: forall direntnametype. Dirent direntnametype -> Boolean
foreign import isSocket :: forall direntnametype. Dirent direntnametype -> Boolean
foreign import isSymbolicLink :: forall direntnametype. Dirent direntnametype -> Boolean

data DirentType
  = DirentType_BlockDevice -- Dirent object describes a block device.
  | DirentType_CharacterDevice -- Dirent object describes a character device.
  | DirentType_Directory -- Dirent object describes a directory.
  | DirentType_FIFO -- Dirent object describes a FIFO pipe.
  | DirentType_File -- Dirent object describes a regular file.
  | DirentType_Socket -- Dirent object describes a socket.
  | DirentType_SymbolicLink -- Dirent object describes a symbolic link.

derive instance Eq DirentType
derive instance Ord DirentType
derive instance Generic DirentType _
instance Show DirentType where
  show = genericShow

getType :: forall direntnametype. Dirent direntnametype -> DirentType
getType dirent =
  if isBlockDevice dirent then DirentType_BlockDevice
  else if isCharacterDevice dirent then DirentType_CharacterDevice
  else if isDirectory dirent then DirentType_Directory
  else if isFIFO dirent then DirentType_FIFO
  else if isFile dirent then DirentType_File
  else if isSocket dirent then DirentType_Socket
  else if isSymbolicLink dirent then DirentType_SymbolicLink
  else unsafeCrashWith ("Impossible: unknown Dirent type for " <> show dirent)

foreign import nameImpl :: forall direntnametype nametype. Dirent direntnametype -> nametype

class IsDirentNameForDirent direntnametype stringOrBuffer | direntnametype -> stringOrBuffer where
  -- | The file name that this <fs.Dirent> object refers to. The type of this value is determined by the options.encoding passed to fs.readdir() or fs.readdirSync().
  name :: Dirent direntnametype -> stringOrBuffer

instance IsDirentNameForDirent DirentNameString String where
  name :: Dirent DirentNameString -> String
  name = nameImpl
else instance IsDirentNameForDirent DirentNameBuffer Buffer where
  name :: Dirent DirentNameBuffer -> Buffer
  name = nameImpl

-- | Get the parent directory path of the file this Dirent object refers to.
-- | Added in: v21.4.0, v20.12.0, v18.20.0
-- | TODO: support for old node version? `Nullable FilePath`?
foreign import parentPath :: forall direntnametype. Dirent direntnametype -> FilePath

foreign import showDirentObj :: forall direntnametype. Dirent direntnametype -> String

instance Show (Dirent direntnametype) where
  show s = showDirentObj s
