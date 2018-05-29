
module Node.FS.Internal where

import Prelude

import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)

mkEffect :: forall a. (Unit -> a) -> Effect a
mkEffect = unsafeCoerce

foreign import unsafeRequireFS :: forall props. { | props }
