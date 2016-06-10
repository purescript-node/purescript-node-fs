
module Node.FS.Internal where

import Prelude
import Control.Monad.Eff (Eff)
import Unsafe.Coerce (unsafeCoerce)

mkEff :: forall e a. (Unit -> a) -> Eff e a
mkEff = unsafeCoerce

foreign import unsafeRequireFS :: forall props. { | props }
