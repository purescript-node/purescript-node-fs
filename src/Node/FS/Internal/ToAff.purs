module Node.FS.Internal.ToAff where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Node.FS.Internal.Callbacks (Callback1)

toAff
  :: forall a
   . (Callback1 a -> Effect Unit)
  -> Aff a
toAff p = makeAff \k -> p k $> nonCanceler

toAff1
  :: forall a x
   . (x -> Callback1 a -> Effect Unit)
  -> x
  -> Aff a
toAff1 f a = toAff (f a)

toAff2
  :: forall a x y
   . (x -> y -> Callback1 a -> Effect Unit)
  -> x
  -> y
  -> Aff a
toAff2 f a b = toAff (f a b)

toAff3
  :: forall a x y z
   . (x -> y -> z -> Callback1 a -> Effect Unit)
  -> x
  -> y
  -> z
  -> Aff a
toAff3 f a b c = toAff (f a b c)

toAff4
  :: forall a v x y z
   . (v -> x -> y -> z -> Callback1 a -> Effect Unit)
  -> v
  -> x
  -> y
  -> z
  -> Aff a
toAff4 f a b c d = toAff (f a b c d)

toAff5
  :: forall a w v x y z
   . (w -> v -> x -> y -> z -> Callback1 a -> Effect Unit)
  -> w
  -> v
  -> x
  -> y
  -> z
  -> Aff a
toAff5 f a b c d e = toAff (f a b c d e)

toAff6
  :: forall a w v x y z t
   . (w -> v -> x -> y -> z -> t -> Callback1 a -> Effect Unit)
  -> w
  -> v
  -> x
  -> y
  -> z
  -> t
  -> Aff a
toAff6 f a b c d e t = toAff (f a b c d e t)

