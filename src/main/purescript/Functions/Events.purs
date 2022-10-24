module Functions.Events where

import Effect (Effect)
import Effect.Aff (Aff)
import React.SyntheticEvent (NativeEventTarget)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

foreign import _readFile :: NativeEventTarget -> EffectFnAff String

readFile :: NativeEventTarget -> Aff String
readFile ev = fromEffectFnAff (_readFile ev)


