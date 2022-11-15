module Functions.Events where

import Effect.Aff (Aff)
import React.SyntheticEvent (SyntheticEvent_, NativeEventTarget)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Web.DOM.Element (Element)

foreign import renderElement :: Element -> String
foreign import _readFile :: NativeEventTarget -> EffectFnAff String
foreign import _readFileFromDrop :: forall r. SyntheticEvent_ (currentTarget :: NativeEventTarget | r) -> EffectFnAff String

readFile :: NativeEventTarget -> Aff String
readFile ev = fromEffectFnAff (_readFile ev)

readFileFromDrop :: forall r. SyntheticEvent_ (currentTarget :: NativeEventTarget | r) -> Aff String
readFileFromDrop ev = fromEffectFnAff (_readFileFromDrop ev)
