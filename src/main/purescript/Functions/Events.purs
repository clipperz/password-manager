module Functions.Events
  ( getClickCoordinates
  , printEvent
  , readFile
  , readFileFromDrop
  , renderElement
  )
  where

import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff)
import React.SyntheticEvent (SyntheticEvent_, SyntheticMouseEvent, NativeEventTarget)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Web.DOM.Element (Element)

foreign import renderElement :: Element -> String
foreign import _readFile :: NativeEventTarget -> EffectFnAff String
foreign import _readFileFromDrop :: forall r. SyntheticEvent_ (currentTarget :: NativeEventTarget | r) -> EffectFnAff String

foreign import _getXClickCoordinates :: SyntheticMouseEvent -> Int
foreign import _getYClickCoordinates :: SyntheticMouseEvent -> Int

foreign import printEvent :: forall r. SyntheticEvent_ (currentTarget :: NativeEventTarget | r) -> Effect Unit

readFile :: NativeEventTarget -> Aff String
readFile ev = fromEffectFnAff (_readFile ev)

readFileFromDrop :: forall r. SyntheticEvent_ (currentTarget :: NativeEventTarget | r) -> Aff String
readFileFromDrop ev = fromEffectFnAff (_readFileFromDrop ev)

getClickCoordinates :: SyntheticMouseEvent -> Tuple Int Int
getClickCoordinates ev = Tuple (_getXClickCoordinates ev) (_getYClickCoordinates ev)
