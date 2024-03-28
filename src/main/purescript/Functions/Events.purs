module Functions.Events
  ( _getYClickCoordinates
  , blur
  , cursorToEnd
  , focus
  , getClickCoordinates
  , getWindowMessage
  , keyboardShortcut
  , printEvent
  , readFile
  , readFileFromDrop
  , renderElement
  )
  where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import React.SyntheticEvent (SyntheticEvent_, SyntheticMouseEvent, NativeEventTarget)
import Web.DOM.Element (Element)

foreign import renderElement :: Element -> String
foreign import _readFile :: NativeEventTarget -> EffectFnAff String
foreign import _readFileFromDrop :: forall r. SyntheticEvent_ (currentTarget :: NativeEventTarget | r) -> EffectFnAff String

foreign import _getWindowMessage :: Unit -> EffectFnAff String

foreign import _keyboardShortcut :: Array String -> EffectFnAff Unit

foreign import _getXClickCoordinates :: SyntheticMouseEvent -> Int
foreign import _getYClickCoordinates :: SyntheticMouseEvent -> Int

foreign import printEvent :: forall r. SyntheticEvent_ (currentTarget :: NativeEventTarget | r) -> Effect Unit

foreign import cursorToEnd :: forall r. SyntheticEvent_ (currentTarget :: NativeEventTarget | r) -> Effect Unit

foreign import focus :: String -> Effect Unit
foreign import blur :: String -> Effect Unit

readFile :: NativeEventTarget -> Aff String
readFile ev = fromEffectFnAff (_readFile ev)

readFileFromDrop :: forall r. SyntheticEvent_ (currentTarget :: NativeEventTarget | r) -> Aff String
readFileFromDrop ev = fromEffectFnAff (_readFileFromDrop ev)

getClickCoordinates :: SyntheticMouseEvent -> Tuple Int Int
getClickCoordinates ev = Tuple (_getXClickCoordinates ev) (_getYClickCoordinates ev)

getWindowMessage :: Aff String
getWindowMessage = fromEffectFnAff (_getWindowMessage unit)

keyboardShortcut :: Array String -> Aff Unit
keyboardShortcut = fromEffectFnAff <<< _keyboardShortcut