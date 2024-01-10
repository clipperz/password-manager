module Views.SetPinView where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, fireOnce, loopW)
import Concur.React (HTML)
import Concur.React.DOM (h1, p, div, form, text, label, span, input, strong)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.Maybe (Maybe)
import Data.Ord ((>))
import Data.Semigroup ((<>))
import Data.String.CodeUnits (length)
import Functions.Communication.OneTimeShare (PIN)
import Functions.Pin (isPinValid)
import React (ReactElement)
import Unsafe.Coerce (unsafeCoerce)
import Views.SimpleWebComponents (simpleButton)

data PinEvent = Reset | SetPin PIN

setPinView :: Boolean -> Widget HTML PinEvent
setPinView pinExists = div [Props._id "pinPage"] [ form [] [
  h1 [] [text "Device PIN"]
, div [Props.className "description"] $ [
    p [] [text "You may create a 5-digit PIN to be used instead of your passphrase. Please note that the PIN is specific to the device you are now using."]
  , p [] [strong [] [text "Warning"], text ": enabling a PIN on your device may represent a security risk! Make sure to keep the device with you at all times!"]  
  ]
, setPinForm
]]

  where
    setPinForm :: Widget HTML PinEvent
    setPinForm = div [Props.className "content"] [
      text $ "PIN is " <> (if pinExists then "" else "not ") <> "set on this device"
    , do
        signalResult <- demand $ do
          pin <- loopW "" (\value -> do
            result <- label [Props.className "pin"] [
              span [Props.className "label"] [text "PIN"]
            , input [
                Props._type "text"
              , Props.inputMode "numeric"
              , Props.placeholder (if pinExists then "*****" else "PIN")
              , Props.value value
              , Props.disabled pinExists
              , Props.pattern "^[0-9]+$"
              , (\e -> 
                  if (unsafeCoerce e).target.validity.valid
                  then Props.unsafeTargetValue e
                  else value
                ) <$> Props.onChange
              ]
            ]
            pure $  if (length result) > 5
                    then value
                    else result
          )
          result :: Maybe PinEvent <- fireOnce (submitWidget pin)
          pure result
        pure signalResult
    ]

    submitWidget :: String -> Widget (Array ReactElement) PinEvent
    submitWidget pin =
      if   pinExists 
      then simpleButton "reset" "Reset"  false                  Reset
      else simpleButton "save"  "Save"  (not (isPinValid pin)) (SetPin pin)


