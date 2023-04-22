module Views.ShareView where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, fireOnce, loopS, loopW)
import Concur.React (HTML)
import Concur.React.DOM (div, form, input, label, span, text)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra ((||))
import Data.Maybe (Maybe, fromMaybe, isJust)
import Data.String (null)
import Data.Tuple (Tuple(..), fst, snd)
import Views.SimpleWebComponents (simpleButton)

shareView :: Maybe String -> Widget HTML (Tuple String String)
shareView maybeSecret = do
  form [Props.className "form"] [
    demand $ do
      result <- loopS (Tuple (fromMaybe "" maybeSecret) "") (\(Tuple secret_ password_) -> do
        newSecret <- loopW secret_ (\v -> div [] [
          label [] [
              span [Props.className "label"] [text "Secret"]
            , (Props.unsafeTargetValue) <$> input [
                Props._type "text"
              , Props.placeholder "secret"
              , Props.value v
              , Props.autoComplete "off", Props.autoCorrect "off", Props.autoCapitalize "off", Props.spellCheck false
              , Props.disabled (isJust maybeSecret)
              , Props.onChange
              ]
            ]
        ])
        newPassword <- loopW password_ (\v -> div [] [
          label [] [
              span [Props.className "label"] [text "Password"]
            , (Props.unsafeTargetValue) <$> input [
                Props._type "password"
              , Props.placeholder "password"
              , Props.value v
              , Props.autoComplete "off", Props.autoCorrect "off", Props.autoCapitalize "off", Props.spellCheck false
              , Props.disabled false
              , Props.onChange
              ]
            ]
        ])
        pure (Tuple newSecret newPassword)
      )
      fireOnce (simpleButton "submit" "submit" ((null (fst result)) || (null (snd result))) result)
  ]