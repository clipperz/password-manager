module Views.ShareView where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, demand, fireOnce, loopS, loopW)
import Concur.React (HTML)
import Concur.React.DOM (div, form, input, label, span, text, textarea)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Argonaut.Decode (fromJsonString)
import Data.Either (Either(..))
import Data.Eq ((/=))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra (not, (||))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (null)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unit (unit)
import DataModel.Card (Card(..))
import JSURI (decodeURI)
import Views.CardViews (cardContent)
import Views.Components (dynamicWrapper)
import Views.SimpleWebComponents (simpleButton)

shareView :: String -> Widget HTML (Tuple String String)
shareView secret = form [Props.className "form"] [
    demand $ do
      let decodedSecret = fromMaybe secret (decodeURI secret)
      result <- loopS (Tuple secret "") (\(Tuple secret_ password_) -> do
        newSecret <- case fromJsonString decodedSecret of
          Right (Card {content}) -> (decodedSecret <$ loopW unit (\_ -> cardContent content))
          Left _                 -> (loopW secret_ (\value -> dynamicWrapper Nothing value $ 
                                      label [] [
                                        span [Props.className "label"] [text "Secret"]
                                      , Props.unsafeTargetValue <$> textarea [
                                          Props.value value
                                        , Props.disabled (secret /= "")
                                        , Props.onChange
                                        , Props.placeholder "secret"
                                        ] []
                                      ])
                                    )
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

simpleSecretSignal :: String -> Boolean -> Signal HTML String
simpleSecretSignal secret enabled =
  loopW secret (\v -> div [] [
    label [] [
        span [Props.className "label"] [text "Secret"]
      , (Props.unsafeTargetValue) <$> input [
          Props._type "text"
        , Props.placeholder "secret"
        , Props.value v
        , Props.autoComplete "off", Props.autoCorrect "off", Props.autoCapitalize "off", Props.spellCheck false
        , Props.disabled (not enabled)
        , Props.onChange
        ]
      ]
  ])