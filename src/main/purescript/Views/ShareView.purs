module Views.ShareView where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, demand, fireOnce, loopS, loopW)
import Concur.React (HTML)
import Concur.React.DOM (div, form, input, label, span, text)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Argonaut.Decode (fromJsonString)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra (not, (||))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Semigroup ((<>))
import Data.String (null)
import Data.Tuple (Tuple(..), fst, snd)
import DataModel.Card (Card(..))
import JSURI (decodeURI)
import Views.CardViews (cardContent)
import Views.SimpleWebComponents (simpleButton)

shareView :: Maybe String -> Widget HTML (Tuple String String)
shareView maybeSecret = form [Props.className "form"] [
    demand $ do
      result <- loopS (Tuple (fromMaybe "" maybeSecret) "") (\(Tuple secret_ password_) -> do
        newSecret <-   case maybeSecret of
          Nothing -> simpleSecretSignal secret_ (isNothing maybeSecret)
          Just secret -> do
            let decodedSecret = fromMaybe secret (decodeURI secret)
            case fromJsonString decodedSecret of
              Right (Card {content}) -> (decodedSecret <$ loopW "" (\_ -> cardContent content))
              Left _                 -> (decodedSecret <$ loopW "" (\_  -> text ("Secret: " <> decodedSecret)))
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