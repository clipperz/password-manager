module Views.ShareView where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, demand, fireOnce, loopS, loopW)
import Concur.React (HTML)
import Concur.React.DOM (div, form, input, label, option, select, span, text, textarea)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Array (filter, head)
import Data.Eq ((/=), (==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra ((||))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (null)
import Data.Time.Duration (Days(..), Hours(..), Minutes(..), convertDuration)
import Data.Tuple (Tuple(..), fst, snd)
import Functions.Communication.OneTimeShare (SecretData)
import Views.Components (dynamicWrapper)
import Views.SimpleWebComponents (simpleButton)

data Secret = SecretString String | SecretCard String

expirationPeriods :: Array (Tuple Days String)
expirationPeriods = [ Tuple (convertDuration (Days    7.0)) ("1W")
                    , Tuple (convertDuration (Days    1.0)) ("1D")
                    , Tuple (convertDuration (Hours   1.0)) ("1H")
                    , Tuple (convertDuration (Minutes 1.0)) ("1M")
                    , Tuple (convertDuration (Hours   0.0)) ("")
                    ]

getDurationFromLabel :: String -> Days
getDurationFromLabel label = fromMaybe (Days 0.0) (fst <$> head (filter (\(Tuple _ label_) -> label_ == label) expirationPeriods))

getLabelFromDuration :: Days -> String
getLabelFromDuration duration = fromMaybe ("duration") (snd <$> head (filter (\(Tuple duration_ _) -> duration_ == duration) expirationPeriods))

shareView :: Secret -> Widget HTML SecretData
shareView dataSecret = do
  form [Props.className "shareForm"] [
    demand $ shareSignal dataSecret
  ]

shareSignal :: Secret -> Signal HTML (Maybe SecretData)
shareSignal dataSecret = do
  result <- loopS {secret: "", password: "", duration: Days 0.0} (\{secret: secret_, password: password_, duration: duration_} -> do
    newSecret <- case dataSecret of
      SecretString secret -> (loopW secret_ (\value -> dynamicWrapper Nothing value $ 
                                label [] [
                                  span [Props.className "label"] [text "Secret"]
                                , Props.unsafeTargetValue <$> textarea [
                                    Props.value (if (secret /= "") then secret else value)
                                  , Props.disabled (secret /= "")
                                  , Props.onChange
                                  , Props.autoComplete "off", Props.autoCorrect "off", Props.autoCapitalize "off", Props.spellCheck false
                                  , Props.placeholder "secret"
                                  ] []
                                ])
                              )
      SecretCard   secret -> pure secret
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
    newDuration <- loopW duration_ (\duration -> do
      getDurationFromLabel <$> label [] [
          span [Props.className "label"] [text "Duration"]
        , select [
            Props.value (getLabelFromDuration duration) 
          , Props.unsafeTargetValue <$> Props.onChange
          ] ((\(Tuple _ label_) -> option [] [text label_]) <$> expirationPeriods)
      ]
    )
    pure {secret: newSecret, password: newPassword, duration: newDuration}
  )
  fireOnce (simpleButton "submit" "submit" ((null (result.secret)) || (null (result.password))) result)

simpleSecretSignal :: String -> Signal HTML String
simpleSecretSignal secret =
  loopW secret (\v -> div [] [
    label [] [
        span [Props.className "label"] [text "Secret"]
      , (Props.unsafeTargetValue) <$> input [
          Props._type "text"
        , Props.placeholder "secret"
        , Props.value v
        , Props.autoComplete "off", Props.autoCorrect "off", Props.autoCapitalize "off", Props.spellCheck false
        , Props.onChange
        ]
      ]
  ])