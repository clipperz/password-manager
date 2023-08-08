module Views.ShareView where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, demand, fireOnce, loopS, loopW)
import Concur.React (HTML)
import Concur.React.DOM (div, form, label, option, select, span, text, textarea)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Semigroupoid ((>>>))
import Data.Array (filter, head)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not, (&&), (||))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Semigroup ((<>))
import Data.String (null)
import Data.Time.Duration (Days(..), Hours(..), Minutes(..), Seconds, convertDuration)
import Data.Tuple (Tuple(..), fst, snd)
import Functions.Communication.OneTimeShare (SecretData)
import Views.Components (dynamicWrapper)
import Views.SimpleWebComponents (simpleButton)

data Secret = SecretString String | SecretCard String

secretIsString :: Secret -> Boolean
secretIsString (SecretString _) = true
secretIsString (SecretCard   _) = false

secretIsCard :: Secret -> Boolean
secretIsCard = secretIsString >>> not

secretIsEmpty :: Secret -> Boolean
secretIsEmpty (SecretString s) = null s
secretIsEmpty (SecretCard   c) = null c

emptySecretData :: SecretData
emptySecretData = {secret: "", duration: convertDuration $ Minutes 10.0}

expirationPeriods :: Array (Tuple Seconds String)
expirationPeriods = [ Tuple (convertDuration (Days    7.0))  ("1 Week")
                    , Tuple (convertDuration (Days    1.0))  ("1 Day")
                    , Tuple (convertDuration (Hours   1.0))  ("1 Hour")
                    , Tuple (convertDuration (Minutes 10.0)) ("10 Minutes")
                    , Tuple (convertDuration (Minutes 1.0))  ("1 Minute")
                    ]

getDurationFromLabel :: String -> Seconds
getDurationFromLabel label = fromMaybe (convertDuration $ Minutes 1.0) (fst <$> head (filter (\(Tuple _ label_) -> label_ == label) expirationPeriods))

getLabelFromDuration :: Seconds -> String
getLabelFromDuration duration = fromMaybe ("Never") (snd <$> head (filter (\(Tuple duration_ _) -> duration_ == duration) expirationPeriods))

shareView :: Boolean -> SecretData -> Secret -> Widget HTML SecretData
shareView enabled secret secretData = do
  form [Props.classList ([Just "shareForm"] <> [if enabled then Nothing else Just "disabled"])] [
    demand $ shareSignal enabled secret secretData 
  ]

shareSignal :: Boolean -> SecretData -> Secret -> Signal HTML (Maybe SecretData)
shareSignal enabled secretData secret' = do
  result <- loopS secretData (\{secret: secret_, duration: duration_} -> do
    newSecret <- case secret' of
      SecretString secret -> (loopW secret_ (\value -> label [] [
                                  span [Props.className "label"] [text "Secret"]
                                , dynamicWrapper Nothing value $ 
                                    Props.unsafeTargetValue <$> textarea [
                                      Props.value (if (not null secret) then secret else value)
                                    , Props.disabled (not null secret || not enabled)
                                    , Props.onChange
                                    , Props.autoComplete "off", Props.autoCorrect "off", Props.autoCapitalize "off", Props.spellCheck false
                                    , Props.placeholder "secret"
                                    ] []
                                ])
                              )
      SecretCard   secret -> pure secret
    newDuration <- loopW duration_ (\duration -> do
      getDurationFromLabel <$> div [Props.className "duration"] [
        label [] [
          span [Props.className "label"] [text "Expires in"]
        , select [
            Props.value (getLabelFromDuration duration) 
          , Props.unsafeTargetValue <$> Props.onChange
          , Props.disabled (not enabled)
          ] ((\(Tuple _ label_) -> option [] [text label_]) <$> expirationPeriods)
        ]
      ]
    )
    pure $ computeSecretData secret' newSecret newDuration
  )
  fireOnce (simpleButton "submit" "submit" (disableSubmitButton secret' result.secret || not enabled) result)

  where
    disableSubmitButton :: Secret -> String -> Boolean
    disableSubmitButton secret resultSecret =
      case secret of
        SecretString s -> (null s && null resultSecret)
        SecretCard _   -> false
    
    computeSecretData :: Secret -> String -> Seconds -> SecretData
    computeSecretData secret newSecret newDuration =
      case secret of
        SecretString s -> {secret: if (not null s) then s else newSecret, duration: newDuration}
        SecretCard   _ -> {secret: newSecret, duration: newDuration}