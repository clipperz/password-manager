module Views.ShareView where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, demand, fireOnce, loopS, loopW)
import Concur.React (HTML)
import Concur.React.DOM (button, div, form, h4, label, option, select, span, text, textarea)
import Concur.React.Props as Props
import Control.Alt ((<$), (<|>))
import Control.Alternative ((*>))
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
import Data.String.CodeUnits (singleton, toCharArray)
import Data.Time.Duration (Days(..), Hours(..), Minutes(..), Seconds, convertDuration)
import Data.Tuple (Tuple(..), fst, snd)
import DataModel.OneTimeShare (SecretData)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Functions.Clipboard (copyToClipboard)
import Functions.Password (randomPIN)
import Views.Components (Enabled(..), dynamicWrapper)
import Views.OverlayView (OverlayColor(..), OverlayStatus(..), overlay)

data Secret = SecretString String -- | SecretCard String

secretIsString :: Secret -> Boolean
secretIsString (SecretString _) = true
-- secretIsString (SecretCard   _) = false

secretIsCard :: Secret -> Boolean
secretIsCard = secretIsString >>> not

secretIsEmpty :: Secret -> Boolean
secretIsEmpty (SecretString s) = null s
-- secretIsEmpty (SecretCard   c) = null c

emptySecretData :: Aff SecretData
emptySecretData = do
  pin <- randomPIN 5
  pure {secret: "", pin: pin, duration: convertDuration $ Minutes 10.0}

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

shareView :: Boolean -> Secret -> SecretData -> Widget HTML SecretData
shareView enabled secret secretData = do
  form [Props.classList ([Just "shareForm"] <> [if enabled then Nothing else Just "disabled"])] [
    demand $ shareSignal enabled secret secretData
  ]

data PinSectionAction = RegeneratePin | CopyPin

shareSignal :: Boolean -> Secret -> SecretData -> Signal HTML (Maybe SecretData)
shareSignal enabled secret' secretData = do
  result <- loopS secretData (\{secret: secret_, pin: pin_, duration: duration_} -> do
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
      -- SecretCard   secret -> pure secret
    newPin <- loopW pin_ (\pin -> do
      result <- pinSection pin (Enabled (enabled && true))
      case result of
        RegeneratePin  -> "" <$ pinSection pin (Enabled false) <|> (liftAff $ randomPIN 5)
        CopyPin        -> "" <$ pinSection pin (Enabled false) <|> (liftAff $ pin <$ delay (Milliseconds 1000.0)) <|> (overlay { status: Copy, color: Black, message: "copied" })
    )
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
    pure $ computeSecretData secret' newSecret newPin newDuration
  )
  fireOnce $ button [
    Props.disabled (disableSubmitButton secret' result.secret || not enabled)
  , Props.className "submit", result <$ Props.onClick
  ] [text "create share link"]

  where
    pinSection :: String -> Enabled -> Widget HTML PinSectionAction
    pinSection pin (Enabled pinEnabled) =
      div [Props.className "pin"] [
        h4 [Props.className "label"] [text "PIN"]
      , div [Props.className "pinText"] ((\c -> span [] [text $ singleton c]) <$> toCharArray pin)
      , RegeneratePin <$ button [
                          Props._type "button"
                        , Props.disabled (not pinEnabled)
                        , Props.className "regeneratePin"
                        , Props.title "regenerate"
                        , Props.onClick
                        ] [span [] [text "generate password"]] 
      , CopyPin       <$ (button [
                          Props._type "button"
                        , Props.className "copyPin"
                        , Props.title "copy"
                        , Props.onClick
                        ] [span [] [text "copy"]] *> (liftAff $ copyToClipboard pin))
      ]

    disableSubmitButton :: Secret -> String -> Boolean
    disableSubmitButton secret resultSecret =
      case secret of
        SecretString s -> (null s && null resultSecret)
        -- SecretCard _   -> false
    
    computeSecretData :: Secret -> String -> String -> Seconds -> SecretData
    computeSecretData secret newSecret newPin newDuration =
      case secret of
        SecretString s -> {secret: if (not null s) then s else newSecret, pin: newPin, duration: newDuration}
        -- SecretCard   _ -> {secret: newSecret, pin: newPin, duration: newDuration}