module Views.SimpleWebComponents where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, loopW, loopS, display)
import Concur.React (HTML)
import Concur.React.DOM (text, textarea, input, label, div', div, button, li)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind, discard, (=<<))
import Control.Semigroupoid ((<<<))
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$), (<$>))
import Data.HeytingAlgebra (not)
import Data.Map (Map, lookup)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Events (readFile)
import Functions.Password (PasswordStrengthFunction, PasswordStrength)
import React.SyntheticEvent (currentTarget, SyntheticEvent_, NativeEventTarget)

simpleTextAreaWidget :: String -> Widget HTML String -> String -> String -> Widget HTML String
simpleTextAreaWidget id lbl placeholder content = do
  div [Props.className "textarea"] [
    label [Props.htmlFor id, Props.className "hide-element"] [lbl]
    , Props.unsafeTargetValue <$> textarea [
        Props.value content
      , Props.onChange
      , Props.placeholder placeholder
    ] []
  ]

simpleInputWidget :: String -> Widget HTML String -> Boolean -> String -> String -> String -> Widget HTML String
simpleInputWidget id lbl disable placeholder value t = do
  res <- div' [
      label [Props.htmlFor id] [lbl]
    , (Props.unsafeTargetValue) <$> input [
        Props._type t
      , Props._id id
      , Props.placeholder placeholder
      , Props.value value
      , Props.disabled disable
      , Props.onChange
      ]
  ]
  pure res

simpleFileInputWidget :: String -> Widget HTML String -> Widget HTML String
simpleFileInputWidget id lbl = do
  div' [
      label [Props.htmlFor id] [lbl]
    , fromSyntheticEvent =<< input [
        Props._type "file"
      , Props._id id
      , Props.onChange
      ]
  ]

  where 
    fromSyntheticEvent :: forall r. SyntheticEvent_ (currentTarget :: NativeEventTarget | r) -> Widget HTML String
    fromSyntheticEvent se = do
      nve <- liftEffect $ currentTarget se
      liftAff $ readFile nve

simpleTextInputWidgetWithFocus :: String -> Widget HTML String -> String -> String -> Widget HTML String
simpleTextInputWidgetWithFocus id lbl placeholder s = do
  res <- div' [
      label [Props.htmlFor id] [lbl]
    , input [
        Props._type "text"
      , Props._id id
      , Props.placeholder placeholder
      , Props.value s
      , Props.disabled false
      , Props.unsafeTargetValue <$> Props.onChange
      , Props.unsafeTargetValue <$> Props.onFocus
      ]
  ]
  pure res

simpleTextInputWidget :: String -> Widget HTML String -> String -> String -> Widget HTML String
simpleTextInputWidget id lbl placeholder s = simpleInputWidget id lbl false placeholder s "text"

disabledSimpleTextInputWidget :: String -> Widget HTML String -> Boolean -> String -> String -> Widget HTML String
disabledSimpleTextInputWidget id lbl disable placeholder s = simpleInputWidget id lbl disable placeholder s "text"

simplePasswordInputWidget :: String -> Widget HTML String -> String -> Widget HTML String
simplePasswordInputWidget id lbl s = simpleInputWidget id lbl false "password" s "password"

simpleNumberInputWidget :: String -> Widget HTML String -> String -> String -> Widget HTML String
simpleNumberInputWidget id lbl placeholder s = simpleInputWidget id lbl false placeholder s "number"

simpleCheckboxWidget :: String -> Widget HTML Boolean -> Boolean -> Boolean -> Widget HTML Boolean 
simpleCheckboxWidget id lbl lblOnLeft v = do
  res <- if lblOnLeft then div' [
    (not v) <$ input [
        Props._type "checkbox"
      , Props._id id 
      , Props.checked v
      , Props.onChange
      ]
    , label [Props.htmlFor id] [lbl]
  ]
  
  else div' [
      label [Props.htmlFor id] [lbl]
    , (not v) <$ input [
        Props._type "checkbox"
      , Props._id id 
      , Props.checked v
      , Props.onChange
      ]
  ]
  pure res

simpleButton :: forall a. String -> Boolean -> a -> Widget HTML a
simpleButton label disable value = button [value <$ Props.onClick, Props.disabled disable] [text label]

simpleTextAreaSignal :: String -> Widget HTML String -> String -> String -> Signal HTML String
simpleTextAreaSignal id label placeholder content = loopW content (simpleTextAreaWidget id label placeholder)

simpleUserSignal :: String -> Signal HTML String
simpleUserSignal u = loopW u (simpleTextInputWidget "username" (text "Username") "username")

simplePasswordSignal :: String -> Signal HTML String
simplePasswordSignal p = loopW p (simplePasswordInputWidget "password" (text "Password"))

simpleCheckboxSignal :: String -> Widget HTML Boolean -> Boolean -> Boolean -> Signal HTML Boolean
simpleCheckboxSignal id lbl lblOnLeft v = loopW v (simpleCheckboxWidget id lbl lblOnLeft)

type PasswordForm = { password       :: String
                    , verifyPassword :: String
                    }
simpleVerifiedPasswordSignal :: PasswordStrengthFunction -> Either PasswordForm String -> Signal HTML (Either PasswordForm String)
simpleVerifiedPasswordSignal psf f = loopS f $ \ef ->
  case ef of
    Left { password, verifyPassword } -> go password verifyPassword
    Right p -> go p p
    where go p vp = do
                      pswd <- loopW p (simplePasswordInputWidget "password" (text "Password"))
                      display $ passwordStrengthShow $ psf pswd
                      pswd2 <- loopW vp (simplePasswordInputWidget "verify_password" (text "Verify password"))
                      display $ text $ if pswd == pswd2 then "The passwords are the same" else "The passwords are not the same"
                      if pswd == pswd2 then
                        pure $ Right pswd
                      else 
                        pure $ Left { password: pswd, verifyPassword: pswd2 }

checkboxesSignal :: Array (Tuple String Boolean) ->  Map String (Widget HTML Boolean) -> Signal HTML (Array (Tuple String Boolean))
checkboxesSignal ts lablesMap = loopS ts \m -> do
  let checkboxes = ((\(Tuple id value) -> Tuple id (simpleCheckboxSignal id (fromMaybe (text "Label not found") (lookup id lablesMap)) false value)) <$> m) :: Array (Tuple String (Signal HTML Boolean))
  let checkboxes2 = ((\(Tuple id s) -> do
                            res <- s
                            pure $ Tuple id res) <$> checkboxes) :: Array (Signal HTML (Tuple String Boolean))
  sequence checkboxes2

clickableListItemWidget :: forall a. Boolean -> Widget HTML a -> Array String -> a -> Widget HTML a
clickableListItemWidget disable item classes reference = 
  let classProps = Props.className <$> classes
  in li ([if disable then Props.emptyProp else reference <$ Props.onClick] <> classProps) [item]

passwordStrengthShow :: forall a. PasswordStrength -> Widget HTML a
passwordStrengthShow = text <<< show

loadingDiv :: forall a. Widget HTML a
loadingDiv = div [ (Props.className "loading") ] [
  div [Props.className "lds-spinner"] [
    div [] []
  , div [] []
  , div [] []
  , div [] []
  , div [] []
  , div [] []
  , div [] []
  , div [] []
  , div [] []
  , div [] []
  , div [] []
  , div [] []
  ]
]  

disableOverlay :: forall a. Widget HTML a
disableOverlay = div [(Props.className "disableOverlay")] []

confirmationWidget :: String -> Widget HTML Boolean
confirmationWidget message = div [(Props.className "disableOverlay")] [
  text message
, simpleButton "Yes" false true
, simpleButton "No" false false
]
