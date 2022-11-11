module OperationalWidgets.PinWidget where

import Bytes (asArrayBuffer)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.Core.FRP (demand, fireOnce, loopW)
import Concur.React.DOM (h1, p, div, fieldset, text)
import Concur.React.Props as Props
import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, except)
import Control.Semigroupoid ((<<<))
import Data.Either (note, Either(..))
import Data.EuclideanRing ((/))
import Data.Function (($))
import Data.Functor ((<$>), void)
import Data.HexString (fromArrayBuffer, hex, toArrayBuffer)
import Data.HeytingAlgebra (not)
import Data.Int (fromString)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), isJust)
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Semiring ((*))
import Data.Show (show)
import Data.String.CodeUnits (length)
import Data.Unfoldable (fromMaybe)
import Data.Unit (Unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Fortuna (randomBytes)
import Functions.ArrayBuffer (concatArrayBuffers)
import Functions.EncodeDecode (encryptJson)
import Functions.JSState (getAppState)
import Functions.Pin (generateKeyFromPin, makeKey, isPinValid)
import Functions.State (getHashFunctionFromAppState)
import Views.SimpleWebComponents (simpleButton, simpleInputWidget)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem, removeItem, Storage)

data PinWidgetAction = Reset | SetPin Int

setPinWidget :: forall a. WidgetState -> Widget HTML a
setPinWidget ws = do
  storage <- liftEffect $ window >>= localStorage
  maybeSavedUser <- liftEffect $ getItem (makeKey "user") storage
  let pinForm = form (isJust maybeSavedUser)
  pinAction <- case ws of
    Default -> pinPage Nothing  pinForm
    Loading -> pinPage Nothing  pinForm
    Error e -> pinPage (Just e) pinForm
  eitherRes <- case pinAction of
    Reset -> (Right <$> (void $ pinPage Nothing (form true))) <|> (runExceptT (deleteCredentials storage))
    SetPin pin -> (Right <$> (void $ pinPage Nothing (form true))) <|> (runExceptT (saveCredentials pin storage)) 
  case eitherRes of
    Left err -> setPinWidget (Error (show err))
    _        -> setPinWidget Default

  where 
    form :: Boolean -> Widget HTML PinWidgetAction
    form pinExists = fieldset [] [
      text $ "PIN is " <> (if pinExists then "" else "not ") <> "set on this device"
    , do
        signalResult <- demand $ do
          pin <- loopW "" (\value -> simpleInputWidget "pinField" (text "PIN") pinExists (if pinExists then "*****" else "PIN") value "number")
          result :: Maybe PinWidgetAction <- fireOnce (submitWidget pin pinExists)
          pure result
        pure signalResult
    ]

    submitWidget pin pinExists =
      if pinExists then
        simpleButton "Reset" false Reset
      else case fromString pin of
        Just p -> simpleButton "Save" (not (isPinValid p)) (SetPin p)
        Nothing -> simpleButton "Save" true (SetPin 0)

    saveCredentials :: Int -> Storage -> ExceptT AppError (Widget HTML) Unit
    saveCredentials pin storage = do
      state@{ username, password } <- ExceptT $ liftEffect getAppState
      u <- except $ note (InvalidStateError (MissingValue "Missing username from state")) username
      p <- except $ note (InvalidStateError (MissingValue "Missing password from state")) password
      let hashf = getHashFunctionFromAppState state
      key <- ExceptT $ Right <$> (liftAff $ generateKeyFromPin hashf pin)

      -- 256 bits
      let paddingBytesLength = (256 - 16 * length (show (hex p))) / 8
      paddingBytes <- ExceptT $ (Right <<< asArrayBuffer) <$> (liftAff $ randomBytes paddingBytesLength)
      paddedPassphrase <- ExceptT $ (Right <<< fromArrayBuffer) <$> (liftEffect $ concatArrayBuffers ((toArrayBuffer $ hex p) : paddingBytes : Nil))
      let obj = { padding: paddingBytesLength, passphrase: paddedPassphrase }

      encryptedCredentials <- ExceptT $ Right <$> (liftAff $ encryptJson key obj)
      liftEffect $ setItem (makeKey "user") u storage -- save username  
      liftEffect $ setItem (makeKey "passphrase") (show $ fromArrayBuffer encryptedCredentials) storage -- save password

    deleteCredentials :: Storage -> ExceptT AppError (Widget HTML) Unit
    deleteCredentials storage = liftEffect $ do
      removeItem (makeKey "user") storage
      removeItem (makeKey "passphrase") storage
      removeItem (makeKey "failures") storage

pinPage :: forall a. Maybe String -> Widget HTML a -> Widget HTML a
pinPage error internalForm = div [Props._id "pinPage"] [
  h1 [] [text "Device PIN"]
, div [Props.className "description"] $ [
    p [] [text "You may create a 5-digit PIN to be used instead of your passphrase. Please note that the PIN is specific to the device you are now using."]
  , p [] [text "Warning: enabling a PIN on your device may represent a security risk! Make sure to keep the device with you at all times!"]  
  ] <> (fromMaybe $ (\t -> p [Props.className "error"] [text t]) <$> error)
, div [Props.className "content"] [internalForm]
]
