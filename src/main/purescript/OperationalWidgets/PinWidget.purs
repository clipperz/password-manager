module OperationalWidgets.PinWidget where

import Bytes (asArrayBuffer)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.Core.FRP (demand, fireOnce, loopS, loopW)
import Concur.React.DOM (div, fieldset, text)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Apply ((<*>))
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, except)
import Control.Monad.State.Trans (evalStateT, StateT(..))
import Control.Semigroupoid ((<<<))
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Derive as KD
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (encrypt, decrypt, raw, unwrapKey, CryptoKey)
import Data.Array (head, tail)
import Data.Bifunctor (bimap)
import Data.BigInt (fromInt)
import Data.Either (note, Either(..))
import Data.EuclideanRing ((/))
import Data.Foldable (fold)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (fromArrayBuffer, hex, toArrayBuffer)
import Data.HeytingAlgebra ((&&), not)
import Data.Int (fromString)
import Data.List (List(..), (:))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), isJust)
import Data.Ord ((>), (<))
import Data.PrettyShow (prettyShow)
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Semiring ((*))
import Data.Show (show)
import Data.String.CodeUnits (length)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Data.Unit (unit, Unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.WidgetState (WidgetState(..))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Fortuna (randomBytes)
import Foreign (ForeignError(..))
import Functions.ArrayBuffer (bigIntToArrayBuffer, concatArrayBuffers)
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
    Default -> div [] [pinForm]
    Loading -> div [] [pinForm]
    Error e -> div [] [div [] [text e], pinForm]
  eitherRes <- case pinAction of
    Reset -> runExceptT (deleteCredentials storage)
    SetPin pin -> runExceptT (saveCredentials pin storage) 
  log $ show eitherRes
  case eitherRes of
    Left err -> setPinWidget (Error (show err))
    _        -> setPinWidget Default

  where 
    form :: Boolean -> Widget HTML PinWidgetAction
    form pinExists = fieldset [] [
      do
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
      log "Done"

    deleteCredentials :: Storage -> ExceptT AppError (Widget HTML) Unit
    deleteCredentials storage = liftEffect $ do
      removeItem (makeKey "user") storage
      removeItem (makeKey "passphrase") storage
      removeItem (makeKey "failures") storage


