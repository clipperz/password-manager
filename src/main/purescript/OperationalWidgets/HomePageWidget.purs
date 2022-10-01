module OperationalWidgets.HomePageWidget where

import Affjax.ResponseFormat as RF
import Concur.Core (Widget)
import Concur.Core.FRP (loopW, demandLoop)
import Concur.React (HTML)
import Concur.React.DOM (div', div, text)
import Control.Applicative (pure)
import Control.Bind (discard, bind)
import Control.Monad.Except.Trans (ExceptT(..), withExceptT, runExceptT, except)
import Control.Monad.State (StateT, get, runStateT, modify_, mapStateT)
import Control.Semigroupoid ((>>>))
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (encrypt, decrypt, raw, unwrapKey, CryptoKey)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Function (($), flip)
import Data.Functor ((<$>), (<$), void)
import Data.HexString (HexString, toArrayBuffer, fromArrayBuffer, splitHexInHalf)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import DataModel.AppState (AppState, AppError(..))
import DataModel.Card (Card)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (CardReference(..), Index, IndexReference)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception as EX
import Functions.EncodeDecode (decryptArrayBuffer)
import Functions.Communication.BackendCommunication (manageGenericRequest)
import Functions.Communication.Blobs (getDecryptedBlob, getDecryptedBlob')
import Functions.Communication.Cards (getIndex)
import Functions.JSState (getAppState)
import Functions.State (makeStateT, extractExceptT, computeInitialState)
import Views.SimpleWebComponents (simpleButton)
import OperationalWidgets.CardsManagerWidget (cardsManagerWidget)

data HomePageAction = LogoutAction

homePageWidget :: IndexReference -> Widget HTML Unit
homePageWidget indexReference = do
  eitherIndex <- liftAff $ runExceptT $ getIndex indexReference
  case eitherIndex of
    Right index -> void $ homePage index Nothing
    Left err -> div' [text ("Couldn't load cards list: " <> (show err))]
  pure unit

  where
    homePage :: Index -> Maybe CardReference -> Widget HTML HomePageAction
    homePage index cardReference = div [] [
      cardsManagerWidget index cardReference
    , simpleButton "Logout" false LogoutAction
    ]

