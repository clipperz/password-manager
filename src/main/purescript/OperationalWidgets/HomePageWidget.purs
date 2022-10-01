module OperationalWidgets.HomePageWidget where

import Affjax.ResponseFormat as RF
import Concur.Core (Widget)
import Concur.Core.FRP (loopW, demandLoop)
import Concur.React (HTML)
import Concur.React.DOM (div', div, text)
import Concur.React.Props as P
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (discard, bind)
import Control.Monad.Except.Trans (ExceptT(..), withExceptT, runExceptT, except)
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
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception as EX
import Functions.EncodeDecode (decryptArrayBuffer)
import Functions.Communication.BackendCommunication (manageGenericRequest)
import Functions.Communication.Blobs (getDecryptedBlob, getDecryptedBlob)
import Functions.Communication.Cards (getIndex)
import Functions.JSState (getAppState)
import Functions.State (computeInitialState)
import Views.SimpleWebComponents (simpleButton, loadingDiv)
import OperationalWidgets.CardsManagerWidget (cardsManagerWidget)

data HomePageAction = Loaded (Either AppError Index) | LogoutAction

homePageWidget :: IndexReference -> Widget HTML Unit
homePageWidget indexReference = go Loading indexReference
  where 
    go widgetState reference = do
      res <- case widgetState of
        Default -> div [] []
        Loading -> loadingDiv <|> (Loaded <$> (liftAff $ runExceptT $ getIndex reference))
        Error err -> div [] [text err, simpleButton "Go back to login" false LogoutAction]
      case res of
        Loaded (Right index) -> void $ homePage index Nothing         
        Loaded (Left err) -> go (Error (show err)) reference
        LogoutAction -> pure unit
    
    homePage :: Index -> Maybe CardReference -> Widget HTML HomePageAction
    homePage index cardReference = div [] [
      cardsManagerWidget index cardReference
    , simpleButton "Logout" false LogoutAction
    ]

