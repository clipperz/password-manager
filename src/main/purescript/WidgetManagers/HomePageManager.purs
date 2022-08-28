module WidgetManagers.HomePageManager where

import Concur.Core (Widget)
import Concur.Core.FRP (loopW, dyn)
import Concur.React (HTML)
import Control.Applicative (pure)
import Control.Bind (discard, bind)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, withExceptT)
import Control.Semigroupoid ((>>>))
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (encrypt, decrypt, raw, unwrapKey, CryptoKey)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, toArrayBuffer, fromArrayBuffer, splitHexInHalf)
import Data.List.Types (List(..))
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception as EX
import EncodeDecode (decryptArrayBuffer)
import DataModel.Index (CardReference(..), Index(..))
import DataModel.Card (Card(..), card0)
import Widgets.HomePage (HomePageAction(..), CardsViewAction(..), homePage)
import WidgetManagers.LoginManager (LoginManagerResult)
import RestBackendCommunication (getDecryptedBlob)

getCard :: CardReference -> Aff Card
getCard (CardReference_v1 { reference, key }) = do
  cryptoKey <- KI.importKey raw (toArrayBuffer key) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  result <- getDecryptedBlob reference cryptoKey
  case result of
    Left err -> do
      _ <- log $ show err
      _ <- log $ show "False Card"
      pure $ Card_v1 { content: card0, timestamp: 1661377622} -- TODO
    Right card -> pure card

getIndex :: HexString -> HexString -> Aff Index
getIndex p encryptedRef = do
  result <- runExceptT $ do
    masterPassword :: CryptoKey <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer p) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
    { before: masterKey, after: indexReference } <- ExceptT $ (\either -> (fromArrayBuffer >>> splitHexInHalf) <$> either) <$> (decryptArrayBuffer masterPassword (toArrayBuffer encryptedRef))
    cryptoKey      :: CryptoKey <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer masterKey) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
    withExceptT (\err -> EX.error $ show err) (ExceptT $ getDecryptedBlob indexReference cryptoKey)
  case result of
    -- Left  _   -> getIndex p encryptedRef
    Left  err -> do
      _ <- log $ show err
      _ <- log $ show "False Index"
      pure $ Index_v1 Nil -- TODO
    Right res -> pure res

homePageManager :: LoginManagerResult -> Widget HTML Unit
homePageManager { p, indexReference } = do
  index <- liftAff $ getIndex p indexReference
  dyn $ loopW (CardsViewAction (ShowCard Nothing)) (\cva ->
    case cva of
      CardsViewAction (ShowCard Nothing) -> homePage index Nothing
      CardsViewAction (ShowCard (Just ref)) -> do
        card <- liftAff $ getCard ref
        homePage index (Just card)
      CardsViewAction (ActOnCard _ a) -> do
        liftEffect $ log $ show a
        homePage index Nothing
  )