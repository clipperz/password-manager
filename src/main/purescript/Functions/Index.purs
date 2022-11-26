module Functions.Index where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), withExceptT, except)
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (encrypt, decrypt, raw, unwrapKey, CryptoKey)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, fromArrayBuffer, toArrayBuffer, toString, Base(..))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (Index)
import DataModel.IndexVersions.IndexV1 (Index_V1, indexFromV1)
import DataModel.User (IndexReference(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.EncodeDecode (decryptJson, decryptWithAesCTR)
import Functions.JSState (getAppState)

-- fromCardToCardEntry :: Index -> Card -> Aff CardEntry
-- fromCardToCardEntry (Index entries) card = do

getIndexContent :: ArrayBuffer -> IndexReference -> ExceptT AppError Aff Index
getIndexContent bytes (IndexReference ref) =
  case ref.indexVersion of 
    "V1" -> do
      cryptoKey     :: CryptoKey   <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer ref.masterKey) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      decryptedData :: ArrayBuffer <- mapError (ExceptT $ liftAff $ decryptWithAesCTR bytes cryptoKey)
      parsedJson    :: Json        <- mapError (except $ jsonParser $ toString Dec $ fromArrayBuffer decryptedData)
      indexV1       :: Index_V1    <- mapError (except $ decodeJson parsedJson)
      pure $ indexFromV1 indexV1
    _    -> mapError $ (ExceptT $ Left <$> pure "version not found")

  where
    mapError :: forall a e. Show e => ExceptT e Aff a -> ExceptT AppError Aff a
    mapError = withExceptT (\err -> ProtocolError $ CryptoError $ show err)
