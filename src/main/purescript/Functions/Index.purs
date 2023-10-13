module Functions.Index where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT(..), withExceptT, except)
import Control.Semigroupoid ((>>>))
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (encrypt, decrypt, raw, unwrapKey, CryptoKey)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Function (($))
import Data.HexString (Base(..), fromArrayBuffer, toArrayBuffer, toString)
import Data.Show (class Show, show)
import DataModel.AppState (AppError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (Index)
import DataModel.IndexVersions.IndexV1 (Index_V1, indexFromV1)
import DataModel.User (IndexReference(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.EncodeDecode (decryptWithAesCTR)

getIndexContent :: ArrayBuffer -> IndexReference -> ExceptT AppError Aff Index
getIndexContent bytes (IndexReference ref) =
  case ref.indexVersion of 
    "V1"    -> do
      cryptoKey     :: CryptoKey   <- liftAff $ KI.importKey raw (toArrayBuffer ref.masterKey) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      decryptedData :: ArrayBuffer <- mapError (ExceptT $ decryptWithAesCTR bytes cryptoKey)
      parsedJson    :: Json        <- mapError (except $ jsonParser $ toString Dec $ fromArrayBuffer decryptedData)
      indexV1       :: Index_V1    <- mapError (except $ decodeJson parsedJson)
      pure $ indexFromV1 indexV1
    version -> throwError $ InvalidVersioning version "index"

  where
    mapError :: forall a e. Show e => ExceptT e Aff a -> ExceptT AppError Aff a
    mapError = withExceptT (show >>> CryptoError >>> ProtocolError)
