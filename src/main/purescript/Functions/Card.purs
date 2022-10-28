module Functions.Card where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), withExceptT, except)
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Generate as KG
import Crypto.Subtle.Key.Types (encrypt, decrypt, raw, unwrapKey, CryptoKey)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (fromArrayBuffer, toArrayBuffer, toString, Base(..))
import Data.Show (class Show, show)
import DataModel.AppState (AppError(..))
import DataModel.Card (Card)
import DataModel.CardVersions.CardV1 (Card_V1, cardFromV1)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (CardReference(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.EncodeDecode (decryptWithAesCTR)

import Effect.Exception as EX

getCardContent :: ArrayBuffer -> CardReference -> ExceptT AppError Aff Card
getCardContent bytes (CardReference ref) =
  case ref.cardVersion of 
    "V1" -> do
      cryptoKey     :: CryptoKey   <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer ref.key) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      decryptedData :: ArrayBuffer <- mapError (ExceptT $ liftAff $ decryptWithAesCTR bytes cryptoKey)
      parsedJson    :: Json        <- mapError (except $ jsonParser $ toString Dec $ fromArrayBuffer decryptedData)
      cardV1        :: Card_V1     <- mapError (except $ decodeJson parsedJson)
      pure $ cardFromV1 cardV1
    _    -> mapError $ (ExceptT $ Left <$> pure "version not found")

  where
    mapError :: forall a e. Show e => ExceptT e Aff a -> ExceptT AppError Aff a
    mapError = withExceptT (\err -> ProtocolError $ CryptoError $ show err)