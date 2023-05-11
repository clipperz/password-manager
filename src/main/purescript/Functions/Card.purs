module Functions.Card
  ( FieldType(..)
  , getCardContent
  , getFieldType
  )
  where

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
import Data.HexString (fromArrayBuffer, toArrayBuffer, toString, Base(..))
import Data.Show (class Show, show)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import DataModel.AppState (AppError(..))
import DataModel.Card (Card, CardField(..))
import DataModel.CardVersions.CardV1 (Card_V1, cardFromV1)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (CardReference(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.EncodeDecode (decryptWithAesCTR)

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

data FieldType = Email | Url | Passphrase | None

testRegex :: Either String Regex -> String -> Boolean
testRegex eitherRegex value =
  case eitherRegex of
    Left _ -> false
    Right regex -> test regex value

emailRegex :: Either String Regex
emailRegex = regex "^([a-zA-Z0-9._%+-]+)@([a-zA-Z0-9.-]+)\\.([a-zA-Z]{2,})$" noFlags

isValidEmail :: String -> Boolean
isValidEmail email = testRegex emailRegex email

urlRegex :: Either String Regex
-- urlRegex = regex "^(?:(ht|f)tp(s?)\\:\\/\\/)?[0-9a-zA-Z]([-\\.\\w]*[0-9a-zA-Z])*(:(0-9)*)*(\\/?)([a-zA-Z0-9\\-\\.\\?\\,'\\/\\\\\\+&amp;%\\$#_]*)?$" noFlags
urlRegex = regex "^https?://[a-zA-Z0-9-]+(\\.[a-zA-Z0-9-]+)*(:[0-9]+)?(/.*)?$" noFlags

isValidUrl :: String -> Boolean
isValidUrl url = testRegex urlRegex url

getFieldType :: CardField -> FieldType
getFieldType (CardField { value, locked })
  | locked              = Passphrase
  | isValidEmail value  = Email
  | isValidUrl value    = Url
  | true                = None