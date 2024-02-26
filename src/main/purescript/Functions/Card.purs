module Functions.Card
  ( FieldType(..)
  , addTag
  , appendToTitle
  , archiveCard
  , getCardContent
  , getFieldType
  , restoreCard
  )
  where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), except, throwError, withExceptT)
import Control.Semigroupoid ((>>>))
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (snoc)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Codec.Argonaut (decode)
import Data.Either (Either(..))
import Data.Function (($))
import Data.HexString (fromArrayBuffer, toArrayBuffer, toString, Base(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import DataModel.AppError (AppError(..))
import DataModel.Card (Card(..), CardField(..), CardValues(..))
import DataModel.CardVersions.CardV1 (Card_V1, cardFromV1)
import DataModel.Codec as Codec
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (CardReference(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.EncodeDecode (decryptWithAesGCM, importCryptoKeyAesGCM)

getCardContent :: ArrayBuffer -> CardReference -> ExceptT AppError Aff Card
getCardContent bytes (CardReference ref) =
  case ref.version of 
    "V1"    -> do
      cryptoKey     :: CryptoKey   <- liftAff $ importCryptoKeyAesGCM (toArrayBuffer ref.key)
      decryptedData :: ArrayBuffer <- mapError (ExceptT $ decryptWithAesGCM bytes cryptoKey)
      parsedJson    :: Json        <- mapError (except  $ jsonParser $ toString Dec (fromArrayBuffer decryptedData))
      cardV1        :: Card_V1     <- mapError (except  $ decode Codec.cardV1Codec parsedJson)
      pure $ cardFromV1 cardV1
    version -> throwError $ InvalidVersioning version "card"

  where
    mapError :: forall a e. Show e => ExceptT e Aff a -> ExceptT AppError Aff a
    mapError = withExceptT (show >>> CryptoError >>> ProtocolError)

data FieldType = Email | Url | Passphrase | None

appendToTitle :: String -> Card -> Card
appendToTitle titleAppend (Card card@{content: CardValues cardValues@{title}}) = Card (card {content = CardValues cardValues {title = title <> titleAppend} })

addTag :: String -> Card -> Card
addTag tag (Card card@{content: CardValues cardValues@{tags}}) = Card (card {content = CardValues cardValues {tags = snoc tags tag}})

archiveCard :: Card -> Card
archiveCard (Card card) = Card card {archived = true}

restoreCard :: Card -> Card
restoreCard (Card card) = Card card {archived = false}
 
testRegex :: Either String Regex -> String -> Boolean
testRegex eitherRegex value =
  case eitherRegex of
    Left  _     -> false
    Right regex -> test regex value

emailRegex :: Either String Regex
emailRegex = regex "^([a-zA-Z0-9._%+-]+)@([a-zA-Z0-9.-]+)\\.([a-zA-Z]{2,})$" noFlags

isValidEmail :: String -> Boolean
isValidEmail email = testRegex emailRegex email

urlRegex :: Either String Regex
urlRegex = regex "^https?://[a-zA-Z0-9-]+(\\.[a-zA-Z0-9-]+)*(:[0-9]+)?(/.*)?$" noFlags

isValidUrl :: String -> Boolean
isValidUrl url = testRegex urlRegex url

getFieldType :: CardField -> FieldType
getFieldType (CardField { value, locked })
  | locked              = Passphrase
  | isValidEmail value  = Email
  | isValidUrl value    = Url
  | true                = None