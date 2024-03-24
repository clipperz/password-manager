module Functions.Card
  ( addTag
  , appendToTitle
  , archiveCard
  , createCardEntry
  , decryptCard
  , encryptCard
  , getFieldType
  , restoreCard
  )
  where

import Control.Alt ((<#>), (<$>))
import Control.Bind (bind, pure, (=<<))
import Control.Monad.Except.Trans (ExceptT(..), withExceptT)
import Control.Semigroupoid ((>>>))
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Function ((#), ($))
import Data.HexString (HexString, fromArrayBuffer, toArrayBuffer)
import Data.Identifier (Identifier, computeIdentifier)
import Data.List (List(..), (:))
import Data.Semigroup ((<>))
import Data.Set (insert)
import Data.Show (class Show, show)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Tuple (Tuple(..))
import DataModel.AppError (AppError(..))
import DataModel.CardVersions.Card (class CardVersions, Card(..), CardField(..), CardValues(..), CardVersion(..), FieldType(..), fromCard, toCard)
import DataModel.CardVersions.CurrentCardVersions (currentCardCodecVersion, currentCardVersion)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.IndexVersions.Index (CardEntry(..), CardReference(..))
import DataModel.SRPVersions.SRP (HashFunction)
import Effect.Aff (Aff)
import Functions.EncodeDecode (decryptJson, encryptJson, exportCryptoKeyToHex, generateCryptoKeyAesGCM, importCryptoKeyAesGCM)

decryptCard :: ArrayBuffer -> CardReference -> ExceptT AppError Aff Card
decryptCard encryptedCard (CardReference {version, key}) =
  case version of 
    CardVersion_1    -> decryptCardJson currentCardCodecVersion

  where
    decryptCardJson :: forall a. CardVersions a => CA.JsonCodec a -> ExceptT AppError Aff Card
    decryptCardJson codec = toCard <$> ExceptT ((\cryptoKey -> decryptJson codec cryptoKey encryptedCard) =<< (importCryptoKeyAesGCM (toArrayBuffer key))) # mapError

    mapError :: forall a e. Show e => ExceptT e Aff a -> ExceptT AppError Aff a
    mapError = withExceptT (show >>> CryptoError >>> ProtocolError)

encryptCard :: Card -> HashFunction -> Aff (Tuple ArrayBuffer CardReference)
encryptCard card hashFunc = do
  identifier        :: Identifier  <- computeIdentifier
  cardKey           :: CryptoKey   <- generateCryptoKeyAesGCM
  encryptedCard     :: ArrayBuffer <- encryptJson currentCardCodecVersion cardKey (fromCard card)
  cardKeyHex        :: HexString   <- exportCryptoKeyToHex cardKey
  encryptedCardHash :: HexString   <- hashFunc (encryptedCard : Nil) <#> fromArrayBuffer
  pure $ Tuple encryptedCard (CardReference { reference: encryptedCardHash, identifier, key: cardKeyHex, version: currentCardVersion } )

-- ------------------------------------------------------------------------

createCardEntry :: HashFunction -> Card -> Aff (Tuple ArrayBuffer CardEntry)
createCardEntry hashFunc card@(Card { content: (CardValues content), archived, timestamp: _ }) = do
  Tuple encryptedCard cardReference <- encryptCard card hashFunc
  let cardEntry  = CardEntry { title: content.title
                             , tags: content.tags
                             , archived: archived
                             , lastUsed: 0.0 --TODO handle lastUsed [fsolaroli - 28/02/2024]
                             , cardReference
                             }
  pure $ Tuple encryptedCard cardEntry

appendToTitle :: String -> Card -> Card
appendToTitle titleAppend (Card card@{content: CardValues cardValues@{title}}) = Card (card {content = CardValues cardValues {title = title <> titleAppend} })

addTag :: String -> Card -> Card
addTag tag (Card card@{content: CardValues cardValues@{tags}}) = Card (card {content = CardValues cardValues {tags = insert tag tags}})

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