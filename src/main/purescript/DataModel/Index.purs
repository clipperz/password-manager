module DataModel.Index where

import Control.Applicative (pure)
import Control.Bind (bind)
import Crypto.Subtle.Key.Types (exportKey, raw, CryptoKey)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (rmap)
import Data.Eq (class Eq, eq, (/=))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, fromArrayBuffer)
import Data.List (delete, filter)
import Data.List.Types (List(..), (:))
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (class Ord, compare)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.String.Common (toLower)
import Data.Tuple (Tuple(..))
import DataModel.Card (Card(..), CardValues(..), currentCardVersion)
import DataModel.SRP (HashFunction)
import Effect.Aff (Aff)
import Functions.EncodeDecode (encryptJson)

-- --------------------------------------------

currentIndexVersion :: String
currentIndexVersion = "V1"

-- --------------------------------------------

newtype CardReference =
  CardReference
    { reference :: HexString
    , key :: HexString
    , cardVersion :: String
    }

derive instance newtypeCardReference :: Newtype CardReference _

instance showCardReference :: Show CardReference where
  show (CardReference record) = show record

instance eqCardReference :: Eq CardReference where
  eq (CardReference { reference: r }) (CardReference { reference: r' }) = eq r r'

instance encodeJsonCardReference :: EncodeJson CardReference where
  encodeJson (CardReference record) = encodeJson record

instance decodeJsonCardReference :: DecodeJson CardReference where
  decodeJson json = rmap (\record -> CardReference record) (decodeJson json)

-- --------------------------------------------

newtype CardEntry =
  CardEntry
    { title :: String
    , cardReference :: CardReference
    , archived :: Boolean
    , tags :: Array String
    , lastUsed :: Number
    -- , attachment :: Boolean
    }

instance showCardEntry :: Show CardEntry where
  show (CardEntry
        { title
        , cardReference: _
        , archived: _
        , tags: _
        , lastUsed: _
        }) = "Entry for " <> title

instance ordCardEntry :: Ord CardEntry where
  compare (CardEntry { title: t }) (CardEntry {title: t'}) = compare (toLower t) (toLower t')

instance eqCardEntry :: Eq CardEntry where
  eq (CardEntry { cardReference: cr }) (CardEntry { cardReference: cr' }) = eq cr cr'

instance encodeJsonCardEntry :: EncodeJson CardEntry where
  encodeJson (CardEntry record) = encodeJson record

instance decodeJsonCardEntry :: DecodeJson CardEntry where
  decodeJson json = rmap (\record -> CardEntry record) (decodeJson json)

derive instance newtypeCardEntry :: Newtype CardEntry _

-- --------------------------------------------

emptyIndex :: Index
emptyIndex = Index Nil

newtype Index = 
  Index (List CardEntry)

instance encodeJsonIndex :: EncodeJson Index where
  encodeJson (Index list) = encodeJson list

instance decodeJsonIndex :: DecodeJson Index where
  decodeJson json = rmap (\list -> Index list) (decodeJson json)

addToIndex :: Index -> CardEntry -> Index
addToIndex (Index list) cardEntry = Index (cardEntry : list) 

removeFromIndex :: Index -> CardEntry -> Index
removeFromIndex (Index index) cardEntry = Index (delete cardEntry index)

updateInIndex :: Index -> CardEntry -> CardEntry -> Index
updateInIndex (Index list) oldEntry newEntry = Index (newEntry : filter (\(CardEntry { cardReference }) -> cardReference /= (unwrap oldEntry).cardReference) list)

-- --------------------------------------------

createCardEntry :: Card -> CryptoKey -> HashFunction -> Aff (Tuple ArrayBuffer CardEntry)
createCardEntry card@(Card { content: (CardValues content), archived, timestamp: _ }) key hashf = do
  encryptedCard <- encryptJson key card
  hash <- hashf (encryptedCard : Nil)
  exportedKey <- fromArrayBuffer <$> exportKey raw key
  let cardEntry = CardEntry { title: content.title
                               , cardReference: CardReference { reference: fromArrayBuffer hash, key: exportedKey, cardVersion: currentCardVersion }
                               , archived: archived
                               , tags: content.tags
                               , lastUsed: 0.0
                               }
  pure $ Tuple encryptedCard cardEntry
