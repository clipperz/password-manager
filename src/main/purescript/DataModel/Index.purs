module DataModel.Index where

import Control.Applicative (pure)
import Control.Bind (bind)
import Crypto.Subtle.Key.Types (exportKey, raw, CryptoKey)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (rmap)
import Data.Eq (class Eq, eq)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, fromArrayBuffer)
import Data.List.Types (List(..), (:))
import Data.Ord (class Ord, compare)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.String.Common (toLower)
import Data.Tuple (Tuple(..))
import DataModel.Card (Card(..), CardValues(..))
import DataModel.SRP (HashFunction)
import Effect.Aff (Aff)
import Functions.EncodeDecode (encryptJson)

-- --------------------------------------------

data CardReference =
  CardReference_v1
    { reference :: HexString
    , key :: HexString
    }

instance showCardReference :: Show CardReference where
  show (CardReference_v1 record) = show record

instance eqCardReference :: Eq CardReference where
  eq (CardReference_v1 { reference: r }) (CardReference_v1 { reference: r' }) = eq r r'

instance encodeJsonCardReference :: EncodeJson CardReference where
  encodeJson (CardReference_v1 record) = encodeJson record

instance decodeJsonCardReference :: DecodeJson CardReference where
  decodeJson json = rmap (\record -> CardReference_v1 record) (decodeJson json)

-- --------------------------------------------

data CardEntry =
  CardEntry_v1
    { title :: String
    , cardReference :: CardReference
    , archived :: Boolean
    , tags :: Array String
    -- , attachment :: Boolean
    }

instance showCardEntry :: Show CardEntry where
  show (CardEntry_v1
        { title
        , cardReference: _
        , archived: _
        , tags: _
        }) = "Entry for " <> title

instance ordCardEntry :: Ord CardEntry where
  compare (CardEntry_v1 { title: t }) (CardEntry_v1 {title: t'}) = compare (toLower t) (toLower t')

instance eqCardEntry :: Eq CardEntry where
  eq (CardEntry_v1 { cardReference: cr }) (CardEntry_v1 { cardReference: cr' }) = eq cr cr'

instance encodeJsonCardEntry :: EncodeJson CardEntry where
  encodeJson (CardEntry_v1 record) = encodeJson record

instance decodeJsonCardEntry :: DecodeJson CardEntry where
  decodeJson json = rmap (\record -> CardEntry_v1 record) (decodeJson json)

-- --------------------------------------------

data Index = 
  Index_v1 (List CardEntry)

instance encodeJsonIndex :: EncodeJson Index where
  encodeJson (Index_v1 list) = encodeJson list

instance decodeJsonIndex :: DecodeJson Index where
  decodeJson json = rmap (\list -> Index_v1 list) (decodeJson json)

-- --------------------------------------------

type IndexReference = HexString

-- --------------------------------------------

createCardEntry :: Card -> CryptoKey -> HashFunction -> Aff (Tuple ArrayBuffer CardEntry)
createCardEntry card@(Card { content: (CardValues content), archived, timestamp: _ }) key hashf = do
  encryptedCard <- encryptJson key card
  hash <- hashf (encryptedCard : Nil)
  exportedKey <- fromArrayBuffer <$> exportKey raw key
  let cardEntry = CardEntry_v1 { title: content.title
                               , cardReference: CardReference_v1 { reference: fromArrayBuffer hash, key: exportedKey }
                               , archived: archived
                               , tags: content.tags
                               }
  pure $ Tuple encryptedCard cardEntry
