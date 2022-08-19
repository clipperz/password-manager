module DataModel.IndexCard where

import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Bifunctor (rmap)
import Data.Eq (class Eq, eq)
import Data.Function (($))
import Data.List.Types (List(..), (:))
import Data.Map.Internal (Map, fromFoldable)
import Data.Ord (class Ord, compare)
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))

-- --------------------------------------------

data CardReference =
  CardReference_v1
    { reference :: String
    , key :: String
    }

instance showCardReference :: Show CardReference where
  show (CardReference_v1 record) = show record

instance eqCardReference :: Eq CardReference where
  eq (CardReference_v1 {reference: r, key: _}) (CardReference_v1 {reference: r', key: _}) = eq r r'

instance encodeJsonCardReference :: EncodeJson CardReference where
  encodeJson (CardReference_v1 record) = encodeJson record

instance decodeJsonCardReference :: DecodeJson CardReference where
  decodeJson json = rmap (\record -> CardReference_v1 record) (decodeJson json)

-- --------------------------------------------

data CardIndex =
  CardIndex_v1
    { title :: String
    , cardReference :: CardReference
    , archived :: Boolean
    }

instance ordCardIndex :: Ord CardIndex where
  compare (CardIndex_v1 {title: t, cardReference: _, archived: _}) (CardIndex_v1 {title: t', cardReference: _, archived: _}) = compare t t'

instance eqCardIndex :: Eq CardIndex where
  eq (CardIndex_v1 {title: _, cardReference: cr, archived: _}) (CardIndex_v1 {title: _, cardReference: cr', archived: _}) = eq cr cr'

instance encodeJsonCardIndex :: EncodeJson CardIndex where
  encodeJson (CardIndex_v1 record) = encodeJson record

instance decodeJsonCardIndex :: DecodeJson CardIndex where
  decodeJson json = rmap (\record -> CardIndex_v1 record) (decodeJson json)

-- --------------------------------------------

type CardId = String

data IndexCard = 
  IndexCard_v1
    { cards :: Map CardId CardIndex
    }

instance encodeJsonIndexCard :: EncodeJson IndexCard where
  encodeJson (IndexCard_v1 record) = encodeJson record

instance decodeJsonIndexCard :: DecodeJson IndexCard where
  decodeJson json = rmap (\record -> IndexCard_v1 record) (decodeJson json)

emptyIndexCard :: IndexCard
emptyIndexCard = IndexCard_v1
  { cards: fromFoldable $ Tuple "CardId" (CardIndex_v1 {title: "", cardReference: CardReference_v1 {reference: "reference", key: "key"}, archived: false}) : Nil --TODO
  }
