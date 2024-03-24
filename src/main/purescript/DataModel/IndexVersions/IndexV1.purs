module DataModel.IndexVersions.IndexV1 where

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, hexStringCodec)
import Data.Identifier (Identifier)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Set (Set)
import DataModel.CardVersions.Card (CardVersion, cardVersionCodec)
import DataModel.IndexVersions.Index (class IndexVersions, CardEntry(..), CardReference(..), Index(..))

newtype Index_V1 = Index_V1 {entries :: (List CardEntry_V1), identifier :: Identifier}
indexV1Codec :: CA.JsonCodec Index_V1
indexV1Codec = wrapIso Index_V1 $
  CAR.object "index_v1"
    { entries: CAC.list cardEntryV1Codec
    , identifier: hexStringCodec
    }

type CardEntry_V1 = 
  { title :: String
  , cardReference :: CardReference_V1
  , archived :: Boolean
  , tags :: Set String
  , lastUsed :: Number
  }
cardEntryV1Codec :: CA.JsonCodec CardEntry_V1
cardEntryV1Codec = 
  CAR.object "CardEntry"
    { title:         CA.string
    , cardReference: cardReferenceV1Codec
    , archived:      CA.boolean
    , tags:          CAC.set CA.string
    , lastUsed:      CA.number
    }

type CardReference_V1 = 
  { reference :: HexString
  , key :: HexString
  , version :: CardVersion
  , identifier :: Identifier
  }
cardReferenceV1Codec :: CA.JsonCodec CardReference_V1
cardReferenceV1Codec = 
  CA.object "CardReference"
    (CAR.record
      { reference:   hexStringCodec
      , key:         hexStringCodec
      , version:     cardVersionCodec
      , identifier:  hexStringCodec
      }
    )

derive instance newtypeIndex_V1 :: Newtype Index_V1 _

instance index_v1 :: IndexVersions Index_V1 where
  toIndex (Index_V1 index) = Index index {entries = ((\entry -> CardEntry entry { cardReference = CardReference entry.cardReference }) <$> index.entries)}
  fromIndex (Index index@{entries}) = Index_V1 index {entries = (\(CardEntry entry@{cardReference: CardReference reference}) -> entry {cardReference = reference}) <$> entries}
