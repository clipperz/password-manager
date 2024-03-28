module DataModel.CardVersions.CardV1 where

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (wrapIso)
import Data.Set (Set)
import DataModel.CardVersions.Card (class CardVersions, Card(..), CardField(..), CardValues(..))

newtype Card_V1 = Card_V1 
  { content :: CardValues_V1
  , secrets :: Array String
  , archived :: Boolean
  , timestamp :: Number
  }
cardV1Codec :: CA.JsonCodec Card_V1
cardV1Codec = wrapIso Card_V1 $
  CAR.object "cardV1"
    { content   : cardValuesV1Codec
    , secrets   : CA.array CA.string
    , archived  : CA.boolean
    , timestamp : CA.number
    }

derive instance newtypeCard_V1 :: Newtype Card_V1 _

instance card_v1 :: CardVersions Card_V1 where
  toCard (Card_V1 card) = Card card { content = CardValues card.content { fields = CardField <$> card.content.fields } }
  fromCard (Card card@{content: CardValues content@{fields}}) = Card_V1 card {content = content {fields = unwrap <$> fields}}

-- ---------------------------------------------------------

type CardValues_V1 = 
  { title   :: String
  , tags    :: Set String
  , fields  :: Array CardField_V1
  , notes   :: String
  }
cardValuesV1Codec :: CA.JsonCodec CardValues_V1
cardValuesV1Codec = 
  CAR.object "cardValuesV1"
    { title   : CA.string
    , tags    : CAC.set CA.string
    , fields  : CA.array cardFieldV1Codec
    , notes   : CA.string
    }

-- ---------------------------------------------------------

type CardField_V1 =
  { name   :: String
  , value  :: String
  , locked :: Boolean
  , settings :: Maybe PasswordGeneratorSettings_V1
  }
cardFieldV1Codec :: CA.JsonCodec CardField_V1
cardFieldV1Codec =
  CAR.object "cardFieldV1"
    { name     : CA.string
    , value    : CA.string
    , locked   : CA.boolean
    , settings : CAR.optional passwordGeneratorSettingsV1Codec
    }

-- ---------------------------------------------------------

type PasswordGeneratorSettings_V1 = {
    length              :: Int,
    characters          :: String
}
passwordGeneratorSettingsV1Codec :: CA.JsonCodec PasswordGeneratorSettings_V1
passwordGeneratorSettingsV1Codec = 
  CAR.object "PasswordGeneratorSettings_V1"
    { length     : CA.int
    , characters : CA.string
    }
