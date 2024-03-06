module DataModel.CardVersions.CardV1 where

import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import DataModel.Card (class CardVersions, Card(..), CardField(..), CardValues(..))

newtype Card_V1 = Card_V1 
  { content :: CardValues_V1
  , secrets :: Array String
  , archived :: Boolean
  , timestamp :: Number
  }

derive instance newtypeCard_V1 :: Newtype Card_V1 _

instance card_v1 :: CardVersions Card_V1 where
  toCard (Card_V1 card) = Card card { content = CardValues card.content { fields = CardField <$> card.content.fields } }

type CardValues_V1 = 
  { title   :: String
  , tags    :: Array String
  , fields  :: Array CardField_V1
  , notes   :: String
  }

type CardField_V1 =
  { name   :: String
  , value  :: String
  , locked :: Boolean
  , settings :: Maybe PasswordGeneratorSettings_V1
  }

type PasswordGeneratorSettings_V1 = {
    length              :: Int,
    characters          :: String
}
