module DataModel.CardVersions.CardV1 where

import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import DataModel.Card (Card(..), CardValues(..), CardField(..))
import DataModel.Password (PasswordGeneratorSettings)

type Card_V1 = 
  { content :: CardValues_V1
  , archived :: Boolean
  , timestamp :: Number
  }

type CardValues_V1 = 
  { title  :: String
  , tags   :: Array String
  , fields :: Array CardField_V1
  , notes  :: String
  }

type CardField_V1 =
  { name   :: String
  , value  :: String
  , locked :: Boolean
  , settings :: Maybe PasswordGeneratorSettings
  }

cardFromV1 :: Card_V1 -> Card
cardFromV1 card = Card card { content = CardValues card.content { fields = CardField <$> card.content.fields } }
