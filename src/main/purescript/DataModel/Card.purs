module DataModel.Card where

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Bifunctor (rmap)
import Data.Eq (class Eq, eq)
import Data.List.Types (List(..))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import DataModel.Password (PasswordGeneratorSettings)

currentCardVersion :: String
currentCardVersion = "V1"

newtype CardField =
  CardField
    { name   :: String
    , value  :: String
    , locked :: Boolean
    , settings :: Maybe PasswordGeneratorSettings
    }
  
instance eqCardField :: Eq CardField where
  eq (CardField r1) (CardField r2) = eq r1 r2

instance showCardField :: Show CardField where
  show (CardField { name, value, locked }) = "[" <> show locked <> "] " <> name <> ": " <> value

instance encodeJsonCardField :: EncodeJson CardField where
  encodeJson (CardField record) = encodeJson record

instance decodeJsonCardField :: DecodeJson CardField where
  decodeJson json = rmap (\record -> CardField record) (decodeJson json)

-- --------------------------------------------

newtype CardValues = 
  CardValues
    { title   :: String
    , tags    :: Array String
    , fields  :: Array CardField
    -- , attachments :: ?? --TODO
    , notes   :: String
    }

instance eqCardValues :: Eq CardValues where
  eq (CardValues r1) (CardValues r2) = eq r1 r2

instance showCardValues :: Show CardValues where
  show (CardValues record) = show record

instance encodeJsonCardValue :: EncodeJson CardValues where
  encodeJson (CardValues record) = encodeJson record

instance decodeJsonCardValue :: DecodeJson CardValues where
  decodeJson json = rmap (\record -> CardValues record) (decodeJson json)

-- --------------------------------------------

newtype Card = 
  Card 
    { content :: CardValues
    , secrets :: Array String
    , archived :: Boolean
    , timestamp :: Number
    }

instance eqCard :: Eq Card where
  eq (Card r1) (Card r2) = eq { content: r1.content, archived: r1.archived } { content: r2.content, archived: r2.archived }

instance showCard :: Show Card where
  show (Card record) = show record

instance encodeJsonCard :: EncodeJson Card where
  encodeJson (Card record) = encodeJson record

instance decodeJsonCard :: DecodeJson Card where
  decodeJson json = rmap (\record -> Card record) (decodeJson json)

-- --------------------------------------------

emptyCardField :: CardField
emptyCardField = CardField { name: "", value: "", locked: false, settings: Nothing }

emptyCard :: Card
emptyCard = Card { timestamp: 0.0
                    , archived: false
                    , secrets: []
                    , content: CardValues { title: ""
                                              , tags: []
                                              , fields: [ CardField { name: "username", value: "", locked: false, settings: Nothing }
                                                        , CardField { name: "password", value: "", locked: true,  settings: Nothing }
                                                        ]
                                              , notes: ""
                                              }
                    }

card0 :: Card
card0 = Card { timestamp: 0.0 , archived: false, secrets: [], content: cardValues0 }
card1 :: Card
card1 = Card { timestamp: 0.0 , archived: false, secrets: [], content: cardValues1 }

cardValues0 :: CardValues
cardValues0 = CardValues { title: "Mail account (SAMPLE)"
                      , tags: ["mail", "sample"]
                      , fields: [ (CardField {name: "username", value: "sample@mail.com", locked: false, settings: Nothing})
                                , (CardField {name: "password", value: "i3k^{flhadhse93na[{%oq[;6-", locked: true, settings: Nothing})]
                      , notes: "Mail account notes"}

cardValues1 :: CardValues
cardValues1 = CardValues { title: "Bank account (SAMPLE)"
                      , tags: ["finance", "sample"]
                      , fields: [ (CardField {name: "IBAN", value: "DE89370400440532015007", locked: false, settings: Nothing})
                                , (CardField {name: "password", value: "?)E%[9=GcgzaAftgP[LSEK7JJv", locked: true, settings: Nothing})
                                , (CardField {name: "User ID", value: "76238784", locked: false, settings: Nothing})
                                , (CardField {name: "Web", value: "https://www.db.com", locked: false, settings: Nothing})
                                , (CardField {name: "Bank", value: "Deutsche Bank", locked: false, settings: Nothing})
                                , (CardField {name: "Branch n.", value: "774942", locked: false, settings: Nothing})]
                      , notes: ""}

defaultCards :: List Card
defaultCards = Nil
