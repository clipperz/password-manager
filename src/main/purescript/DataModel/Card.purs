module DataModel.Card where

import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Bifunctor (rmap)
import Data.List.Types (List(..), (:))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)

data CardField =
  CardField_v1
    { name   :: String
    , value  :: String
    , locked :: Boolean
    }

instance showCardField :: Show CardField where
  show (CardField_v1 { name, value, locked }) = "[" <> show locked <> "] " <> name <> ": " <> value

instance encodeJsonCardField :: EncodeJson CardField where
  encodeJson (CardField_v1 record) = encodeJson record

instance decodeJsonCardField :: DecodeJson CardField where
  decodeJson json = rmap (\record -> CardField_v1 record) (decodeJson json)

-- --------------------------------------------

data CardValues = 
  CardValues_v1
    { title  :: String
    , tags   :: Array String
    , fields :: Array CardField
    -- , attachments :: ?? --TODO
    , notes  :: String
    }

instance showCardValues :: Show CardValues where
  show (CardValues_v1 record) = show record

instance encodeJsonCardValue :: EncodeJson CardValues where
  encodeJson (CardValues_v1 record) = encodeJson record

instance decodeJsonCardValue :: DecodeJson CardValues where
  decodeJson json = rmap (\record -> CardValues_v1 record) (decodeJson json)

-- --------------------------------------------

data Card = 
  Card_v1 
    { content :: CardValues
    , timestamp :: Int
    }

instance showCard :: Show Card where
  show (Card_v1 record) = show record

instance encodeJsonCard :: EncodeJson Card where
  encodeJson (Card_v1 record) = encodeJson record

instance decodeJsonCard :: DecodeJson Card where
  decodeJson json = rmap (\record -> Card_v1 record) (decodeJson json)

-- --------------------------------------------

emptyCard = Card_v1 { timestamp: 0
                      , content: CardValues_v1 { title: ""
                                                , tags: []
                                                , fields: []
                                                , notes: ""
                                                }
                      }
    
card0 :: CardValues
card0 = CardValues_v1 { title: "Mail account (SAMPLE)"
                      , tags: ["mail"]
                      , fields: [ (CardField_v1 {name: "username", value: "sample@mail.com", locked: false})
                                , (CardField_v1 {name: "password", value: "i3k^{flhadhse93na[{%oq[;6-", locked: true})]
                      , notes: "Mail account notes"}

card1 :: CardValues
card1 = CardValues_v1 { title: "Bank account (SAMPLE)"
                      , tags: ["finance", "sample"]
                      , fields: [ (CardField_v1 {name: "IBAN", value: "DE89370400440532015007", locked: false})
                                , (CardField_v1 {name: "password", value: "?)E%[9=GcgzaAftgP[LSEK7JJv", locked: true})
                                , (CardField_v1 {name: "User ID", value: "76238784", locked: false})
                                , (CardField_v1 {name: "Web", value: "https://www.db.com", locked: false})
                                , (CardField_v1 {name: "Bank", value: "Deutsche Bank", locked: false})
                                , (CardField_v1 {name: "Branch n.", value: "774942", locked: false})]
                      , notes: ""}

defaultCards :: List Card
defaultCards = Card_v1 { content: card0, timestamp: 1661377622} :
               Card_v1 { content: card1, timestamp: 166137865 } : Nil
