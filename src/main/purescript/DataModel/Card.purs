module DataModel.Card where

import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Bifunctor (rmap)
import Data.Eq (class Eq, eq)
import Data.List.Types (List(..), (:))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)

newtype CardField =
  CardField
    { name   :: String
    , value  :: String
    , locked :: Boolean
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

data CardValues = 
  CardValues
    { title  :: String
    , tags   :: Array String
    , fields :: Array CardField
    -- , attachments :: ?? --TODO
    , notes  :: String
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

data Card = 
  Card_v1 
    { content :: CardValues
    , archived :: Boolean
    , timestamp :: Int
    }

instance eqCard :: Eq Card where
  eq (Card_v1 r1) (Card_v1 r2) = eq { content: r1.content, archived: r1.archived } { content: r2.content, archived: r2.archived }

instance showCard :: Show Card where
  show (Card_v1 record) = show record

instance encodeJsonCard :: EncodeJson Card where
  encodeJson (Card_v1 record) = encodeJson record

instance decodeJsonCard :: DecodeJson Card where
  decodeJson json = rmap (\record -> Card_v1 record) (decodeJson json)

-- --------------------------------------------

emptyCardField :: CardField
emptyCardField = CardField { name: "", value: "", locked: false }

emptyCard :: Card
emptyCard = Card_v1 { timestamp: 0
                    , archived: false
                    , content: CardValues { title: ""
                                              , tags: []
                                              , fields: [ CardField { name: "username", value: "", locked: false }
                                                        , CardField { name: "password", value: "", locked: true }
                                                        ]
                                              , notes: ""
                                              }
                    }
    
card0 :: CardValues
card0 = CardValues { title: "Mail account (SAMPLE)"
                      , tags: ["mail"]
                      , fields: [ (CardField {name: "username", value: "sample@mail.com", locked: false})
                                , (CardField {name: "password", value: "i3k^{flhadhse93na[{%oq[;6-", locked: true})]
                      , notes: "Mail account notes"}

card1 :: CardValues
card1 = CardValues { title: "Bank account (SAMPLE)"
                      , tags: ["finance", "sample"]
                      , fields: [ (CardField {name: "IBAN", value: "DE89370400440532015007", locked: false})
                                , (CardField {name: "password", value: "?)E%[9=GcgzaAftgP[LSEK7JJv", locked: true})
                                , (CardField {name: "User ID", value: "76238784", locked: false})
                                , (CardField {name: "Web", value: "https://www.db.com", locked: false})
                                , (CardField {name: "Bank", value: "Deutsche Bank", locked: false})
                                , (CardField {name: "Branch n.", value: "774942", locked: false})]
                      , notes: ""}

defaultCards :: List Card
defaultCards = Card_v1 { content: card0, timestamp: 1661377622, archived: false} :
               Card_v1 { content: card1, timestamp: 166137865 , archived: false} : Nil
