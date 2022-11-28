module DataModel.Card where

import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Bifunctor (rmap)
import Data.Eq (class Eq, eq)
import Data.List.Types (List(..), (:))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)

currentCardVersion :: String
currentCardVersion = "V1"

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

newtype CardValues = 
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

newtype Card = 
  Card 
    { content :: CardValues
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
emptyCardField = CardField { name: "", value: "", locked: false }

emptyCard :: Card
emptyCard = Card { timestamp: 0.0
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
                      , tags: ["mail", "sample"]
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
defaultCards = Nil
-- defaultCards = Card { content: card0, timestamp: 1661377622.0, archived: false} :
--                Card { content: card1, timestamp: 166137865.0 , archived: false} : Nil
