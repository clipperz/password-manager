module DataModel.CardVersions.Card where

import Control.Alt ((<#>), (<$>))
import Control.Alternative (pure)
import Control.Bind (bind)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Variant as CAV
import Data.Either (Either(..))
import Data.Eq (class Eq, eq)
import Data.Function (($))
import Data.List.Types (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profunctor (dimap)
import Data.Semigroup ((<>))
import Data.Set (Set, empty, fromFoldable)
import Data.Show (class Show, show)
import Data.Unit (unit)
import Data.Variant as V
import DataModel.Password (PasswordGeneratorSettings)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Type.Proxy (Proxy(..))

data CardVersion = CardVersion_1
cardVersionCodec :: CA.JsonCodec CardVersion
cardVersionCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { cardVersion_1: Left unit
    }
  where
    toVariant = case _ of
      CardVersion_1 -> V.inj (Proxy :: _ "cardVersion_1") unit
    fromVariant = V.match
      { cardVersion_1: \_ -> CardVersion_1
      }

instance showCardVersion :: Show CardVersion where
 show CardVersion_1 = "CardVersion_1"

-- --------------------------------------------

newtype CardField =
  CardField
    { name   :: String
    , value  :: String
    , locked :: Boolean
    , settings :: Maybe PasswordGeneratorSettings
    }

derive instance newtypeCardField :: Newtype CardField _

instance eqCardField :: Eq CardField where
  eq (CardField r1) (CardField r2) = eq r1 r2

instance showCardField :: Show CardField where
  show (CardField { name, value, locked }) = "[" <> show locked <> "] " <> name <> ": " <> value

instance arbitratryCardField :: Arbitrary CardField where
  arbitrary = CardField <$> arbitrary

-- --------------------------------------------

newtype CardValues = 
  CardValues
    { title   :: String
    , tags    :: Set String
    , fields  :: Array CardField
    , notes   :: String
    }

derive instance newtypeCardValues :: Newtype CardValues _

instance eqCardValues :: Eq CardValues where
  eq (CardValues r1) (CardValues r2) = eq r1 r2

instance showCardValues :: Show CardValues where
  show (CardValues record) = show record

instance arbitraryCardValues :: Arbitrary CardValues where
  arbitrary = CardValues <$> do
    title  <- arbitrary
    tags   <- arbitrary <#> (\(array :: Array String) -> fromFoldable array)
    fields <- arbitrary
    notes  <- arbitrary
    pure $ {title, tags, fields, notes}

-- --------------------------------------------

newtype Card = 
  Card 
    { content :: CardValues
    , secrets :: Array String
    , archived :: Boolean
    , timestamp :: Number
    }

derive instance newtypeCard :: Newtype Card _

instance eqCard :: Eq Card where
  eq (Card r1) (Card r2) = eq { content: r1.content, archived: r1.archived } { content: r2.content, archived: r2.archived }

instance showCard :: Show Card where
  show (Card record) = show record

class CardVersions a where
  toCard   :: a    -> Card
  fromCard :: Card -> a

instance arbitraryCard :: Arbitrary Card where
  arbitrary = Card <$> arbitrary


-- --------------------------------------------

emptyCardField :: CardField
emptyCardField = CardField { name: "", value: "", locked: false, settings: Nothing }

emptyCard :: Card
emptyCard = Card { timestamp: 0.0
                    , archived: false
                    , secrets: []
                    , content: CardValues { title: ""
                                              , tags: empty
                                              , fields: [ CardField { name: "username", value: "", locked: false, settings: Nothing }
                                                        , CardField { name: "password", value: "", locked: true,  settings: Nothing }
                                                        ]
                                              , notes: ""
                                              }
                    }

defaultCards :: List Card
defaultCards = Nil

data FieldType = Email | Url | Passphrase | None
