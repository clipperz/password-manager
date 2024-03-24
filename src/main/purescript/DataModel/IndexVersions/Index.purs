module DataModel.IndexVersions.Index where

import Control.Bind (pure, (>>=))
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Variant as CAV
import Data.Either (Either(..))
import Data.Eq (class Eq, eq)
import Data.Function (($))
import Data.HexString (HexString, hex)
import Data.Identifier (Identifier, computeIdentifier)
import Data.List (delete)
import Data.List.Types (List(..), (:))
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (class Ord, compare)
import Data.Profunctor (dimap)
import Data.Semigroup ((<>))
import Data.Set (Set)
import Data.Show (class Show, show)
import Data.String.Common (toLower)
import Data.Unit (unit)
import Data.Variant as V
import DataModel.CardVersions.Card (CardVersion)
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))

-- --------------------------------------------

data IndexVersion = IndexVersion_1
indexVersionCodec :: CA.JsonCodec IndexVersion
indexVersionCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { indexVesion_1: Left unit
    }
  where
    toVariant = case _ of
      IndexVersion_1 -> V.inj (Proxy :: _ "indexVesion_1") unit 
    fromVariant = V.match
      { indexVesion_1: \_ -> IndexVersion_1
      }

class IndexVersions a where
  toIndex   :: a     -> Index
  fromIndex :: Index -> a

-- --------------------------------------------

newtype CardReference =
  CardReference
    { reference :: HexString
    , key :: HexString
    , version :: CardVersion
    , identifier :: Identifier
    }

derive instance newtypeCardReference :: Newtype CardReference _

instance showCardReference :: Show CardReference where
  show (CardReference record) = show record

instance eqCardReference :: Eq CardReference where
  eq (CardReference { reference: r }) (CardReference { reference: r' }) = eq r r'

-- --------------------------------------------

newtype CardEntry =
  CardEntry
    { title :: String
    , cardReference :: CardReference
    , archived :: Boolean
    , tags :: Set String
    , lastUsed :: Number
    }

instance showCardEntry :: Show CardEntry where
  show (CardEntry
        { title
        , cardReference: _
        , archived: _
        , tags: _
        , lastUsed: _
        }) = "Entry for " <> title

instance ordCardEntry :: Ord CardEntry where
  compare (CardEntry { title: t }) (CardEntry {title: t'}) = compare (toLower t) (toLower t')

instance eqCardEntry :: Eq CardEntry where
  eq (CardEntry { cardReference: cr }) (CardEntry { cardReference: cr' }) = eq cr cr'

derive instance newtypeCardEntry :: Newtype CardEntry _

reference :: CardEntry -> HexString
reference (CardEntry entry) = (unwrap entry.cardReference).reference

-- --------------------------------------------

newtype Index = Index {entries :: (List CardEntry), identifier :: Identifier}

derive instance newtypeIndex :: Newtype Index _

emptyIndex :: Index
emptyIndex = Index {entries: Nil, identifier: hex("")}

prepareIndex :: List CardEntry -> Aff Index
prepareIndex entries = 
  computeIdentifier >>= 
  (\identifier -> pure $ Index {entries, identifier})

addToIndex :: CardEntry -> Index -> Aff Index
addToIndex cardEntry (Index {entries}) = 
  computeIdentifier >>= 
  (\identifier -> pure $ Index {entries: (cardEntry : entries), identifier})

removeFromIndex :: CardEntry -> Index -> Aff Index
removeFromIndex cardEntry (Index {entries}) =
  computeIdentifier >>= 
  (\identifier -> pure $ Index {entries: (delete cardEntry entries), identifier})

-- --------------------------------------------
