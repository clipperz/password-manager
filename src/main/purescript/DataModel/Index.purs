module DataModel.Index where

import Control.Alt ((<#>))
import Control.Bind (bind, pure)
import Control.Category ((>>>))
import Data.Eq (class Eq, eq)
import Data.EuclideanRing ((/))
import Data.Function (($))
import Data.HexString (HexString, fromArrayBuffer, hex)
import Data.List (delete)
import Data.List.Types (List(..), (:))
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (class Ord, compare)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.String.Common (toLower)
import Effect.Aff (Aff)
import Functions.SRP (randomArrayBuffer)

-- --------------------------------------------

currentIndexVersion :: String
currentIndexVersion = "V1"

-- --------------------------------------------

newtype CardReference =
  CardReference
    { reference :: HexString
    , key :: HexString
    , version :: String
    , identifier :: HexString
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
    , tags :: Array String
    , lastUsed :: Number
    -- , attachment :: Boolean
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

emptyIndex :: Index
emptyIndex = Index {entries: Nil, identifier: hex("")}

prepareIndex :: List CardEntry -> Aff Index
prepareIndex entries = randomArrayBuffer (256/8) <#> (fromArrayBuffer >>> (\identifier -> Index {entries, identifier}))

newtype Index = Index {entries :: (List CardEntry), identifier :: HexString}

derive instance newtypeIndex :: Newtype Index _

addToIndex :: CardEntry -> Index -> Aff Index
addToIndex cardEntry (Index {entries}) = do
  newIndexCardIdentifier <- randomArrayBuffer (256/8) <#> fromArrayBuffer
  pure $ Index {entries: (cardEntry : entries), identifier: newIndexCardIdentifier}

removeFromIndex :: CardEntry -> Index -> Aff Index
removeFromIndex cardEntry (Index {entries}) = do
  newIndexCardIdentifier <- randomArrayBuffer (256/8) <#> fromArrayBuffer
  pure $ Index {entries: (delete cardEntry entries), identifier: newIndexCardIdentifier}

-- --------------------------------------------
