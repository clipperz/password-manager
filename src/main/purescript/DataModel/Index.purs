module DataModel.Index where

import Control.Alt ((<#>))
import Control.Category ((>>>))
import Data.Eq (class Eq, eq, (/=))
import Data.EuclideanRing ((/))
import Data.HexString (HexString, fromArrayBuffer, hex)
import Data.List (delete, filter)
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

addToIndex :: CardEntry -> Index -> Index
addToIndex cardEntry (Index index@{entries}) = Index index {entries = (cardEntry : entries)}

removeFromIndex :: CardEntry -> Index -> Index
removeFromIndex cardEntry (Index index@{entries}) = Index index {entries = (delete cardEntry entries)}

updateInIndex :: CardEntry -> CardEntry -> Index -> Index
updateInIndex oldEntry newEntry (Index index@{entries}) = Index index {entries = (newEntry : filter (\(CardEntry { cardReference }) -> cardReference /= (unwrap oldEntry).cardReference) entries)}

-- --------------------------------------------
