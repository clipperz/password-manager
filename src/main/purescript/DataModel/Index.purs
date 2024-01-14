module DataModel.Index where

import Data.Eq (class Eq, eq, (/=))
import Data.HexString (HexString)
import Data.List (delete, filter)
import Data.List.Types (List(..), (:))
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (class Ord, compare)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.String.Common (toLower)

-- --------------------------------------------

currentIndexVersion :: String
currentIndexVersion = "V1"

-- --------------------------------------------

newtype CardReference =
  CardReference
    { reference :: HexString
    , key :: HexString
    , cardVersion :: String
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
emptyIndex = Index Nil

newtype Index = 
  Index (List CardEntry)

derive instance newtypeIndex :: Newtype Index _

addToIndex :: CardEntry -> Index -> Index
addToIndex cardEntry (Index list) = Index (cardEntry : list) 

removeFromIndex :: CardEntry -> Index -> Index
removeFromIndex cardEntry (Index index) = Index (delete cardEntry index)

updateInIndex :: CardEntry -> CardEntry -> Index -> Index
updateInIndex oldEntry newEntry (Index list) = Index (newEntry : filter (\(CardEntry { cardReference }) -> cardReference /= (unwrap oldEntry).cardReference) list)

-- --------------------------------------------
