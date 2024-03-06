module DataModel.IndexVersions.IndexV1 where

import Data.Functor ((<$>))
import Data.HexString (HexString)
import Data.Identifier (Identifier)
import Data.List (List)
import Data.Newtype (class Newtype)
import DataModel.Card (CardVersion)
import DataModel.Index (class IndexVersions, CardEntry(..), CardReference(..), Index(..))

newtype Index_V1 = Index_V1 {entries :: (List CardEntry_V1), identifier :: Identifier}

type CardEntry_V1 = 
  { title :: String
  , cardReference :: CardReference_V1
  , archived :: Boolean
  , tags :: Array String
  , lastUsed :: Number
  }

type CardReference_V1 = 
  { reference :: HexString
  , key :: HexString
  , version :: CardVersion
  , identifier :: Identifier
  }

derive instance newtypeIndex_V1 :: Newtype Index_V1 _

instance index_v1 :: IndexVersions Index_V1 where
  toIndex (Index_V1 index) = Index index {entries = ((\entry -> CardEntry entry { cardReference = CardReference entry.cardReference }) <$> index.entries)}

