module DataModel.IndexVersions.IndexV1 where

import Data.Functor ((<$>))
import Data.List (List)
import Data.HexString (HexString)
import DataModel.Index (Index(..), CardEntry(..), CardReference(..))

type Index_V1 = List CardEntry_V1

type CardEntry_V1 = 
  { title :: String
  , cardReference :: CardReference_V1
  , archived :: Boolean
  , tags :: Array String
  }

type CardReference_V1 =
  { reference :: HexString
  , key :: HexString
  , cardVersion :: String
  }

indexFromV1 :: Index_V1 -> Index
indexFromV1 index = Index ((\entry -> CardEntry entry { cardReference = CardReference entry.cardReference }) <$> index)
