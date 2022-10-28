module DataModel.IndexVersions.IndexV1 where

import Data.List (List)
import Data.HexString (HexString)

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
  , version :: String
  }