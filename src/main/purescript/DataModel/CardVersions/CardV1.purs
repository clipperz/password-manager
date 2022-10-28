module DataModel.CardVersions.CardV1 where

type Card_V1 = 
  { content :: CardValues_V1
  , archived :: Boolean
  , timestamp :: Int
  }

type CardValues_V1 = 
  { title  :: String
  , tags   :: Array String
  , fields :: Array CardField_V1
  , notes  :: String
  }

type CardField_V1 =
  { name   :: String
  , value  :: String
  , locked :: Boolean
  }