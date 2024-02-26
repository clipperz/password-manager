module DataModel.User where

import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.HexString (HexString)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import DataModel.Password (PasswordGeneratorSettings, standardPasswordGeneratorSettings)

-- ========================================================================

data MasterKeyEncodingVersion = V_1

-- --------------------------------------------------------------------------

data SRPVersion = V_6a

-- ========================================================================

type MasterKey = Tuple HexString MasterKeyEncodingVersion

newtype UserCard =
  UserCard
    { originMasterKey :: HexString
    , masterKey :: MasterKey
    }

derive instance newtypeUserCard :: Newtype UserCard _

-- --------------------------------------------------------------------------

newtype RequestUserCard =
  RequestUserCard
    { c :: HexString
    , v :: HexString
    , s :: HexString
    , srpVersion :: SRPVersion
    , originMasterKey :: Maybe HexString
    , masterKey :: MasterKey
    }

derive instance newtypeRequestUserCard :: Newtype RequestUserCard _

-- --------------------------------------------------------------------------

newtype IndexReference =
  IndexReference
    { reference :: HexString
    , masterKey :: HexString
    , indexVersion :: String
    }
  
derive instance newtypeIndexReference :: Newtype IndexReference _

-- --------------------------------------------------------------------------

newtype UserPreferences = 
  UserPreferences
    { passwordGeneratorSettings :: PasswordGeneratorSettings
    , automaticLock :: Either Int Int -- Left  -> automatic lock disabled while keeping the time
                                      -- Right -> automatic lock enabled
    }
derive instance newTypeUserPreferences :: Newtype UserPreferences _

derive instance eqUserPreferences :: Eq UserPreferences

defaultUserPreferences :: UserPreferences
defaultUserPreferences = UserPreferences {passwordGeneratorSettings: standardPasswordGeneratorSettings, automaticLock: Right 10}

-- ------------------------------------------------------------------------

newtype UserInfo = 
  UserInfo
    { indexReference  :: IndexReference
    , identifier      :: HexString
    , userPreferences :: UserPreferences
    }

derive instance newTypeUserInfo :: Newtype UserInfo _

type UserInfoReferences = Tuple HexString HexString
