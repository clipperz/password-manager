module DataModel.User where

import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.HexString (HexString)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show (class Show, show)
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

instance showIndexReference :: Show IndexReference where
  show (IndexReference record) = show record

-- --------------------------------------------------------------------------

newtype UserPreferences = 
  UserPreferences
    { passwordGeneratorSettings :: PasswordGeneratorSettings
    , automaticLock :: Either Int Int -- Left  -> automatic lock disabled while keeping the time
                                      -- Right -> automatic lock enabled
    }
derive instance newTypeUserPreferences :: Newtype UserPreferences _

instance showUserPreferences :: Show UserPreferences where
  show (UserPreferences record) = show record

derive instance eqUserPreferences :: Eq UserPreferences

defaultUserPreferences :: UserPreferences
defaultUserPreferences = UserPreferences {passwordGeneratorSettings: standardPasswordGeneratorSettings, automaticLock: Right 10}

newtype UserPreferencesReference =
  UserPreferencesReference
    { reference :: HexString
    , key :: HexString
    }

derive instance newTypeUserPreferencesReference :: Newtype UserPreferencesReference _

instance showUserPreferencesReference :: Show UserPreferencesReference where
  show (UserPreferencesReference record) = show record

newtype UserInfoReferences = --TODO: change references structure [fsolaroli - 06/01/2024]
  UserInfoReferences 
    { preferencesReference :: UserPreferencesReference
    , indexReference :: IndexReference
    }

derive instance newTypeUserInfoReferences :: Newtype UserInfoReferences _
  
instance showUserInfoReferences :: Show UserInfoReferences where
  show (UserInfoReferences record) = show record
