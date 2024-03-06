module DataModel.User where

import Control.Alternative (pure)
import Control.Bind ((>>=))
import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Function (($))
import Data.HexString (HexString)
import Data.Identifier (Identifier, computeIdentifier)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import DataModel.Index (IndexVersion)
import DataModel.Password (PasswordGeneratorSettings, standardPasswordGeneratorSettings)
import Effect.Aff (Aff)

-- ========================================================================


-- --------------------------------------------------------------------------

data SRPVersion = V_6a

-- ========================================================================

data MasterKeyEncodingVersion = MasterKeyEncodingVersion_1

currentMasterKeyEncodingVersion :: MasterKeyEncodingVersion
currentMasterKeyEncodingVersion = MasterKeyEncodingVersion_1

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
    , key       :: HexString
    , version   :: IndexVersion
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
    , identifier      :: Identifier
    , userPreferences :: UserPreferences
    }

derive instance newTypeUserInfo :: Newtype UserInfo _

class UserInfoVersions a where
  toUserInfo :: a -> UserInfo

type UserInfoReferences = {reference :: HexString, key :: HexString}

prepareUserInfo :: IndexReference -> UserPreferences -> Aff UserInfo
prepareUserInfo indexReference userPreferences = 
  computeIdentifier >>=
  (\identifier -> pure $ UserInfo {indexReference, userPreferences, identifier})

-- ----------

newtype UserInfo_V1 = 
  UserInfo_V1
    { indexReference  :: IndexReference_V1
    , identifier      :: Identifier
    , userPreferences :: UserPreferences_V1
    }

derive instance newTypeUserInfo_V1 :: Newtype UserInfo_V1 _

newtype IndexReference_V1 =
  IndexReference_V1
    { reference :: HexString
    , key       :: HexString
    , version   :: IndexVersion
    }
  
derive instance newtypeIndexReference_V1 :: Newtype IndexReference_V1 _

newtype UserPreferences_V1 = 
  UserPreferences_V1
    { passwordGeneratorSettings :: PasswordGeneratorSettings
    , automaticLock :: Either Int Int
    }

derive instance newtypeuserPReferences_V1 :: Newtype UserPreferences_V1 _

instance userInfov1 :: UserInfoVersions UserInfo_V1 where
 toUserInfo (UserInfo_V1 userInfo@{indexReference: IndexReference_V1 indexReference, userPreferences: UserPreferences_V1 userPreferences}) = UserInfo (userInfo {indexReference = IndexReference indexReference, userPreferences = UserPreferences userPreferences})