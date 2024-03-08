module DataModel.UserVersions.UserV1 where

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either)
import Data.Function (($))
import Data.HexString (HexString, hexStringCodec)
import Data.Identifier (Identifier)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import DataModel.CardVersions.CardV1 (passwordGeneratorSettingsV1Codec)
import DataModel.IndexVersions.Index (IndexVersion, indexVersionCodec)
import DataModel.Password (PasswordGeneratorSettings)
import DataModel.UserVersions.User (class UserInfoVersions, IndexReference(..), UserInfo(..), UserPreferences(..))

newtype UserInfo_V1 = 
  UserInfo_V1
    { indexReference  :: IndexReference_V1
    , identifier      :: Identifier
    , userPreferences :: UserPreferences_V1
    }
userInfoV1Codec :: CA.JsonCodec UserInfo_V1
userInfoV1Codec = wrapIso UserInfo_V1 (
  CAR.object "UserInfoV1"
    { indexReference:  indexReferenceV1Codec
    , identifier:      hexStringCodec
    , userPreferences: userPreferencesV1Codec
    }
)

derive instance newTypeUserInfo_V1 :: Newtype UserInfo_V1 _

derive instance newtypeuserPreferences_V1 :: Newtype UserPreferences_V1 _

instance userInfov1 :: UserInfoVersions UserInfo_V1 where
 toUserInfo (UserInfo_V1 userInfo@{indexReference: IndexReference_V1 indexReference, userPreferences: UserPreferences_V1 userPreferences}) = UserInfo (userInfo {indexReference = IndexReference indexReference, userPreferences = UserPreferences userPreferences})
 fromUserInfo (UserInfo userInfo@{indexReference: IndexReference indexReference, userPreferences: UserPreferences userPreferences}) = UserInfo_V1 (userInfo {indexReference = IndexReference_V1 indexReference, userPreferences = UserPreferences_V1 userPreferences})

-- ----------------------------------------------------------------

newtype IndexReference_V1 =
  IndexReference_V1
    { reference :: HexString
    , key       :: HexString
    , version   :: IndexVersion
    }
indexReferenceV1Codec :: CA.JsonCodec IndexReference_V1
indexReferenceV1Codec = wrapIso IndexReference_V1 $
  CAR.object "IndexReferenceV1"
    { reference: hexStringCodec
    , key:       hexStringCodec
    , version:   indexVersionCodec
    }
  
derive instance newtypeIndexReference_V1 :: Newtype IndexReference_V1 _

-- ----------------------------------------------------------------

newtype UserPreferences_V1 = 
  UserPreferences_V1
    { passwordGeneratorSettings :: PasswordGeneratorSettings
    , automaticLock :: Either Int Int
    }
userPreferencesV1Codec :: CA.JsonCodec UserPreferences_V1
userPreferencesV1Codec = wrapIso UserPreferences_V1 (
  CAR.object "UserPreferences_V1"
    { passwordGeneratorSettings: passwordGeneratorSettingsV1Codec
    , automaticLock:             CAC.either CA.int CA.int
    }
)
