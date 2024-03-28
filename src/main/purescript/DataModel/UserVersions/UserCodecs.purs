module DataModel.UserVersions.UserCodecs where

import Control.Bind ((>>=))
import Control.Category ((<<<), (>>>))
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonDecodeError(..), codec', decode, encode)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format, unformat)
import Data.Function (($))
import Data.HexString (HexString, hexStringCodec)
import Data.Identifier (Identifier)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import DataModel.CardVersions.CardV1 (passwordGeneratorSettingsV1Codec)
import DataModel.IndexVersions.Index (IndexVersion, indexVersionCodec)
import DataModel.Password (PasswordGeneratorSettings)
import DataModel.UserVersions.User (class UserInfoVersions, IndexReference(..), UserInfo(..), UserPreferences(..), toUserInfo)

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

instance userInfov1 :: UserInfoVersions UserInfo_V1 where
 toUserInfo = toUserInfo_V2 >>> toUserInfo

toUserInfo_V2 :: UserInfo_V1 -> UserInfo_V2
toUserInfo_V2 (UserInfo_V1 userInfo@{indexReference: IndexReference_V1 indexReference, userPreferences: UserPreferences_V1 userPreferences}) = UserInfo_V2 {indexReference: IndexReference_V1 indexReference, userPreferences: UserPreferences_V1 userPreferences, dateOfLastDonation: Nothing, identifier: userInfo.identifier}

-- ----------------------------------------------------------------

iso8601DateFormatter :: Formatter
iso8601DateFormatter = YearFull : Placeholder "-" : MonthTwoDigits : Placeholder "-" : DayOfMonthTwoDigits : Nil

newtype UserInfo_V2 = 
  UserInfo_V2
    { indexReference     :: IndexReference_V1
    , identifier         :: Identifier
    , userPreferences    :: UserPreferences_V1
    , dateOfLastDonation :: Maybe DateTime
    }
userInfoV2Codec :: CA.JsonCodec UserInfo_V2
userInfoV2Codec = wrapIso UserInfo_V2 (
  CAR.object "UserInfoV2"
    { indexReference:  indexReferenceV1Codec
    , identifier:      hexStringCodec
    , userPreferences: userPreferencesV1Codec
    , dateOfLastDonation: CAC.maybe $ codec' (\json -> decode CA.string json >>= (lmap TypeMismatch <<< unformat iso8601DateFormatter)) (format iso8601DateFormatter >>> encode CA.string)
    }
)

derive instance newTypeUserInfo_V2 :: Newtype UserInfo_V2 _

instance userInfov2 :: UserInfoVersions UserInfo_V2 where
 toUserInfo   (UserInfo_V2 userInfo@{indexReference: IndexReference_V1 indexReference, userPreferences: UserPreferences_V1 userPreferences}) = UserInfo (userInfo {indexReference = IndexReference indexReference, userPreferences = UserPreferences userPreferences})

fromUserInfo :: UserInfo -> UserInfo_V2
fromUserInfo (UserInfo userInfo@{indexReference: IndexReference indexReference, userPreferences: UserPreferences userPreferences}) = UserInfo_V2 (userInfo {indexReference = IndexReference_V1 indexReference, userPreferences = UserPreferences_V1 userPreferences})

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

derive instance newtypeuserPreferences_V1 :: Newtype UserPreferences_V1 _
