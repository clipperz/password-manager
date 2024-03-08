module DataModel.UserVersions.CurrentUserVersions where

import Data.Codec.Argonaut (JsonCodec)
import DataModel.UserVersions.User (MasterKeyEncodingVersion(..))
import DataModel.UserVersions.UserV1 (UserInfo_V1, userInfoV1Codec)

currentMasterKeyEncodingVersion :: MasterKeyEncodingVersion
currentMasterKeyEncodingVersion = MasterKeyEncodingVersion_1

currentUserInfoCodecVersion :: JsonCodec UserInfo_V1
currentUserInfoCodecVersion = userInfoV1Codec
