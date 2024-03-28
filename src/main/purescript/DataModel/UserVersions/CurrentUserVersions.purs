module DataModel.UserVersions.CurrentUserVersions where

import Data.Codec.Argonaut (JsonCodec)
import DataModel.UserVersions.User (MasterKeyEncodingVersion(..))
import DataModel.UserVersions.UserCodecs (UserInfo_V2, userInfoV2Codec)

currentMasterKeyEncodingVersion :: MasterKeyEncodingVersion
currentMasterKeyEncodingVersion = MasterKeyEncodingVersion_2

currentUserInfoCodecVersion :: JsonCodec UserInfo_V2
currentUserInfoCodecVersion = userInfoV2Codec
