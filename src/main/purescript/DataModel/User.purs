module DataModel.User where

import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.HexString (HexString)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show (class Show, show)
import Data.Tuple (Tuple)
import DataModel.Password (PasswordGeneratorSettings)

-- ========================================================================

data MasterKeyEncodingVersion = V_1

instance encodeMasterKeyEncodingVersion :: EncodeJson MasterKeyEncodingVersion where
  encodeJson V_1 = encodeJson "1.0"

instance decodeMasterKeyEncodingVersion :: DecodeJson MasterKeyEncodingVersion where
  decodeJson json = case decodeJson json of
    Right "1.0" -> Right V_1
    Right _     -> Left (UnexpectedValue json)
    Left  err   -> Left err

-- --------------------------------------------------------------------------

data SRPVersion = V_6a

instance encodeSRPVersion :: EncodeJson SRPVersion where
  encodeJson V_6a = encodeJson "6a"

instance decodeSRPVersion :: DecodeJson SRPVersion where
  decodeJson json = case decodeJson json of
    Right "6a"  -> Right V_6a
    Right _     -> Left (UnexpectedValue json)
    Left  err   -> Left err

-- ========================================================================

type MasterKey = Tuple HexString MasterKeyEncodingVersion

newtype UserCard =
  UserCard
    { originMasterKey :: HexString
    , masterKey :: MasterKey
    }

instance encodeJsonUserCard :: EncodeJson UserCard where
  encodeJson (UserCard record) = encodeJson record

instance decodeJsonUserCard :: DecodeJson UserCard where
  decodeJson json = rmap (\record -> UserCard record) (decodeJson json)

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

instance encodeJsonRegisterUserCard :: EncodeJson RequestUserCard where
  encodeJson (RequestUserCard record) = encodeJson record

instance decodeJsonRegisterUserCard :: DecodeJson RequestUserCard where
  decodeJson json = rmap (\record -> RequestUserCard record) (decodeJson json)

-- --------------------------------------------------------------------------

newtype IndexReference =
  IndexReference
    { reference :: HexString
    , masterKey :: HexString
    , indexVersion :: String
    }
  
instance showIndexReference :: Show IndexReference where
  show (IndexReference record) = show record

instance encodeJsonIndexReference :: EncodeJson IndexReference where
  encodeJson (IndexReference record) = encodeJson record

instance decodeJsonIndexReference :: DecodeJson IndexReference where
  decodeJson json = rmap (\record -> IndexReference record) (decodeJson json)

newtype UserPreferences = 
  UserPreferences
    { passwordGeneratorSettings :: PasswordGeneratorSettings
    , automaticLock :: Either Int Int -- Left  -> automatic lock disabled while keeping the time
                                      -- Right -> automatic lock enabled
    }
derive instance newTypeUserPreferences :: Newtype UserPreferences _

instance showUserPreferences :: Show UserPreferences where
  show (UserPreferences record) = show record

instance encodeJsonUserPreferences :: EncodeJson UserPreferences where
  encodeJson (UserPreferences record) = encodeJson record

instance decodeJsonUserPreferences :: DecodeJson UserPreferences where
  decodeJson json = rmap (\record -> UserPreferences record) (decodeJson json)

derive instance eqUserPreferences :: Eq UserPreferences

newtype UserPreferencesReference =
  UserPreferencesReference
    { reference :: HexString
    , key :: HexString
    }

derive instance newTypeUserPreferencesReference :: Newtype UserPreferencesReference _

instance showUserPreferencesReference :: Show UserPreferencesReference where
  show (UserPreferencesReference record) = show record

instance encodeJsonUserPreferencesReference :: EncodeJson UserPreferencesReference where
  encodeJson (UserPreferencesReference record) = encodeJson record

instance decodeJsonUserPreferencesReference :: DecodeJson UserPreferencesReference where
  decodeJson json = rmap (\record -> UserPreferencesReference record) (decodeJson json)

newtype UserInfoReferences =
  UserInfoReferences 
    { preferencesReference :: UserPreferencesReference
    , indexReference :: IndexReference
    }

derive instance newTypeUserInfoReferences :: Newtype UserInfoReferences _
  
instance showUserInfoReferences :: Show UserInfoReferences where
  show (UserInfoReferences record) = show record

instance encodeJsonUserInfoReferences :: EncodeJson UserInfoReferences where
  encodeJson (UserInfoReferences record) = encodeJson record

instance decodeJsonUserInfoReferences :: DecodeJson UserInfoReferences where
  decodeJson json = rmap (\record -> UserInfoReferences record) (decodeJson json)
