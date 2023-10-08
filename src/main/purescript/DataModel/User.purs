module DataModel.User where

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Bifunctor (rmap)
import Data.Either (Either)
import Data.Eq (class Eq)
import Data.HexString (HexString)
import Data.Show (class Show, show)
import DataModel.Password (PasswordGeneratorSettings)

newtype UserCard =
  UserCard
    { c :: HexString
    , v :: HexString
    , s :: HexString
    , srpVersion :: String
    , masterKeyEncodingVersion :: String
    , masterKeyContent :: HexString
    }

instance showUserCard :: Show UserCard where
  show (UserCard record) = show record

instance encodeJsonUserCard :: EncodeJson UserCard where
  encodeJson (UserCard record) = encodeJson record

instance decodeJsonUserCard :: DecodeJson UserCard where
  decodeJson json = rmap (\record -> UserCard record) (decodeJson json)

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
  
instance showUserInfoReferences :: Show UserInfoReferences where
  show (UserInfoReferences record) = show record

instance encodeJsonUserInfoReferences :: EncodeJson UserInfoReferences where
  encodeJson (UserInfoReferences record) = encodeJson record

instance decodeJsonUserInfoReferences :: DecodeJson UserInfoReferences where
  decodeJson json = rmap (\record -> UserInfoReferences record) (decodeJson json)
