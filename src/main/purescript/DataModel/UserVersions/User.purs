module DataModel.UserVersions.User where

import Control.Alternative (pure)
import Control.Bind (bind)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Variant as CAV
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Function (($))
import Data.HexString (HexString, hexStringCodec)
import Data.Identifier (Identifier, computeIdentifier)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profunctor (dimap, wrapIso)
import Data.Tuple (Tuple)
import Data.Unit (unit)
import Data.Variant as V
import DataModel.IndexVersions.Index (IndexVersion)
import DataModel.Password (PasswordGeneratorSettings, standardPasswordGeneratorSettings)
import DataModel.SRPVersions.SRP (SRPVersion, srpVersionCodec)
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))


data MasterKeyEncodingVersion = MasterKeyEncodingVersion_1 | MasterKeyEncodingVersion_2
masterKeyEncodingVersionCodec :: CA.JsonCodec MasterKeyEncodingVersion
masterKeyEncodingVersionCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { masterKeyEncodingVersion_1: Left unit
    , masterKeyEncodingVersion_2: Left unit
    }
  where
    toVariant = case _ of
      MasterKeyEncodingVersion_1 -> V.inj (Proxy :: _ "masterKeyEncodingVersion_1") unit 
      MasterKeyEncodingVersion_2 -> V.inj (Proxy :: _ "masterKeyEncodingVersion_2") unit 
    fromVariant = V.match
      { masterKeyEncodingVersion_1: \_ -> MasterKeyEncodingVersion_1
      , masterKeyEncodingVersion_2: \_ -> MasterKeyEncodingVersion_2
      }

type MasterKey = Tuple HexString MasterKeyEncodingVersion
masterKeyCodec :: CA.JsonCodec MasterKey
masterKeyCodec = CAC.tuple hexStringCodec masterKeyEncodingVersionCodec

-- --------------------------------------------------------------------------

newtype UserCard =
  UserCard
    { originMasterKey :: HexString
    , masterKey       :: MasterKey
    }
userCardCodec :: CA.JsonCodec UserCard
userCardCodec = wrapIso UserCard $
  CAR.object "userCard"
    { originMasterKey : hexStringCodec
    , masterKey       : masterKeyCodec
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
requestUserCardCodec :: CA.JsonCodec RequestUserCard
requestUserCardCodec = wrapIso RequestUserCard $
  CAR.object "requestUserCard"
    { c: hexStringCodec
    , v: hexStringCodec
    , s: hexStringCodec
    , srpVersion: srpVersionCodec
    , originMasterKey: CAR.optional hexStringCodec
    , masterKey: masterKeyCodec
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
    { indexReference     :: IndexReference
    , identifier         :: Identifier
    , userPreferences    :: UserPreferences
    , dateOfLastDonation :: Maybe DateTime
    }

derive instance newTypeUserInfo :: Newtype UserInfo _

class UserInfoVersions a where
  toUserInfo :: a -> UserInfo

type UserInfoReferences = {reference :: HexString, key :: HexString}

prepareUserInfo :: IndexReference -> UserPreferences -> Aff UserInfo
prepareUserInfo indexReference userPreferences = do
  identifier <- computeIdentifier
  pure $ UserInfo {indexReference, userPreferences, identifier, dateOfLastDonation: Nothing}
