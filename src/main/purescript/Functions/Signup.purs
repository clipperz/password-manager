module Functions.Signup where

import Control.Bind (bind)
import Control.Applicative (pure)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, withExceptT)
import Control.Semigroupoid ((>>>))
import Crypto.Subtle.Constants.AES (aesCTR, l256)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Generate as KG
import Crypto.Subtle.Key.Types (encrypt, exportKey, decrypt, raw, unwrapKey, CryptoKey)
import Data.Array (fromFoldable)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, fromArrayBuffer)
import Data.List.Types (List(..), (:))
import Data.Tuple (Tuple(..), snd)
import Data.PrettyShow (prettyShow)
import DataModel.Card (Card, defaultCards)
import DataModel.Credentials (Credentials)
import DataModel.Index (Index(..), CardEntry(..), CardReference(..), createCardEntry, currentIndexVersion)
import DataModel.Password (standardPasswordGeneratorSettings)
import DataModel.SRP (SRPConf, SRPError(..))
import DataModel.User (UserCard(..), IndexReference(..), UserPreferencesReference(..), UserPreferences(..), UserInfoReferences(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.EncodeDecode (encryptJson)
import Functions.State (getSRPConf)
import Functions.SRP as SRP


type RegisterUserRequest = {
    user :: UserCard
  , preferencesReference :: HexString
  , preferencesContent :: HexString
  , indexCardReference :: HexString
  , indexCardContent   :: HexString
  , cards :: Array (Tuple HexString HexString)
}

prepareCards :: SRPConf -> List Card -> Aff (List (Tuple ArrayBuffer CardEntry))
prepareCards conf cards = extractAff $ convertToCardEntry <$> cards
  where convertToCardEntry :: Card -> Aff (Tuple ArrayBuffer CardEntry)
        convertToCardEntry card = do
          key <- KG.generateKey (KG.aes aesCTR l256) true [encrypt, decrypt, unwrapKey]
          createCardEntry card key conf.hash
        
        extractAff :: List (Aff (Tuple ArrayBuffer CardEntry)) -> Aff (List (Tuple ArrayBuffer CardEntry))
        extractAff Nil = pure Nil
        extractAff (Cons elem list) = do
          e <- elem
          l <- extractAff list
          pure $ Cons e l

prepareSignupParameters :: Credentials -> Aff (Either SRPError RegisterUserRequest)
prepareSignupParameters form = runExceptT $ do
  conf <- withExceptT (\e -> SRPError (prettyShow e)) (ExceptT $ liftEffect getSRPConf)
  cAb <- liftAff $ SRP.prepareC conf form.username form.password
  let c = fromArrayBuffer cAb
  pAb <- liftAff $ SRP.prepareP conf form.username form.password
  sAb <- liftAff $ SRP.randomArrayBuffer 32
  let salt = fromArrayBuffer sAb
  cards                  :: List (Tuple ArrayBuffer CardEntry) <- ExceptT $ Right <$> prepareCards conf defaultCards 
  v                      :: HexString   <- ExceptT $ SRP.prepareV conf sAb pAb
  masterKey              :: CryptoKey   <- ExceptT $ Right <$> KG.generateKey (KG.aes aesCTR l256) true [encrypt, decrypt, unwrapKey]
  masterKey2             :: CryptoKey   <- ExceptT $ Right <$> KG.generateKey (KG.aes aesCTR l256) true [encrypt, decrypt, unwrapKey]
  indexCardContent       :: ArrayBuffer <- ExceptT $ Right <$> encryptJson masterKey (Index (snd <$> cards))
  masterPassword         :: CryptoKey   <- ExceptT $ Right <$> KI.importKey raw pAb (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  indexCardContentHash   :: HexString   <- ExceptT $ (fromArrayBuffer >>> Right) <$> conf.hash (indexCardContent : Nil)
  masterKeyHex           :: HexString   <- ExceptT $ (fromArrayBuffer >>> Right) <$> exportKey raw masterKey
  masterKeyHex2          :: HexString   <- ExceptT $ (fromArrayBuffer >>> Right) <$> exportKey raw masterKey2
  let indexReference     =  IndexReference { reference: indexCardContentHash, masterKey: masterKeyHex, indexVersion: currentIndexVersion }

  let userPreferences = UserPreferences { passwordGeneratorSettings: standardPasswordGeneratorSettings
                                        , automaticLock: Right 10
                                        }
  preferencesContent     :: ArrayBuffer <- ExceptT $ Right <$> encryptJson masterKey2 userPreferences
  preferencesContentHash :: HexString   <- ExceptT $ (fromArrayBuffer >>> Right) <$> conf.hash (preferencesContent : Nil)
  let preferencesReference = UserPreferencesReference { reference: preferencesContentHash, key: masterKeyHex2 }

  let userInfoReference = UserInfoReferences { preferencesReference, indexReference }

  masterKeyContent       :: HexString   <- ExceptT $ (fromArrayBuffer >>> Right) <$> encryptJson masterPassword userInfoReference
  pure  { user:
            UserCard
              { c: c
              , v: v
              , s: salt
              , srpVersion : "6a"
              , masterKeyEncodingVersion : "1.0"
              , masterKeyContent : masterKeyContent
              }
        , preferencesReference : preferencesContentHash
        , preferencesContent : fromArrayBuffer preferencesContent
        , indexCardReference : indexCardContentHash
        , indexCardContent   : fromArrayBuffer indexCardContent
        , cards : fromFoldable ((\(Tuple encryptedCard (CardEntry { cardReference: (CardReference { reference }) })) -> (Tuple reference (fromArrayBuffer encryptedCard))) <$> cards)
        }
