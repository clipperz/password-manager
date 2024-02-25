module Functions.Signup where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.Array (fromFoldable)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, fromArrayBuffer)
import Data.List.Types (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import DataModel.Card (Card, defaultCards)
import DataModel.Codec as Codec
import DataModel.Communication.Signup (RegisterUserRequest)
import DataModel.Credentials (Credentials)
import DataModel.Index (Index(..), CardEntry(..), CardReference(..), currentIndexVersion)
import DataModel.SRP (SRPConf, SRPError)
import DataModel.User (IndexReference(..), MasterKeyEncodingVersion(..), RequestUserCard(..), SRPVersion(..), UserInfoReferences(..), UserPreferencesReference(..), defaultUserPreferences)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.EncodeDecode (encryptJson, exportCryptoKeyToHex, generateCryptoKeyAesGCM, importCryptoKeyAesGCM)
import Functions.Index (createCardEntry)
import Functions.SRP as SRP

prepareCards :: SRPConf -> List Card -> Aff (List (Tuple ArrayBuffer CardEntry))
prepareCards srpConf cards = extractAff $ convertToCardEntry <$> cards
  where convertToCardEntry :: Card -> Aff (Tuple ArrayBuffer CardEntry)
        convertToCardEntry card = do
          key <- generateCryptoKeyAesGCM
          createCardEntry card key srpConf.hash
        
        extractAff :: List (Aff (Tuple ArrayBuffer CardEntry)) -> Aff (List (Tuple ArrayBuffer CardEntry))
        extractAff  Nil             = pure Nil
        extractAff (Cons elem list) = do
          e <- elem
          l <- extractAff list
          pure $ Cons e l

prepareSignupParameters :: SRPConf -> Credentials -> Aff (Either SRPError RegisterUserRequest)
prepareSignupParameters srpConf form = runExceptT $ do
  cAb <- liftAff $ SRP.prepareC srpConf form.username form.password
  let c = fromArrayBuffer cAb
  pAb <- liftAff $ SRP.prepareP srpConf form.username form.password
  sAb <- liftAff $ SRP.randomArrayBuffer 32
  let salt = fromArrayBuffer sAb
  cards                  :: List (Tuple ArrayBuffer CardEntry) <- liftAff $ prepareCards srpConf defaultCards 
  v                      :: HexString   <- ExceptT $ SRP.prepareV srpConf sAb pAb
  masterKey              :: CryptoKey   <- liftAff $ generateCryptoKeyAesGCM
  masterKey2             :: CryptoKey   <- liftAff $ generateCryptoKeyAesGCM
  indexCardContent       :: ArrayBuffer <- liftAff $ encryptJson Codec.indexCodec masterKey (Index (snd <$> cards))
  masterPassword         :: CryptoKey   <- liftAff $ importCryptoKeyAesGCM pAb
  indexCardContentHash   :: HexString   <- liftAff $ fromArrayBuffer <$> srpConf.hash (indexCardContent : Nil)
  masterKeyHex           :: HexString   <- liftAff $ exportCryptoKeyToHex masterKey
  masterKeyHex2          :: HexString   <- liftAff $ exportCryptoKeyToHex masterKey2
  let indexReference     =  IndexReference { reference: indexCardContentHash, masterKey: masterKeyHex, indexVersion: currentIndexVersion }
  preferencesContent     :: ArrayBuffer <- liftAff $ encryptJson Codec.userPreferencesCodec masterKey2 defaultUserPreferences
  preferencesContentHash :: HexString   <- liftAff $ fromArrayBuffer <$> srpConf.hash (preferencesContent : Nil)
  let preferencesReference = UserPreferencesReference { reference: preferencesContentHash, key: masterKeyHex2 }

  let userInfoReference = UserInfoReferences { preferencesReference, indexReference }

  masterKeyContent       :: HexString   <- liftAff $ fromArrayBuffer <$> encryptJson Codec.userInfoReferencesCodec masterPassword userInfoReference
  pure  { user:
            RequestUserCard
              { c: c
              , v: v
              , s: salt
              , srpVersion: V_6a
              , masterKey: Tuple masterKeyContent V_1
              , originMasterKey: Nothing
              }
        , p : fromArrayBuffer pAb
        , preferencesReference: preferencesContentHash
        , preferencesContent:   fromArrayBuffer preferencesContent
        , indexCardReference:   indexCardContentHash
        , indexCardContent:     fromArrayBuffer indexCardContent
        , cards:                fromFoldable ((\(Tuple encryptedCard (CardEntry { cardReference: (CardReference { reference }) })) -> (Tuple reference (fromArrayBuffer encryptedCard))) <$> cards)
        }
