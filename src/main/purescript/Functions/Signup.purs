module Functions.Signup where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Crypto.Subtle.Constants.AES (aesCTR, l256)
import Crypto.Subtle.Key.Generate as KG
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (encrypt, exportKey, decrypt, raw, unwrapKey, CryptoKey)
import Data.Array (fromFoldable)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, fromArrayBuffer)
import Data.List.Types (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import DataModel.Card (Card, defaultCards)
import DataModel.Credentials (Credentials)
import DataModel.Index (Index(..), CardEntry(..), CardReference(..), createCardEntry, currentIndexVersion)
import DataModel.Password (standardPasswordGeneratorSettings)
import DataModel.SRP (SRPConf, SRPError)
import DataModel.User (IndexReference(..), MasterKeyEncodingVersion(..), RequestUserCard(..), SRPVersion(..), UserInfoReferences(..), UserPreferences(..), UserPreferencesReference(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.EncodeDecode (encryptJson)
import Functions.SRP as SRP


type RegisterUserRequest = {
  user                 :: RequestUserCard
, p                    :: HexString
, preferencesReference :: HexString
, preferencesContent   :: HexString
, indexCardReference   :: HexString
, indexCardContent     :: HexString
, cards                :: Array (Tuple HexString HexString)
}

prepareCards :: SRPConf -> List Card -> Aff (List (Tuple ArrayBuffer CardEntry))
prepareCards srpConf cards = extractAff $ convertToCardEntry <$> cards
  where convertToCardEntry :: Card -> Aff (Tuple ArrayBuffer CardEntry)
        convertToCardEntry card = do
          key <- KG.generateKey (KG.aes aesCTR l256) true [encrypt, decrypt, unwrapKey]
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
  masterKey              :: CryptoKey   <- liftAff $ KG.generateKey (KG.aes aesCTR l256) true [encrypt, decrypt, unwrapKey]
  masterKey2             :: CryptoKey   <- liftAff $ KG.generateKey (KG.aes aesCTR l256) true [encrypt, decrypt, unwrapKey]
  indexCardContent       :: ArrayBuffer <- liftAff $ encryptJson masterKey (Index (snd <$> cards))
  masterPassword         :: CryptoKey   <- liftAff $ KI.importKey raw pAb (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  indexCardContentHash   :: HexString   <- liftAff $ fromArrayBuffer <$> srpConf.hash (indexCardContent : Nil)
  masterKeyHex           :: HexString   <- liftAff $ fromArrayBuffer <$> exportKey raw masterKey
  masterKeyHex2          :: HexString   <- liftAff $ fromArrayBuffer <$> exportKey raw masterKey2
  let indexReference     =  IndexReference { reference: indexCardContentHash, masterKey: masterKeyHex, indexVersion: currentIndexVersion }

  let userPreferences    = UserPreferences { passwordGeneratorSettings: standardPasswordGeneratorSettings
                                           , automaticLock: Right 10
                                           }
  preferencesContent     :: ArrayBuffer <- liftAff $ encryptJson masterKey2 userPreferences
  preferencesContentHash :: HexString   <- liftAff $ fromArrayBuffer <$> srpConf.hash (preferencesContent : Nil)
  let preferencesReference = UserPreferencesReference { reference: preferencesContentHash, key: masterKeyHex2 }

  let userInfoReference = UserInfoReferences { preferencesReference, indexReference }

  masterKeyContent       :: HexString   <- liftAff $ fromArrayBuffer <$> encryptJson masterPassword userInfoReference
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
