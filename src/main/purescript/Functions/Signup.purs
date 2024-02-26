module Functions.Signup where

import Control.Alt ((<#>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.Array (fromFoldable)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either)
import Data.EuclideanRing ((/))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, fromArrayBuffer)
import Data.List.Types (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import DataModel.Card (Card, defaultCards)
import DataModel.Codec as Codec
import DataModel.Communication.Signup (RegisterUserRequest)
import DataModel.Credentials (Credentials)
import DataModel.Index (CardEntry, Index, currentIndexVersion, prepareIndex)
import DataModel.SRP (SRPConf, SRPError)
import DataModel.User (IndexReference(..), MasterKey, RequestUserCard(..), SRPVersion(..), UserInfo(..), defaultUserPreferences)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.Communication.Users (computeMasterKey)
import Functions.EncodeDecode (encryptJson, exportCryptoKeyToHex, generateCryptoKeyAesGCM, importCryptoKeyAesGCM)
import Functions.Index (createCardEntry)
import Functions.SRP (randomArrayBuffer)
import Functions.SRP as SRP

prepareCards :: SRPConf -> List Card -> Aff (List (Tuple CardEntry {cardContent :: HexString, cardReference :: HexString, cardIdentifier :: HexString}))
prepareCards srpConf cards = sequence $ cards <#> (\card -> do
    Tuple content entry <- createCardEntry srpConf.hash card
    identifier          <- fromArrayBuffer <$> randomArrayBuffer (256/8)
    pure $ Tuple entry {cardContent: fromArrayBuffer content, cardReference: (unwrap (unwrap entry).cardReference).reference, cardIdentifier: identifier}
  )

prepareSignupParameters :: SRPConf -> Credentials -> Aff (Either SRPError RegisterUserRequest)
prepareSignupParameters srpConf form = runExceptT $ do
  c   <- liftAff $ SRP.prepareC srpConf form.username form.password <#> fromArrayBuffer
  pAb <- liftAff $ SRP.prepareP srpConf form.username form.password
  sAb <- liftAff $ SRP.randomArrayBuffer 32
  v   <- ExceptT $ SRP.prepareV srpConf sAb pAb
  
  cards <- liftAff $ prepareCards srpConf defaultCards 
  
  index                :: Index       <- liftAff $ prepareIndex (fst <$> cards)
  indexCryptoKey       :: CryptoKey   <- liftAff $ generateCryptoKeyAesGCM
  indexCardContent     :: ArrayBuffer <- liftAff $ encryptJson Codec.indexCodec indexCryptoKey index
  indexCryptoKeyHex    :: HexString   <- liftAff $ exportCryptoKeyToHex indexCryptoKey
  indexCardContentHash :: HexString   <- liftAff $ fromArrayBuffer <$> srpConf.hash (indexCardContent : Nil)
  let indexReference    = IndexReference         { reference: indexCardContentHash, masterKey: indexCryptoKeyHex, indexVersion: currentIndexVersion }
  
  userInfoIdentifier   :: HexString   <- liftAff $ fromArrayBuffer <$> randomArrayBuffer (256/8)
  let userInfo          = UserInfo               { indexReference, userPreferences: defaultUserPreferences, identifier: userInfoIdentifier }
  userInfoCryptoKey    :: CryptoKey   <- liftAff $ generateCryptoKeyAesGCM
  encryptedUserInfo    :: ArrayBuffer <- liftAff $ encryptJson Codec.userInfoCodec userInfoCryptoKey userInfo
  userInfoHash         :: ArrayBuffer <- liftAff $ srpConf.hash (encryptedUserInfo : Nil)

  masterPassword       :: CryptoKey   <- liftAff $ importCryptoKeyAesGCM pAb
  masterKey            :: MasterKey   <- liftAff $ computeMasterKey userInfoHash userInfoCryptoKey masterPassword

  pure  { user:
            RequestUserCard
              { c: c
              , v: v
              , s: fromArrayBuffer sAb
              , srpVersion: V_6a
              , masterKey
              , originMasterKey: Nothing
              }
        , p :                   fromArrayBuffer pAb
        , userInfoReference:    fromArrayBuffer userInfoHash
        , userInfoContent:      fromArrayBuffer encryptedUserInfo
        , userInfoIdentifier
        , indexCardReference:   indexCardContentHash
        , indexCardContent:     fromArrayBuffer indexCardContent
        , indexCardIdentifier: (unwrap index).identifier
        , cards:                fromFoldable $ snd <$> cards
        }
