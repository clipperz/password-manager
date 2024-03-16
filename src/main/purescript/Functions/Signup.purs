module Functions.Signup
  ( prepareSignupParameters
  )
  where

import Control.Alt ((<#>))
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
import Data.Newtype (unwrap)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import DataModel.CardVersions.Card (Card, defaultCards)
import DataModel.Communication.Signup (RegisterUserRequest)
import DataModel.Credentials (Credentials)
import DataModel.IndexVersions.Index (CardEntry(..), CardReference(..), Index, prepareIndex)
import DataModel.SRPVersions.CurrentSRPVersions (currentSRPVersion)
import DataModel.SRPVersions.SRP (SRPConf, SRPError)
import DataModel.UserVersions.CurrentUserVersions (currentUserInfoCodecVersion)
import DataModel.UserVersions.User (MasterKey, RequestUserCard(..), UserInfo, defaultUserPreferences, prepareUserInfo)
import DataModel.UserVersions.UserCodecs (fromUserInfo)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.Card (createCardEntry)
import Functions.Communication.Users (computeMasterKey)
import Functions.EncodeDecode (encryptJson, exportCryptoKeyToHex, generateCryptoKeyAesGCM, importCryptoKeyAesGCM)
import Functions.Index (encryptIndex)
import Functions.SRP as SRP

prepareCards :: SRPConf -> List Card -> Aff (List (Tuple CardEntry {cardContent :: HexString, cardReference :: HexString, cardIdentifier :: HexString}))
prepareCards srpConf cards = sequence $ cards <#> (\card -> do
    Tuple content entry@(CardEntry {cardReference: CardReference {reference, identifier}}) <- createCardEntry srpConf.hash card
    pure $ Tuple entry {cardContent: fromArrayBuffer content, cardReference: reference, cardIdentifier: identifier}
  )

prepareSignupParameters :: SRPConf -> Credentials -> Aff (Either SRPError RegisterUserRequest)
prepareSignupParameters srpConf form = runExceptT $ do
  c   <- liftAff $ SRP.prepareC srpConf form.username form.password <#> fromArrayBuffer
  pAb <- liftAff $ SRP.prepareP srpConf form.username form.password
  sAb <- liftAff $ SRP.randomArrayBuffer 32
  v   <- ExceptT $ SRP.prepareV srpConf sAb pAb
  
  cards <- liftAff $ prepareCards srpConf defaultCards 
  
  index                :: Index       <- liftAff $ prepareIndex (fst <$> cards)
  Tuple encryptedIndex indexReference <- liftAff $ encryptIndex index srpConf.hash

  userInfo             :: UserInfo    <- liftAff $ prepareUserInfo indexReference defaultUserPreferences

  userInfoCryptoKey    :: CryptoKey   <- liftAff $ generateCryptoKeyAesGCM
  encryptedUserInfo    :: ArrayBuffer <- liftAff $ encryptJson currentUserInfoCodecVersion userInfoCryptoKey (fromUserInfo userInfo)
  userInfoHash         :: ArrayBuffer <- liftAff $ srpConf.hash (encryptedUserInfo : Nil)

  userInfoCryptoKeyHex :: HexString   <- liftAff $ exportCryptoKeyToHex userInfoCryptoKey
  masterPassword       :: CryptoKey   <- liftAff $ importCryptoKeyAesGCM pAb
  masterKey            :: MasterKey   <- liftAff $ computeMasterKey {reference: fromArrayBuffer userInfoHash, key: userInfoCryptoKeyHex} masterPassword

  pure  { user:
            RequestUserCard
              { c: c
              , v: v
              , s: fromArrayBuffer sAb
              , srpVersion: currentSRPVersion
              , masterKey
              , originMasterKey: Nothing
              }
        , p :                   fromArrayBuffer pAb
        , userInfoReference:    fromArrayBuffer userInfoHash
        , userInfoContent:      fromArrayBuffer encryptedUserInfo
        , userInfoIdentifier:   (unwrap userInfo).identifier
        , indexCardReference:   (unwrap indexReference).reference
        , indexCardContent:     fromArrayBuffer encryptedIndex
        , indexCardIdentifier: (unwrap index).identifier
        , cards:                fromFoldable $ snd <$> cards
        }
