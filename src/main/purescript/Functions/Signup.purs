module Functions.Signup where

import Control.Bind (bind, (>>=))
import Control.Applicative (pure)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
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
import DataModel.Card (Card, defaultCards, UserCard)
import DataModel.Credentials (Credentials)
import DataModel.Index (Index(..), CardEntry(..), CardReference(..), createCardEntry)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.ArrayBuffer (concatArrayBuffers)
import Functions.SRP as SRP
import Functions.EncodeDecode (encryptJson, encryptArrayBuffer)


type RegisterUserRequest = {
    user :: UserCard
  , indexCardReference :: HexString
  , indexCardContent   :: HexString
  , cards :: Array (Tuple HexString HexString)
}

prepareCards :: SRP.SRPConf -> List Card -> Aff (List (Tuple ArrayBuffer CardEntry))
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

prepareSignupParameters :: SRP.SRPConf -> Credentials -> Aff (Either SRP.SRPError RegisterUserRequest)
prepareSignupParameters conf form = runExceptT $ do
  cAb <- liftAff $ SRP.prepareC conf form.username form.password
  let c = fromArrayBuffer cAb
  pAb <- liftAff $ SRP.prepareP conf form.username form.password
  sAb <- liftAff $ SRP.randomArrayBuffer 32
  let salt = fromArrayBuffer sAb
  cards                :: List (Tuple ArrayBuffer CardEntry) <- ExceptT $ Right <$> prepareCards conf defaultCards 
  v                    :: HexString   <- ExceptT $ SRP.prepareV conf sAb pAb
  masterKey            :: CryptoKey   <- ExceptT $ Right <$> KG.generateKey (KG.aes aesCTR l256) true [encrypt, decrypt, unwrapKey]
  indexCardContent     :: ArrayBuffer <- ExceptT $ Right <$> encryptJson masterKey (Index_v1 (snd <$> cards))
  masterPassword       :: CryptoKey   <- ExceptT $ Right <$> KI.importKey raw pAb (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  indexCardContentHash :: ArrayBuffer <- ExceptT $ Right <$> conf.hash (indexCardContent : Nil)
  masterKeyAb          :: ArrayBuffer <- ExceptT $ Right <$> exportKey raw masterKey
  masterKeyContent     :: ArrayBuffer <- ExceptT $ Right <$> ((liftEffect $ concatArrayBuffers (masterKeyAb : indexCardContentHash : Nil)) >>= (encryptArrayBuffer masterPassword)) 
  pure  { user: { c: c
                , v: v
                , s: salt
                , srpVersion : "6a"
                , masterKeyEncodingVersion : "1.0"
                , masterKeyContent : fromArrayBuffer masterKeyContent
                }
        , indexCardReference : fromArrayBuffer indexCardContentHash
        , indexCardContent   : fromArrayBuffer indexCardContent
        , cards : fromFoldable ((\(Tuple encryptedCard (CardEntry_v1 { cardReference: (CardReference_v1 { reference }) })) -> (Tuple reference (fromArrayBuffer encryptedCard))) <$> cards)
        }
