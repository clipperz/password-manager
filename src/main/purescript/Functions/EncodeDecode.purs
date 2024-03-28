module Functions.EncodeDecode where

import Control.Alt ((<$>))
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, except, withExceptT)
import Crypto.Subtle.Constants.AES (aesGCM, l256, t128)
import Crypto.Subtle.Encrypt as Encrypt
import Crypto.Subtle.Key.Generate as KG
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (decrypt, encrypt, exportKey)
import Crypto.Subtle.Key.Types as Key.Types
import Data.Argonaut.Core as A
import Data.Argonaut.Parser as P
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Codec.Argonaut (JsonCodec, decode, encode)
import Data.CommutativeRing ((*))
import Data.Either (Either(..))
import Data.EuclideanRing ((/))
import Data.Function (($))
import Data.HexString (Base(..), HexString, fromArrayBuffer, hex, splitHexAt, toArrayBuffer, toString)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception as EX
import Functions.ArrayBuffer (concatArrayBuffers)
import Functions.SRP (randomArrayBuffer)

defaultIVSize = 96 :: Int

setAesGCM :: ArrayBuffer -> Encrypt.EncryptAlgorithm
setAesGCM iv = Encrypt.aesGCM iv Nothing (Just t128)

generateCryptoKeyAesGCM :: Aff Key.Types.CryptoKey
generateCryptoKeyAesGCM = KG.generateKey (KG.aes aesGCM l256) true [encrypt, decrypt, Key.Types.unwrapKey]

importCryptoKeyAesGCM :: ArrayBuffer -> Aff Key.Types.CryptoKey
importCryptoKeyAesGCM key = KI.importKey Key.Types.raw key (KI.aes aesGCM) false [encrypt, decrypt, Key.Types.unwrapKey]

exportCryptoKeyToHex :: Key.Types.CryptoKey -> Aff HexString
exportCryptoKeyToHex key = fromArrayBuffer <$> exportKey Key.Types.raw key

encryptWithAesGCM :: ArrayBuffer -> Key.Types.CryptoKey -> Aff ArrayBuffer
encryptWithAesGCM ab key = do
  iv <- (randomArrayBuffer (defaultIVSize/8))
  Encrypt.encrypt (setAesGCM iv) key ab >>= (\encryptedContent -> liftEffect $ concatArrayBuffers (iv:encryptedContent:Nil))

decryptWithAesGCM :: ArrayBuffer -> Key.Types.CryptoKey -> Aff (Either EX.Error ArrayBuffer)
decryptWithAesGCM ab key = do
  let {before: iv, after: payload} = splitHexAt ((defaultIVSize / 8) * 2) (fromArrayBuffer ab) 
  decryptedData <- Encrypt.decrypt (setAesGCM (toArrayBuffer iv)) key (toArrayBuffer payload)
  case decryptedData of
    Just decryptedAB -> pure $ Right decryptedAB
    Nothing          -> pure $ Left (EX.error "Decryption was not possible")

encryptArrayBuffer ::  Key.Types.CryptoKey -> ArrayBuffer -> Aff ArrayBuffer
encryptArrayBuffer key ab = encryptWithAesGCM ab key

decryptArrayBuffer ::  Key.Types.CryptoKey -> ArrayBuffer -> Aff (Either EX.Error ArrayBuffer)
decryptArrayBuffer key ab = decryptWithAesGCM ab key

encryptJson :: forall a. JsonCodec a -> Key.Types.CryptoKey -> a -> Aff ArrayBuffer
encryptJson codec key object = encryptWithAesGCM (toArrayBuffer $ hex (A.stringify $ encode codec object)) key
  
decryptJson :: forall a. JsonCodec a -> Key.Types.CryptoKey -> ArrayBuffer -> Aff (Either EX.Error a)
decryptJson codec key bytes = do
    result :: Either EX.Error a <- runExceptT $ do
      decryptedData :: ArrayBuffer <- ExceptT $ liftAff $ decryptWithAesGCM bytes key
      parsedJson  <- withExceptT (\err -> EX.error $ show err) (except $ P.jsonParser $ toString Dec $ fromArrayBuffer decryptedData)
      object :: a <- withExceptT (\err -> EX.error $ show err) (except $ decode codec parsedJson)
      pure object
    pure result
