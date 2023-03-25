module Functions.EncodeDecode where

import Control.Applicative (pure)
import Control.Bind ((>>=), bind)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, except, withExceptT)
import Control.Semigroupoid ((<<<))
import Crypto.Subtle.Encrypt as Encrypt
import Crypto.Subtle.Key.Types as Key.Types
import Data.Argonaut.Core as A
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Parser as P
import Data.ArrayBuffer.Typed as ABTyped
import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView, Uint8)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (fromArrayBuffer, toArrayBuffer, hex, toString, Base(..))
import Data.Maybe (Maybe(..))
import Data.Semiring ((*))
import Data.Show (show)
import Data.TextDecoder as Decoder
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception as EX
import Functions.ArrayBuffer (emptyByteArrayBuffer)

defaultBlockSize = 16 :: Int

getCounter :: Int -> Effect ArrayBuffer
-- getCounter = Fortuna.randomBytes blockSizeBytes
getCounter = pure <<< emptyByteArrayBuffer

setAES :: Int -> Effect Encrypt.EncryptAlgorithm
setAES blockSizeBytes = (getCounter blockSizeBytes) >>= (\c -> pure $ Encrypt.aesCTR c (blockSizeBytes * 8))

encryptWithAesCTR :: ArrayBuffer -> Key.Types.CryptoKey -> Aff ArrayBuffer
encryptWithAesCTR ab key = (liftEffect $ setAES defaultBlockSize) >>= (\alg -> Encrypt.encrypt alg key ab)

encryptStringWithAesCTR :: String -> Key.Types.CryptoKey -> Aff ArrayBuffer
encryptStringWithAesCTR s key = do
  alg :: Encrypt.EncryptAlgorithm <- liftEffect $ setAES defaultBlockSize
  Encrypt.encrypt alg key $ toArrayBuffer $ hex s

decryptWithAesCTR :: ArrayBuffer -> Key.Types.CryptoKey -> Aff (Either EX.Error ArrayBuffer)
decryptWithAesCTR ab key = do
  alg <- liftEffect $ setAES defaultBlockSize
  decryptedData <- Encrypt.decrypt alg key ab
  case decryptedData of
    Just decryptedAB -> pure $ Right decryptedAB
    Nothing          -> pure $ Left (EX.error "Decryption was not possible")

decryptStringWithAesCTR :: ArrayBuffer -> Key.Types.CryptoKey -> Aff (Either EX.Error String)
decryptStringWithAesCTR ab key = do
  alg <- liftEffect $ setAES defaultBlockSize
  decryptedData     :: Maybe ArrayBuffer <- Encrypt.decrypt alg key ab
  decryptedDataView :: Either EX.Error (ArrayView Uint8) <- case decryptedData of
      Nothing -> pure $ Left (EX.error "Something went wrong")
      Just d  -> liftEffect $ Right <$> (ABTyped.whole d)
  pure $ decryptedDataView >>= (Decoder.decode Decoder.Utf8)

encryptArrayBuffer ::  Key.Types.CryptoKey -> ArrayBuffer -> Aff ArrayBuffer
encryptArrayBuffer key ab = encryptWithAesCTR ab key

decryptArrayBuffer ::  Key.Types.CryptoKey -> ArrayBuffer -> Aff (Either EX.Error ArrayBuffer)
decryptArrayBuffer key ab = decryptWithAesCTR ab key

encryptJson :: forall a. EncodeJson a => Key.Types.CryptoKey -> a -> Aff ArrayBuffer
encryptJson key object = encryptWithAesCTR (toArrayBuffer $ hex (A.stringify $ encodeJson object)) key
  
decryptJson :: forall a. DecodeJson a => Key.Types.CryptoKey -> ArrayBuffer -> Aff (Either EX.Error a)
decryptJson key bytes = do
    result :: Either EX.Error a <- runExceptT $ do
      decryptedData :: ArrayBuffer <- ExceptT $ liftAff $ decryptWithAesCTR bytes key
      parsedJson  <- withExceptT (\err -> EX.error $ show err) (except $ P.jsonParser $ toString Dec $ fromArrayBuffer decryptedData)
      object :: a <- withExceptT (\err -> EX.error $ show err) (except $ decodeJson parsedJson)
      pure object
    pure result
