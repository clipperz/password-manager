module Test.EncodeDecode where

import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Crypto.Subtle.Constants.AES as AES
import Crypto.Subtle.Encrypt as Encrypt
import Crypto.Subtle.Key.Generate as Key.Generate
import Crypto.Subtle.Key.Types as Key.Types
import Data.ArrayBuffer.Typed as Data.ArrayBuffer.Typed
import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView, Uint8)
import Data.Eq ((==))
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Ring ((*))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.TextDecoder as Decoder
import Data.TextEncoder as Encoder
import Data.Unit (Unit)
import DataModel.Card (Card(..), card0)
import Effect.Aff (Aff)
import Effect.Exception (Error, error)
import Effect.Class (liftEffect)
import Functions.EncodeDecode
-- import Crypto.Subtle.Constants.AES (aesCTR, l256)
-- import Crypto.Subtle.Key.Import as KI
-- import Crypto.Subtle.Key.Generate as KG
-- import Crypto.Subtle.Key.Types (encrypt, exportKey, decrypt, raw, unwrapKey, CryptoKey)
import Test.Spec (describe, it, SpecT)
import Test.Spec.Assertions (shouldEqual)
import Test.QuickCheck ((<?>), (===), Result(..))
-- import Test.Spec.QuickCheck (quickCheck)
import TestUtilities (makeTestableOnBrowser, quickCheckAffInBrowser, failOnBrowser, makeQuickCheckOnBrowser)
import Functions.ArrayBuffer (emptyByteArrayBuffer)

encodeDecodeSpec :: SpecT Aff Unit Identity Unit
encodeDecodeSpec = 
  describe "EncodeDecode" do
    let encryptDecrypt = "encrypt then decrypt"
    it encryptDecrypt do
      quickCheckAffInBrowser encryptDecrypt 10 encryptDecryptText
    let encryptDecryptJson = "encrypt then decrypt using json"
    it encryptDecryptJson do
      let card@(Card r) = Card { content: card0, timestamp: 1661377622, archived: false }
      masterKey :: Key.Types.CryptoKey <- Key.Generate.generateKey (Key.Generate.aes AES.aesCTR AES.l256) true [Key.Types.encrypt, Key.Types.decrypt, Key.Types.unwrapKey]
      encrypted <- encryptJson masterKey card
      result <- decryptJson masterKey encrypted
      case result of
        Left err -> failOnBrowser encryptDecryptJson (show err)
        Right (Card decrypted) -> makeTestableOnBrowser encryptDecryptJson decrypted shouldEqual r


encryptDecryptText :: String -> Aff Result
encryptDecryptText text = do
  let encodedText = Encoder.encode Encoder.Utf8 text :: ArrayView Uint8   -- Uint8Array
  let textBuffer = Data.ArrayBuffer.Typed.buffer encodedText :: ArrayBuffer
  let blockSize' = 16 :: Int
  let blockSizeInBits' = blockSize' * 8 :: Int
  let emptyCounter = emptyByteArrayBuffer blockSize' :: ArrayBuffer
  let algorithm = (Encrypt.aesCTR emptyCounter blockSizeInBits') :: Encrypt.EncryptAlgorithm

  key              :: Key.Types.CryptoKey    <- Key.Generate.generateKey (Key.Generate.aes AES.aesCTR AES.l256) false [Key.Types.encrypt, Key.Types.decrypt]

  encryptedData     :: ArrayBuffer     <- Encrypt.encrypt algorithm key textBuffer

  decryptedData :: Maybe ArrayBuffer <- Encrypt.decrypt algorithm key encryptedData
  decryptedDataView :: Either Error (ArrayView Uint8) <- case decryptedData of
      Nothing -> pure $ Left (error "Something went wrong")
      Just d  -> liftEffect $ Right <$> (Data.ArrayBuffer.Typed.whole d)

  let decoded = (decryptedDataView >>= (Decoder.decode Decoder.Utf8)  :: (Either Error String))
  pure $ case decoded of
    Left err -> Failed $ show err
    Right s -> text === s
