module Test.EncodeDecode where

import Functions.EncodeDecode

import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Crypto.Subtle.Constants.AES as AES
import Crypto.Subtle.Encrypt as Encrypt
import Crypto.Subtle.Key.Generate as Key.Generate
import Crypto.Subtle.Key.Types as Key.Types
import Data.ArrayBuffer.Typed as Data.ArrayBuffer.Typed
import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView, Uint8)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Ring ((*))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.CodePoints (toCodePointArray)
import Data.TextDecoder as Decoder
import Data.TextEncoder as Encoder
import Data.Unit (Unit)
import DataModel.Card (Card(..), cardValues0)
import DataModel.Codec as Codec
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Functions.ArrayBuffer (emptyByteArrayBuffer)
import Test.QuickCheck ((<?>), (===), Result(..))
import Test.Spec (describe, it, SpecT)
import Test.Spec.Assertions (shouldEqual)
import TestClasses (AsciiString, UnicodeString)
import TestUtilities (makeTestableOnBrowser, quickCheckAffInBrowser, failOnBrowser, makeQuickCheckOnBrowser)

encodeDecodeSpec :: SpecT Aff Unit Identity Unit
encodeDecodeSpec = 
  describe "EncodeDecode" do
    let samples = 50

    let encodeDecode = "encodes then decode unicode strings"
    it encodeDecode do
      makeQuickCheckOnBrowser encodeDecode samples encodeDecodeText
    let encryptDecrypt = "encrypt then decrypt ascii strings"
    it encryptDecrypt do
      quickCheckAffInBrowser encryptDecrypt samples encryptDecryptText
    let encryptDecryptJson = "encrypt then decrypt using json"
    it encryptDecryptJson do
      let card@(Card r) = Card { content: cardValues0, secrets: [], timestamp: 1661377622.0, archived: false }
      masterKey :: Key.Types.CryptoKey <- Key.Generate.generateKey (Key.Generate.aes AES.aesCTR AES.l256) true [Key.Types.encrypt, Key.Types.decrypt, Key.Types.unwrapKey]
      encrypted <- encryptJson Codec.cardCodec masterKey card
      result    <- decryptJson Codec.cardCodec masterKey encrypted
      case result of
        Left err -> failOnBrowser encryptDecryptJson (show err)
        Right (Card decrypted) -> makeTestableOnBrowser encryptDecryptJson decrypted shouldEqual r


encryptDecryptText :: AsciiString -> Aff Result
encryptDecryptText text' = do
  let text = unwrap text'
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

encodeDecodeText :: UnicodeString -> Result
encodeDecodeText text' = do
  let text = unwrap text'
  let encodedText = Encoder.encode Encoder.Utf8 text :: ArrayView Uint8   -- Uint8Array
  let decoded = Decoder.decode Decoder.Utf8 encodedText
  case decoded of
    Left err -> Failed (show err)
    Right t -> text == t <?> (text <> " /= " <> t <> " => " <> (show (toCodePointArray text)) <> " /= " <> (show (toCodePointArray t)))
