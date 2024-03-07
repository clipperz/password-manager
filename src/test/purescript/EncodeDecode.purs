module Test.EncodeDecode where

import Functions.EncodeDecode

import Control.Alt ((<#>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Crypto.Subtle.Key.Types as Key.Types
import Data.ArrayBuffer.Typed as Data.ArrayBuffer.Typed
import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView, Uint8)
import Data.Either (Either(..), hush)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (fromArrayBuffer)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.CodePoints (toCodePointArray)
import Data.TextDecoder as Decoder
import Data.TextEncoder as Encoder
import Data.Unit (Unit)
import DataModel.Card (Card(..), cardValues0)
import Test.DebugCodec as Codec
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Test.QuickCheck ((<?>), (===), Result(..))
import Test.Spec (describe, it, SpecT)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
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
      masterKey :: Key.Types.CryptoKey <- generateCryptoKeyAesGCM
      encrypted <- encryptJson Codec.cardCodec masterKey card
      result    <- decryptJson Codec.cardCodec masterKey encrypted
      case result of
        Left err -> failOnBrowser encryptDecryptJson (show err)
        Right (Card decrypted) -> makeTestableOnBrowser encryptDecryptJson decrypted shouldEqual r
    let encryptSameContent = "encrypt same content"
    it encryptSameContent do
      let card = Card { content: cardValues0, secrets: [], timestamp: 1661377622.0, archived: false }
      masterKey :: Key.Types.CryptoKey <- generateCryptoKeyAesGCM
      encrypted1 <- encryptJson Codec.cardCodec masterKey card
      encrypted2 <- encryptJson Codec.cardCodec masterKey card
      makeTestableOnBrowser encryptSameContent (fromArrayBuffer encrypted1) shouldNotEqual (fromArrayBuffer encrypted2)

encryptDecryptText :: AsciiString -> Aff Result
encryptDecryptText text' = do
  let text = unwrap text'
  let encodedText = Encoder.encodeUtf8 text :: ArrayView Uint8   -- Uint8Array
  let textBuffer = Data.ArrayBuffer.Typed.buffer encodedText :: ArrayBuffer

  key              :: Key.Types.CryptoKey    <- generateCryptoKeyAesGCM

  encryptedData     :: ArrayBuffer     <- encryptArrayBuffer key textBuffer

  decryptedData :: Maybe ArrayBuffer <- decryptArrayBuffer key encryptedData <#> hush
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
  let encodedText = Encoder.encodeUtf8 text :: ArrayView Uint8   -- Uint8Array
  let decoded = Decoder.decode Decoder.Utf8 encodedText
  case decoded of
    Left err -> Failed (show err)
    Right t -> text == t <?> (text <> " /= " <> t <> " => " <> (show (toCodePointArray text)) <> " /= " <> (show (toCodePointArray t)))
