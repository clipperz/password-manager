module Test.EncodeDecode where

import Functions.EncodeDecode

import Control.Applicative (pure)
import Control.Bind (bind, discard, (=<<))
import Control.Category ((<<<))
import Crypto.Subtle.Key.Types as Key.Types
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (parseJson)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (decode, encode)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Eq (class Eq, (==))
import Data.Function (($))
import Data.HexString (fromArrayBuffer)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.String.CodePoints (toCodePointArray)
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Test.DebugCodec as Codec
import Test.QuickCheck (Result(..), (/==), (<?>))
import Test.Spec (describe, it, SpecT)
import TestClasses (UnicodeString, unicodeStringCodec)
import TestUtilities (makeQuickCheckOnBrowser, quickCheckAffInBrowser)

encodeDecodeSpec :: SpecT Aff Unit Identity Unit
encodeDecodeSpec = 
  describe "EncodeDecode" do
    let samples = 100

    let encodeDecode = "encodes then decode "
    it encodeDecode do
      makeQuickCheckOnBrowser (encodeDecode <> "unicode strings") samples encodeDecodeText
    let encryptDecrypt = "encrypt then decrypt ascii strings"
    it encryptDecrypt do
      quickCheckAffInBrowser encryptDecrypt samples (encryptDecryptWithCodec unicodeStringCodec)
    let encryptDecryptJson = "encrypt then decrypt using json"
    it encryptDecryptJson do
      quickCheckAffInBrowser (encryptDecryptJson <> " -- Card") samples (encryptDecryptWithCodec Codec.cardCodec)
    let encryptSameContent = "encrypt same content"
    it encryptSameContent do
      quickCheckAffInBrowser encryptSameContent samples (\value -> do
        masterKey :: Key.Types.CryptoKey <- generateCryptoKeyAesGCM
        encrypted1 <- encryptJson Codec.cardCodec masterKey value
        encrypted2 <- encryptJson Codec.cardCodec masterKey value
        pure $ (fromArrayBuffer encrypted1) /== (fromArrayBuffer encrypted2)
      )

encryptDecryptWithCodec :: forall a. Eq a => Show a =>  CA.JsonCodec a -> a -> Aff Result
encryptDecryptWithCodec codec value = do
  masterKey :: Key.Types.CryptoKey <- generateCryptoKeyAesGCM
  encrypted <- encryptJson codec masterKey value
  result    <- decryptJson codec masterKey encrypted
  pure $ case result of
    Left err -> Failed (show err)
    Right res  -> value == res <?> ((show value) <> " /= " <> (show res))

encodeDecodeWithCodec :: forall a. Eq a => Show a =>  CA.JsonCodec a -> a -> Result
encodeDecodeWithCodec codec value = do
  let encoded =  stringify   $ encode codec value
  let decoded = (lmap show <<< decode codec       ) =<< lmap show (parseJson encoded)
  case decoded of
    Left  err -> Failed (show err)
    Right res -> value == res <?> ((show value) <> " /= " <> (show res))

encodeDecodeText :: UnicodeString -> Result
encodeDecodeText text' = do
  let text = unwrap text'
  let encodedText = stringify $ encode CA.string text
  let decoded = (lmap show <<< decode CA.string) =<< lmap show (parseJson encodedText)
  case decoded of
    Left err -> Failed (show err)
    Right t -> text == t <?> (text <> " /= " <> t <> " => " <> (show (toCodePointArray text)) <> " /= " <> (show (toCodePointArray t)))
