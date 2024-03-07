module Test.Codec
  ( codecSpec
  )
  where

import Control.Alt ((<$>))
import Control.Bind (discard, (=<<))
import Control.Category ((<<<))
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonDecodeError(..))
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Function (($))
import Data.Identity (Identity)
import Data.Unit (Unit)
import Test.DebugCodec as Codec
import DataModel.Password (standardPasswordGeneratorSettings)
import Effect.Aff (Aff)
import Test.Spec (describe, it, SpecT)
import Test.Spec.Assertions (shouldEqual)
import TestUtilities (makeTestableOnBrowser)

encodeDecode :: forall a. CA.Codec (Either JsonDecodeError) Json Json a a -> a -> (Either JsonDecodeError a)
encodeDecode codec a = CA.decode codec $ CA.encode codec a

decodeEncode :: forall a. CA.Codec (Either JsonDecodeError) Json Json a a -> String -> (Either JsonDecodeError String)
decodeEncode codec json = (stringify <<< CA.encode codec) <$> (CA.decode codec =<< (lmap TypeMismatch $ jsonParser json))


codecSpec :: SpecT Aff Unit Identity Unit
codecSpec =
  describe "Codec" do

    it "codecTests" do
      makeTestableOnBrowser "passwordGeneratorSettingsCodec" (encodeDecode Codec.passwordGeneratorSettingsCodec standardPasswordGeneratorSettings) shouldEqual (Right standardPasswordGeneratorSettings)
      makeTestableOnBrowser "overlayStatusCodec"             (decodeEncode Codec.overlayStatusCodec             """{"tag":"hidden"}""")            shouldEqual (Right """{"tag":"hidden"}""")
      makeTestableOnBrowser "overlayColorCodec"              (decodeEncode Codec.overlayColorCodec              """{"tag":"black"}""")             shouldEqual (Right """{"tag":"black"}""")
