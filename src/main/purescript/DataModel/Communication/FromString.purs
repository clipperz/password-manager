module DataModel.Communication.FromString where

import Control.Applicative (pure)
import Control.Semigroupoid ((<<<))
import Data.Argonaut.Core as DAC
import Data.Argonaut.Parser as DAP
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Function (($))
import Data.HexString (hex, toArrayBuffer)
import Data.MediaType (MediaType(..))
import Data.Unit (Unit, unit)
import Effect (Effect)
import Web.File.Blob as WFB
import Web.DOM.Document as WDD

foreign import toDocument :: String -> Effect WDD.Document

class FromString a where
  fromString :: String -> Effect a

instance abFromString :: FromString ArrayBuffer where
  fromString = pure <<< toArrayBuffer <<< hex

instance blobFromString :: FromString WFB.Blob where
  fromString s = pure $ WFB.fromString s (MediaType "text/plain")

instance documentFromString :: FromString WDD.Document where
  fromString = toDocument

instance stringFromString :: FromString String where
  fromString = pure

instance jsonFromString :: FromString DAC.Json where
  fromString s = 
    pure $ case DAP.jsonParser s of
      Left err -> DAC.fromString s
      Right json -> json

instance unitFromString :: FromString Unit where
  fromString _ = pure unit
