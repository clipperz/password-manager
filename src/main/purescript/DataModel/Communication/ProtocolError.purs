module DataModel.Communication.ProtocolError where

import Affjax.Web as AXW
import Data.Semigroup ((<>))
import Data.Show (class Show, show)

data ProtocolError = RequestError AXW.Error 
                   | ResponseError Int 
                   | SRPError String 
                   | DecodeError String 
                   | CryptoError String
                   | IllegalRequest String
                   | IllegalResponse String
instance showProtocolError :: Show ProtocolError where
  show (RequestError err)     = "Request Error: "  <> AXW.printError err
  show (ResponseError i)      = "Response Error: " <> "response status code " <> show i
  show (SRPError err)         = "SRP Error: "      <> err
  show (DecodeError err)      = "Decode Error: "   <> err
  show (CryptoError err)      = "Crypto Error: "   <> err
  show (IllegalRequest err)   = "Illegal request: "   <> err
  show (IllegalResponse err)  = "Illegal response: "   <> err
