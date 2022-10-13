module DataModel.Communication.ProtocolError where

import Affjax.Web as AXW
import Data.PrettyShow (class PrettyShow, prettyShow)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)

data ProtocolError = RequestError AXW.Error 
                   | ResponseError Int 
                   | SRPError String 
                   | DecodeError String -- for errors while decoding JSONs
                   | CryptoError String -- for errors while decoding crypted things
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

instance prettyShowProtocolError :: PrettyShow ProtocolError where
  prettyShow (RequestError err)     = "An network error happened during the operation, the servery may be unreachable: please retry."
  prettyShow (ResponseError i)      = "The server could not satisfy your request, the HTTP Error code returned is " <> show i
  prettyShow (SRPError err)         = "There was an error in the completion of the SRP protocol, please retry."
  prettyShow (DecodeError err)      = "The data obtained from the server is not in a comprehensible format, please contact us!"
  prettyShow (CryptoError err)      = "Your encryption/decryption operation didn't work, please contact us!"
  prettyShow (IllegalRequest err)   = "The application is not working correctly, please restart it."
  prettyShow (IllegalResponse err)  = "The server is not working correctly, please retry in a bit."
