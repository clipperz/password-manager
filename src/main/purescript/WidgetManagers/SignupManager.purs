module WidgetManagers.SignupManager where

import Control.Bind (bind, (>>=))
import Concur.Core (Widget)
import Concur.Core.FRP (demandLoop, loopW)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Control.Applicative (pure)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, withExceptT)
import Crypto.Subtle.Constants.AES (aesCTR, l256)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Generate as KG
import Crypto.Subtle.Key.Types (encrypt, exportKey, decrypt, raw, unwrapKey, CryptoKey)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, fromArrayBuffer)
import Data.List.Types (List(..), (:))
import DataModel.IndexCard (emptyIndexCard)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import EncodeDecode (encryptJson, encryptArrayBuffer)
import RestBackendCommunication
import SRP as SRP
import Utilities (concatArrayBuffers)
import Widgets.SignupForm (signupForm, SignupForm)


prepareSignupParameters :: SRP.SRPConf -> SignupForm -> Aff (Either SRP.SRPError RegisterUserRequest)
prepareSignupParameters conf form = runExceptT $ do
  cAb <- liftAff $ SRP.prepareC conf form.username form.password
  let c = fromArrayBuffer cAb
  pAb <- liftAff $ SRP.prepareP conf form.username form.password
  sAb <- liftAff $ SRP.randomArrayBuffer 32
  let salt = fromArrayBuffer sAb
  v                    :: HexString   <- ExceptT $ SRP.prepareV conf sAb pAb
  masterKey            :: CryptoKey   <- ExceptT $ Right <$> KG.generateKey (KG.aes aesCTR l256) true [encrypt, decrypt, unwrapKey]
  indexCardContent     :: ArrayBuffer <- ExceptT $ Right <$> encryptJson masterKey emptyIndexCard
  masterPassword       :: CryptoKey   <- ExceptT $ Right <$> KI.importKey raw pAb (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  indexCardContentHash :: ArrayBuffer <- ExceptT $ Right <$> conf.hash (indexCardContent : Nil)
  masterKeyAb          :: ArrayBuffer <- ExceptT $ Right <$> exportKey raw masterKey
  masterKeyContent     :: ArrayBuffer <- ExceptT $ Right <$> ((liftEffect $ concatArrayBuffers (masterKeyAb : indexCardContentHash : Nil)) >>= (encryptArrayBuffer masterPassword)) 
  pure  { user: { c: c
                , v: v
                , s: salt
                , srpVersion : "6a"
                , masterKeyEncodingVersion : "1.0"
                , masterKeyContent : fromArrayBuffer masterKeyContent
                }
        , indexCardReference : fromArrayBuffer indexCardContentHash
        , indexCardContent   : fromArrayBuffer indexCardContent
        }

signupManager :: SRP.SRPConf -> Widget HTML SignupForm
signupManager conf = demandLoop "" (\s -> loopW (Left s) (\err -> do
  signupFormResult <- case err of
    Left  string -> div [] [text $ string, signupForm]
    Right _      -> signupForm
  liftAff $ runExceptT $ do
    singupParameters <- withExceptT (\_ -> "Registration failed") (ExceptT $ prepareSignupParameters conf signupFormResult)
    _                <- withExceptT (\_ -> "Registration failed") (ExceptT $ registerUser singupParameters)
    pure $ signupFormResult
))
