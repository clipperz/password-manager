module WidgetManagers.SignupManager where

import Concur.Core (Widget)
import Concur.Core.FRP (demandLoop, loopW)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.State (StateT(..), modify_)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, withExceptT)
import Crypto.Subtle.Constants.AES (aesCTR, l256)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Generate as KG
import Crypto.Subtle.Key.Types (encrypt, exportKey, decrypt, raw, unwrapKey, CryptoKey)
import Data.Array (fromFoldable)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, fromArrayBuffer)
import Data.List.Types (List(..), (:))
import Data.Tuple (Tuple(..), snd)
import DataModel.AppState (AppState)
import DataModel.Card (Card, defaultCards)
import DataModel.Index (Index(..), CardEntry(..), CardReference(..), createCardEntry)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import EncodeDecode (encryptJson, encryptArrayBuffer)
import RestBackendCommunication
import SRP as SRP
import Utilities (concatArrayBuffers, makeStateT)
import Widgets.SignupForm (signupForm, SignupForm)

prepareCards :: SRP.SRPConf -> List Card -> Aff (List (Tuple ArrayBuffer CardEntry))
prepareCards conf cards = extractAff $ convertToCardEntry <$> cards
  where convertToCardEntry :: Card -> Aff (Tuple ArrayBuffer CardEntry)
        convertToCardEntry card = do
          key <- KG.generateKey (KG.aes aesCTR l256) true [encrypt, decrypt, unwrapKey]
          createCardEntry card key conf.hash
        
        extractAff :: List (Aff (Tuple ArrayBuffer CardEntry)) -> Aff (List (Tuple ArrayBuffer CardEntry))
        extractAff Nil = pure Nil
        extractAff (Cons elem list) = do
          e <- elem
          l <- extractAff list
          pure $ Cons e l

prepareSignupParameters :: SRP.SRPConf -> SignupForm -> Aff (Either SRP.SRPError RegisterUserRequest)
prepareSignupParameters conf form = runExceptT $ do
  cAb <- liftAff $ SRP.prepareC conf form.username form.password
  let c = fromArrayBuffer cAb
  pAb <- liftAff $ SRP.prepareP conf form.username form.password
  sAb <- liftAff $ SRP.randomArrayBuffer 32
  let salt = fromArrayBuffer sAb
  cards                :: List (Tuple ArrayBuffer CardEntry) <- ExceptT $ Right <$> prepareCards conf defaultCards 
  v                    :: HexString   <- ExceptT $ SRP.prepareV conf sAb pAb
  masterKey            :: CryptoKey   <- ExceptT $ Right <$> KG.generateKey (KG.aes aesCTR l256) true [encrypt, decrypt, unwrapKey]
  indexCardContent     :: ArrayBuffer <- ExceptT $ Right <$> encryptJson masterKey (Index_v1 (snd <$> cards))
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
        , cards : fromFoldable ((\(Tuple encryptedCard (CardEntry_v1 { cardReference: (CardReference_v1 { reference }) })) -> (Tuple reference (fromArrayBuffer encryptedCard))) <$> cards)
        }

signupManager :: SRP.SRPConf -> StateT AppState (Widget HTML) SignupForm
signupManager conf = makeStateT $ demandLoop "" (\s -> loopW (Left s) (\err -> do
  signupFormResult <- case err of
    Left  string -> div [] [text $ string, signupForm]
    Right _      -> signupForm
  liftAff $ runExceptT $ do
    singupParameters <- withExceptT (\_ -> "Registration failed") (ExceptT $ prepareSignupParameters conf signupFormResult)
    _                <- withExceptT (\_ -> "Registration failed") (ExceptT $ registerUser singupParameters)
    pure $ signupFormResult
))
