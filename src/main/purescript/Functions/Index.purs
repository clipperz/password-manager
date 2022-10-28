module Functions.Index where

import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), withExceptT, except)
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (encrypt, decrypt, raw, unwrapKey, CryptoKey)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, toArrayBuffer)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.User (IndexReference)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception as EX
import Functions.EncodeDecode (decryptJson)
import Functions.JSState (getAppState)

decryptIndexReference :: HexString -> ExceptT AppError Aff IndexReference
decryptIndexReference encryptedRef = do
  currentState <- ExceptT $ liftEffect getAppState
  case currentState of
    { c: _, p: Just p, proxy: _, sessionKey: _, toll: _ } -> do
      masterPassword :: CryptoKey <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer p) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      mapCryptoError $ ExceptT $ decryptJson masterPassword (toArrayBuffer encryptedRef)
    _ -> except $ Left $ InvalidStateError $ MissingValue "Missing p"

  where 
    mapCryptoError :: forall a. ExceptT EX.Error Aff a -> ExceptT AppError Aff a
    mapCryptoError = withExceptT (\e -> ProtocolError $ CryptoError $ "Decrypt indexReference: " <> EX.message e)