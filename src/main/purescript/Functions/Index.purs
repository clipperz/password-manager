module Functions.Index where

import Control.Alt ((<#>), (<$>))
import Control.Applicative (pure)
import Control.Bind (bind, (=<<))
import Control.Monad.Except (throwError)
import Control.Monad.Except.Trans (ExceptT(..), withExceptT)
import Control.Semigroupoid ((>>>))
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Codec.Argonaut (JsonCodec)
import Data.Function ((#), ($))
import Data.HexString (HexString, fromArrayBuffer, toArrayBuffer)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import DataModel.AppError (AppError(..))
import DataModel.AppState (AppState, InvalidStateError(..), ProxyResponse(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.IndexVersions.CurrentIndexVersions (currentIndexCodecVersion, currentIndexVersion)
import DataModel.IndexVersions.Index (class IndexVersions, Index(..), IndexVersion(..), fromIndex, toIndex)
import DataModel.IndexVersions.IndexV1 (indexV1Codec)
import DataModel.SRPVersions.SRP (HashFunction)
import DataModel.UserVersions.User (IndexReference(..), UserInfo(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.Communication.Backend (ConnectionState)
import Functions.Communication.Blobs (deleteBlob, getBlob, postBlob)
import Functions.Communication.Users (UpdateUserStateUpdateInfo, updateUserInfo)
import Functions.EncodeDecode (decryptJson, encryptJson, exportCryptoKeyToHex, generateCryptoKeyAesGCM, importCryptoKeyAesGCM)

decryptIndex :: ArrayBuffer -> IndexReference -> ExceptT AppError Aff Index
decryptIndex encryptedIndex (IndexReference {version, key}) =
  case version of 
    IndexVersion_1 -> decryptJsonIndex indexV1Codec

  where
    decryptJsonIndex :: forall a. IndexVersions a => JsonCodec a -> ExceptT AppError Aff Index
    decryptJsonIndex codec = toIndex <$> ExceptT ((\cryptoKey -> decryptJson codec cryptoKey encryptedIndex) =<< (importCryptoKeyAesGCM (toArrayBuffer key))) # mapError

    mapError :: forall a e. Show e => ExceptT e Aff a -> ExceptT AppError Aff a
    mapError = withExceptT (show >>> CryptoError >>> ProtocolError)

encryptIndex :: Index -> HashFunction -> Aff (Tuple ArrayBuffer IndexReference)
encryptIndex index hashFunc = do
  indexKey           :: CryptoKey   <- generateCryptoKeyAesGCM
  encryptedIndex     :: ArrayBuffer <- encryptJson currentIndexCodecVersion indexKey (fromIndex index)
  indexKeyHex        :: HexString   <- exportCryptoKeyToHex indexKey
  encryptedIndexHash :: HexString   <- hashFunc (encryptedIndex : Nil) <#> fromArrayBuffer
  pure $ Tuple encryptedIndex (IndexReference { reference: encryptedIndexHash, key: indexKeyHex, version: currentIndexVersion } )

-- ----------------------------------------------------------------------------------------

getIndex :: ConnectionState -> IndexReference -> ExceptT AppError Aff (ProxyResponse Index)
getIndex connectionState indexRef@(IndexReference { reference }) = do
  ProxyResponse proxy blob <- getBlob connectionState reference
  decryptIndex blob indexRef <#> ProxyResponse proxy

postIndex :: ConnectionState -> Index -> ExceptT AppError Aff (ProxyResponse IndexReference)
postIndex connectionState@{hashFunc} index@(Index {identifier}) = do
  Tuple encryptedIndex indexReference@(IndexReference {reference}) <- liftAff $ encryptIndex index hashFunc
  ProxyResponse proxy _                                            <- postBlob connectionState encryptedIndex reference identifier
  pure $ ProxyResponse proxy indexReference

deleteIndex :: ConnectionState -> IndexReference -> Index ->  ExceptT AppError Aff (ProxyResponse Unit)
deleteIndex connectionState (IndexReference {reference}) (Index {identifier}) = do
  ProxyResponse proxy _ <- deleteBlob connectionState reference identifier
  pure $ ProxyResponse proxy unit

updateIndex :: AppState -> Index -> ExceptT AppError Aff (ProxyResponse UpdateUserStateUpdateInfo)

updateIndex state@{ c: Just c, p: Just p, proxy, hash: hashFunc, userInfo: Just (UserInfo userInfo@{indexReference}), index: Just oldIndex, srpConf } newIndex = do
  let connectionState = {proxy, hashFunc, srpConf, c, p}

  ProxyResponse proxy'  newIndexReference <- postIndex   connectionState                                 newIndex
  ProxyResponse proxy'' _                 <- deleteIndex connectionState {proxy = proxy'} indexReference oldIndex
  -- -------------------
  updateUserInfo (state {proxy = proxy''}) (UserInfo userInfo { indexReference = newIndexReference })

updateIndex _ _ = 
  throwError $ InvalidStateError (MissingValue "Corrupted State")