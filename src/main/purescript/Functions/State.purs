module Functions.State where

import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Data.Either (Either(..))
import Data.Function (($))
import Data.Map.Internal (empty)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import DataModel.AppState (AppState, AppError, baseSRPInfo, HashState(..), KDFState(..))
import DataModel.AsyncValue (AsyncValue(..))
import DataModel.Proxy (Proxy(..))
import DataModel.SRP(SRPConf, KDF, HashFunction, concatKDF, hashFuncSHA1, hashFuncSHA256)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.JSState (getAppState, modifyAppState)

baseUrl :: String 
baseUrl = "http://localhost:8090" --TODO: get from configuration file/build

computeInitialState :: Effect AppState
computeInitialState = pure { proxy: (OnlineProxy baseUrl), sessionKey: Nothing, currentChallenge: Nothing, toll: (Loading Nothing), c: Nothing, p: Nothing, srpInfo: baseSRPInfo, hash: SHA256, cardsCache: empty, indexReference: Nothing }

resetState :: Aff Unit
resetState = (liftEffect computeInitialState) >>= modifyAppState

getKDFFromState :: KDFState -> KDF
getKDFFromState kdfState =
  case kdfState of
    ConcatKDF -> concatKDF

getHashFromState :: HashState -> HashFunction
getHashFromState hashState =
  case hashState of
    SHA256 -> hashFuncSHA256
    SHA1   -> hashFuncSHA1

getHashFunctionFromAppState :: AppState -> HashFunction
getHashFunctionFromAppState s = getHashFromState s.hash

getSRPConf :: Effect (Either AppError SRPConf)
getSRPConf = do
  eitherState <- getAppState
  pure $ case eitherState of
    Left err -> Left err 
    Right state -> Right $ getSRPConfFromState state

getSRPConfFromState :: AppState -> SRPConf
getSRPConfFromState state = { group: state.srpInfo.group, k: state.srpInfo.k, hash: getHashFromState state.hash, kdf: getKDFFromState state.srpInfo.kdf }
