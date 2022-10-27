module Functions.State where

import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Map.Internal (empty)
import Data.Maybe (Maybe(..))
import DataModel.AppState (AppState, AppError, baseSRPInfo, HashState(..), KDFState(..))
import DataModel.AsyncValue (AsyncValue(..))
import DataModel.Proxy (Proxy(..))
import DataModel.SRP(SRPConf, KDF, HashFunction, concatKDF, hashFuncSHA1, hashFuncSHA256)
import Effect (Effect)
import Functions.JSState (getAppState)

baseUrl :: String 
baseUrl = "http://localhost:8090" --TODO: get from configuration file/build

computeInitialState :: Effect AppState
computeInitialState = pure { proxy: (OnlineProxy baseUrl), sessionKey: Nothing, currentChallenge: Nothing, toll: (Loading Nothing), c: Nothing, p: Nothing, srpInfo: baseSRPInfo, hash: SHA256, cardsCache: empty }

getKDFFromState :: KDFState -> KDF
getKDFFromState kdfState =
  case kdfState of
    ConcatKDF -> concatKDF

getHashFromState :: HashState -> HashFunction
getHashFromState hashState =
  case hashState of
    SHA256 -> hashFuncSHA256
    SHA1   -> hashFuncSHA1

getSRPConf :: Effect (Either AppError SRPConf)
getSRPConf = do
  eitherState <- getAppState
  pure $ case eitherState of
    Left err -> Left err 
    Right state -> Right $ getSRPConfFromState state

getSRPConfFromState :: AppState -> SRPConf
getSRPConfFromState state = { group: state.srpInfo.group, k: state.srpInfo.k, hash: getHashFromState state.hash, kdf: getKDFFromState state.srpInfo.kdf }
