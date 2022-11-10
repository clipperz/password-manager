module Functions.State where

import Control.Applicative (pure)
import Control.Bind (bind, (>>=), discard)
import Control.Monad.Except.Trans (ExceptT(..), mapExceptT, except)
import Control.Semigroupoid ((<<<))
import Data.Array (filter, catMaybes, head)
import Data.Either (Either(..), note)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Map.Internal (empty)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import DataModel.AppState (AppState, AppError(..), baseSRPInfo, HashState(..), KDFState(..))
import DataModel.AsyncValue (AsyncValue(..))
import DataModel.Proxy (Proxy(..), BackendSessionState(..))
import DataModel.SRP(SRPConf, KDF, HashFunction, concatKDF, hashFuncSHA1, hashFuncSHA256)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.JSState (getAppState, modifyAppState)
import Record (merge)
import Web.DOM.Element (fromNode, id)
import Web.DOM.Node (childNodes)
import Web.DOM.NodeList (toArray)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toNode)
import Web.HTML.Location (origin)
import Web.HTML.Window (location, document)

offlineDataId = "offlineData"

computeInitialState :: ExceptT AppError Effect AppState
computeInitialState = do
  w <- ExceptT $ Right <$> window
  b <- ExceptT $ note (CannotInitState "incorrectly formatted HTML body") <$> (document w >>= body)
  childs <- ExceptT $ Right <$> (childNodes (toNode b) >>= toArray)
  elementsWithId <- ExceptT $ Right <$> (sequence $ mapIds <$> (catMaybes $ fromNode <$> childs))
  let script = head ((\(Tuple e _) -> e) <$> (filter (\(Tuple _ i) -> i == offlineDataId) elementsWithId))
  case script of
    Just elem -> except $ Right $ withOfflineProxy
    Nothing -> do
      l <- ExceptT $ Right <$> ((location w) >>= origin)
      except $ Right $ withOnlineProxy l

  where 
    mapIds e = (Tuple e) <$> (id e)

    withOfflineProxy = merge { proxy: OfflineProxy (BackendSessionState { b: Nothing, aa: Nothing, bb: Nothing }) } baseState
    withOnlineProxy url = merge { proxy: (OnlineProxy url) } baseState
    baseState = { currentChallenge: Nothing
                , sessionKey: Nothing
                , toll: (Loading Nothing)
                , username: Nothing
                , password: Nothing
                , c: Nothing
                , p: Nothing
                , srpInfo: baseSRPInfo
                , hash: SHA256
                , cardsCache: empty
                , indexReference: Nothing 
                , userPreferences: Nothing
                }

resetState :: ExceptT AppError Aff Unit
resetState = do
  is <- mapExceptT liftEffect computeInitialState
  ExceptT $ Right <$> (modifyAppState is)

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

isOfflineCopy :: AppState -> Boolean
isOfflineCopy state@{ proxy } =
  case proxy of
    OfflineProxy _ -> true
    _ -> false
