module Functions.State where

import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array (filter, catMaybes, head)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Map.Internal (empty)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unit (Unit)
import DataModel.AppState (AppError, AppState, HashState(..), KDFState(..), ProxyConnectionStatus(..), SRPInfo)
import DataModel.AsyncValue (AsyncValue(..))
import DataModel.ProxyType (ProxyType(..), BackendSessionState(..))
import DataModel.SRP (HashFunction, KDF, SRPConf, concatKDF, group1024, hashFuncSHA1, hashFuncSHA256, k)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.JSState (getAppState, saveAppState)
import Record (merge)
import Web.DOM (Element, Node)
import Web.DOM.Element (fromNode, id)
import Web.DOM.Node (childNodes)
import Web.DOM.NodeList (toArray)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toNode)
import Web.HTML.Window (document)

offlineDataId :: String
offlineDataId = "offlineData"

computeInitialState :: Effect AppState
computeInitialState = do
  script <- runMaybeT do
    body            :: HTMLElement                  <- MaybeT $ (window >>= document >>= body)
    childs          :: Array Node                   <- liftEffect $ (childNodes (toNode body) >>= toArray)
    elementsWithId  :: Array (Tuple Element String) <- liftEffect $ sequence $ mapIds <$> (catMaybes $ fromNode <$> childs)
    MaybeT $ pure $ fst <$> head (filter (\element_id -> (snd element_id) == offlineDataId) elementsWithId)
  case script of
    Just _  -> pure $ withOfflineProxy
    Nothing -> pure ( withOnlineProxy "/api")

  where 
    mapIds :: Element -> Effect (Tuple Element String)
    mapIds e = (Tuple e) <$> (id e)

    baseSRPInfo :: SRPInfo
    baseSRPInfo = {
      group: group1024
    , k: k
    , hash: SHA256
    , kdf: ConcatKDF
    }

    withOfflineProxy = merge { proxy: OfflineProxy (BackendSessionState { b: Nothing, aa: Nothing, bb: Nothing }) } baseState
    withOnlineProxy url = merge { proxy: (OnlineProxy url) } baseState
    baseState = { currentChallenge: Nothing
                , sessionKey: Nothing
                , toll: (Loading Nothing)
                , username: Nothing
                , password: Nothing
                , c: Nothing
                , s: Nothing
                , p: Nothing
                , srpInfo: baseSRPInfo
                , hash: SHA256
                , cardsCache: empty
                , masterKey: Nothing
                , userInfoReferences: Nothing 
                , userPreferences: Nothing
                }

resetState :: Aff Unit
resetState = do
  is <- liftEffect computeInitialState
  liftEffect $ saveAppState is

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
getSRPConfFromState state = { group: state.srpInfo.group, k: state.srpInfo.k, hash: getHashFromState state.srpInfo.hash, kdf: getKDFFromState state.srpInfo.kdf }

isOfflineCopy :: AppState -> ProxyConnectionStatus
isOfflineCopy { proxy } =
  case proxy of
    OfflineProxy _  -> ProxyOffline
    _               -> ProxyOnline
