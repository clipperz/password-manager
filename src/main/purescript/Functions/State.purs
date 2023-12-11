module Functions.State where

import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array (filter, catMaybes, head)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString)
import Data.Map (Map)
import Data.Map.Internal (empty)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import DataModel.AppState (AppError, AppState, HashState(..), KDFState(..), ProxyConnectionStatus(..))
import DataModel.AsyncValue (AsyncValue(..))
import DataModel.Card (Card)
import DataModel.Index (Index)
import DataModel.ProxyType (ProxyType(..))
import DataModel.SRP (HashFunction, KDF, SRPConf, concatKDF, group1024, hashFuncSHA1, hashFuncSHA256, k)
import DataModel.StatelessAppState (StatelessAppState)
import DataModel.StatelessAppState as Stateless
import DataModel.User (MasterKey, UserInfoReferences, UserPreferences)
import Effect (Effect)
import Effect.Class (liftEffect)
import Functions.JSState (getAppState)
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

computeInitialStatelessState :: Effect StatelessAppState
computeInitialStatelessState = do
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
  
    withOfflineProxy    = merge { proxy: Stateless.StaticProxy Nothing } baseState
    withOnlineProxy url = merge { proxy: (Stateless.OnlineProxy url {toll: Loading Nothing, currentChallenge: Nothing} Nothing) } baseState

resetState :: StatelessAppState -> StatelessAppState
resetState state = merge baseState state

baseState ∷ { username :: Maybe String
            , password :: Maybe String
            , pinEncryptedPassword :: Maybe HexString
            , c :: Maybe HexString
            , p :: Maybe HexString
            , s :: Maybe HexString
            , srpConf :: SRPConf
            , hash :: HashFunction
            , cardsCache :: Map HexString Card
            , masterKey :: Maybe MasterKey
            , userInfoReferences :: Maybe UserInfoReferences
            , userPreferences :: Maybe UserPreferences
            , index :: Maybe Index
            }
baseState = { username: Nothing
            , password: Nothing
            , pinEncryptedPassword: Nothing
            , c: Nothing
            , s: Nothing
            , p: Nothing
            , srpConf: baseSRPConf
            , hash: hashFuncSHA256
            , cardsCache: empty
            , masterKey: Nothing
            , userInfoReferences: Nothing 
            , userPreferences: Nothing
            , index: Nothing
            }
  where
    baseSRPConf :: SRPConf
    baseSRPConf = {
      group: group1024
    , k: k
    , hash: hashFuncSHA256
    , kdf: concatKDF
    }


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
