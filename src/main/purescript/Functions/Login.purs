module Functions.Login where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), throwError)
import Data.Either (Either(..))
import Data.Eq ((/=))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, fromArrayBuffer)
import Data.HeytingAlgebra ((&&))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Unit (unit)
import DataModel.AppState (AppError(..))
import DataModel.Credentials (Credentials)
import DataModel.SRP (HashFunction, SRPConf)
import DataModel.StatelessAppState (Proxy(..), ProxyResponse(..))
import DataModel.User (MasterKey, UserInfoReferences, UserPreferences)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.Login (login)
import Functions.Communication.Users (getStatelessMasterKey, getStatelessUserPreferences)
import Functions.JSState (updateAppState)
import Functions.SRP as SRP
import Functions.Timer (activateTimer)

type LoginStateUpdate = {
  proxy              :: Proxy
, userInfoReferences :: Maybe UserInfoReferences
, userPreferences    :: Maybe UserPreferences
, masterKey          :: Maybe MasterKey
, username           :: Maybe String
, password           :: Maybe String
, s                  :: Maybe HexString
, c                  :: Maybe HexString
, p                  :: Maybe HexString
}

doLogin :: Proxy -> HashFunction -> SRPConf -> Credentials -> ExceptT AppError Aff LoginStateUpdate
doLogin proxy hashFunc srpConf { username, password } =
  if username /= "" && password /= "" then do
 
    c         <- liftAff $ fromArrayBuffer <$> SRP.prepareC srpConf username password
    p         <- liftAff $ fromArrayBuffer <$> SRP.prepareP srpConf username password
    
    {proxy: newProxy, userInfoReferences, s} <- login proxy hashFunc srpConf c p
    
    ProxyResponse newProxy'  masterKey       <- getStatelessMasterKey { proxy: newProxy, hashFunc } c
    ProxyResponse newProxy'' userPreferences <- getStatelessUserPreferences { proxy: newProxy', hashFunc } (unwrap userInfoReferences).preferencesReference
    
    case (unwrap userPreferences).automaticLock of
      Right n -> liftEffect (activateTimer n)
      Left  _ -> pure unit

    -- TODO REMOVE
    ExceptT $ updateAppState { 
      sessionKey:         case newProxy'' of
                            StaticProxy _              -> Nothing
                            OnlineProxy _ _ sessionKey -> sessionKey
    , userInfoReferences: Just userInfoReferences 
    , userPreferences:    Just userPreferences
    , masterKey:          Just masterKey 
    , username:           Just username
    , password:           Just password
    , s:                  Just s
    , c:                  Just c
    , p:                  Just p
    }
    -- -----------

    pure { proxy:              newProxy''
         , userInfoReferences: Just userInfoReferences 
         , userPreferences:    Just userPreferences
         , masterKey:          Just masterKey 
         , username:           Just username
         , password:           Just password
         , s:                  Just s
         , c:                  Just c
         , p:                  Just p
         }

  else throwError $ InvalidOperationError "Empty credentials"
