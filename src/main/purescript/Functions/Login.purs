module Functions.Login where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), throwError, withExceptT)
import Data.Either (Either(..))
import Data.Eq ((/=))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (fromArrayBuffer)
import Data.HeytingAlgebra ((&&))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.PrettyShow (prettyShow)
import Data.Show (show)
import Data.Unit (Unit, unit)
import DataModel.Credentials (Credentials)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.Login (login)
import Functions.Communication.Users (getMasterKey, getUserPreferences)
import Functions.JSState (updateAppState)
import Functions.SRP as SRP
import Functions.State (getSRPConf)
import Functions.Timer (activateTimer)

doLogin :: Credentials -> ExceptT String Aff Unit
doLogin { username, password } =
  if username /= "" && password /= "" then do
 
    conf      <- withExceptT prettyShow (ExceptT $ liftEffect getSRPConf)
    c         <- liftAff $ fromArrayBuffer <$> SRP.prepareC conf username password
    p         <- liftAff $ fromArrayBuffer <$> SRP.prepareP conf username password
    
    _ <- withExceptT show $ login conf c p
    
    masterKey <- withExceptT prettyShow $ getMasterKey c
    up        <- withExceptT prettyShow   getUserPreferences
    
    withExceptT prettyShow (ExceptT $ updateAppState { 
      userPreferences: Just up
    , masterKey:       Just masterKey 
    , username:        Just username
    , password:        Just password
    , c:               Just c
    , p:               Just p
    })

    case (unwrap up).automaticLock of
      Right n -> liftAff $ liftEffect (activateTimer n)
      Left  _ -> pure unit

  else throwError "Empty credentials"
