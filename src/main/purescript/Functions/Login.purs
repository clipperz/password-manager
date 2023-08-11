module Functions.Login where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), except, withExceptT)
import Data.Either (Either(..))
import Data.Eq ((/=))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (fromArrayBuffer)
import Data.HeytingAlgebra ((&&))
import Data.Maybe (Maybe(..))
import Data.PrettyShow (prettyShow)
import Data.Show (show)
import Data.Unit (Unit, unit)
import DataModel.Credentials (Credentials)
import DataModel.User (UserPreferences(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.Communication.Login (login)
import Functions.Communication.Users (getUserCard, getUserPreferences)
import Functions.JSState (updateAppState)
import Functions.State (getSRPConf)
import Functions.SRP as SRP
import Functions.Timer (activateTimer)

doLogin :: Credentials -> ExceptT String Aff Unit
doLogin { username, password } =
  if username /= "" && password /= "" then do
    conf <- withExceptT (prettyShow) (ExceptT $ liftEffect getSRPConf)
    c    <- ExceptT $ Right <$> fromArrayBuffer <$> SRP.prepareC conf username password
    p    <- ExceptT $ Right <$> fromArrayBuffer <$> SRP.prepareP conf username password

    withExceptT (prettyShow) (ExceptT $ updateAppState { username: Just username, password: Just password, c: Just c, p: Just p })

    _ <- withExceptT (\e -> show e{- "Login failed" -}) login
    uc <- withExceptT (prettyShow)  getUserCard
    up@(UserPreferences userPreferences) <- withExceptT (prettyShow) getUserPreferences

    withExceptT (prettyShow) (ExceptT $ updateAppState { userPreferences: Just up, userCard: Just uc })
    
    case userPreferences.automaticLock of
      Left  _ -> except  $ Right unit
      Right n -> ExceptT $ Right <$> (liftEffect $ activateTimer n)
    
    pure unit
  else except $ Left $ "Empty credentials"
