module OperationalWidgets.HomePageWidget
  ( HomePageAction
  , HomePageExitStatus(..)
  , homePageWidget
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Except.Trans (runExceptT)
import Control.Semigroupoid ((<<<))
import Data.Either (Either(..), hush)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.PrettyShow (prettyShow)
import Data.Show (show)
import DataModel.AppState (UserConnectionStatus(..), ProxyConnectionStatus(..))
import DataModel.FragmentData (FragmentData, getCardToAdd)
import DataModel.Index (Index)
import DataModel.WidgetState (WidgetState(..))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Communication.Users (getIndex)
import Functions.JSState (getAppState)
import Functions.State (resetState, isOfflineCopy)
import OperationalWidgets.CardsManagerWidget (cardsManagerWidget, CardsManagerAction(..))
import OperationalWidgets.UserAreaWidget (userAreaWidget, UserAreaAction(..))
import Views.CardsManagerView (CardView(..))
import Views.SimpleWebComponents (simpleButton, loadingDiv)

data HomePageAction = CardsManagerAction CardsManagerAction | UserAreaAction UserAreaAction | LogoutAction

data HomePageExitStatus = Clean | ReadyForLogin String

homePageWidget :: UserConnectionStatus -> FragmentData -> Widget HTML HomePageExitStatus
homePageWidget status fragment = 
  case status of
    UserLoggedIn -> do
      eitherState <- liftEffect $ getAppState
      case eitherState of
        Left err -> go (Error (show err)) ProxyOffline        true
        Right st -> go Loading            (isOfflineCopy st)  true
    UserAnonymous ->
      go Default ProxyOffline true

  where 
    go :: WidgetState -> ProxyConnectionStatus -> Boolean -> Widget HTML HomePageExitStatus
    go widgetState proxyConnectionStatus hideUserAreaWidget = do
      res <- case widgetState of
        Default -> div [] []
        Loading -> loadingDiv <|> ((UserAreaAction <<< Loaded) <$> (liftAff $ runExceptT $ getIndex))
        Error err -> div [] [text err, simpleButton "back" "Go back to login" false LogoutAction]
      interpretHomePageActions proxyConnectionStatus hideUserAreaWidget Nothing Nothing res 
    
    homePage :: ProxyConnectionStatus -> Boolean -> Index -> CardView -> Widget HTML HomePageExitStatus
    homePage proxyConnectionStatus hideUserAreaWidget index cardView = do
      result :: HomePageAction <- div [Props._id "homePage"] [
                  CardsManagerAction <$> cardsManagerWidget proxyConnectionStatus index (getCardToAdd fragment) { cardView: cardView, cardViewState: Default }
                , UserAreaAction <$> (userAreaWidget hideUserAreaWidget proxyConnectionStatus)
                ]
      interpretHomePageActions proxyConnectionStatus hideUserAreaWidget (Just index) (Just cardView) result

    interpretHomePageActions :: ProxyConnectionStatus -> Boolean -> Maybe Index -> Maybe CardView -> HomePageAction -> Widget HTML HomePageExitStatus
    interpretHomePageActions proxyConnectionStatus hideUserAreaWidget ix cv result =
      case result of
        UserAreaAction (Loaded (Right index)) -> homePage proxyConnectionStatus hideUserAreaWidget index NoCard         
        UserAreaAction NoAction -> case ix of
          Just ix' -> homePage proxyConnectionStatus hideUserAreaWidget ix' (fromMaybe NoCard cv)
          Nothing -> go (Error "No index found") proxyConnectionStatus hideUserAreaWidget
        UserAreaAction (GetIndexError err) -> 
          go (Error (show err)) proxyConnectionStatus hideUserAreaWidget
        UserAreaAction (Loaded (Left err)) -> do
          _ <- liftEffect $ log $ show err
          go (Error (prettyShow err)) proxyConnectionStatus hideUserAreaWidget
        UserAreaAction Lock -> do
          maybeUser <- liftEffect $ getUsername
          pure $ maybe Clean ReadyForLogin maybeUser
        UserAreaAction Logout -> pure $ Clean
        UserAreaAction DeleteAccount -> do
          _ <- liftAff resetState
          pure $ Clean
        CardsManagerAction OpenUserArea -> case ix of
          Just ix'  -> homePage proxyConnectionStatus false ix' (fromMaybe NoCard cv)
          Nothing   -> go (Error "No index found") proxyConnectionStatus true
        LogoutAction -> do
          _ <- liftAff resetState
          pure $ Clean

    getUsername :: Effect (Maybe String)
    getUsername = (\as -> as >>= (\a -> a.username)) <$> (hush <$> getAppState)    
