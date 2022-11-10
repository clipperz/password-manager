module OperationalWidgets.HomePageWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except.Trans (runExceptT)
import Control.Semigroupoid ((<<<))
import Data.Either (Either(..), hush)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.PrettyShow (prettyShow)
import Data.Show (show)
import Data.Unit (unit)
import DataModel.Index (Index)
import DataModel.WidgetState (WidgetState(..))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Communication.Users (getIndex)
import Functions.JSState (getAppState)
import Functions.State (resetState, isOfflineCopy)
import Views.CardsManagerView (CardView(..))
import Views.SimpleWebComponents (simpleButton, loadingDiv)
import OperationalWidgets.CardsManagerWidget (cardsManagerWidget)
import OperationalWidgets.UserAreaWidget (userAreaWidget, UserAreaAction(..))

data HomePageAction = UserAreaAction UserAreaAction | LogoutAction

data HomePageExitStatus = Clean | ReadyForLogin String

homePageWidget :: Widget HTML HomePageExitStatus
homePageWidget = do
  eitherState <- liftEffect $ getAppState
  case eitherState of
    Left err -> go (Error (show err)) true
    Right st -> go Loading (isOfflineCopy st)

  where 
    go :: WidgetState -> Boolean -> Widget HTML HomePageExitStatus
    go widgetState isOffline = do
      res <- case widgetState of
        Default -> div [] []
        Loading -> loadingDiv <|> ((UserAreaAction <<< Loaded) <$> (liftAff $ runExceptT $ getIndex))
        Error err -> div [] [text err, simpleButton "Go back to login" false LogoutAction]
      interpretHomePageActions isOffline Nothing res 
    
    homePage :: Boolean -> Index -> CardView -> Widget HTML HomePageExitStatus
    homePage isOffline index cardView = do
      result <- div [Props._id "homePage"] [
                  cardsManagerWidget isOffline index { cardView: cardView, cardViewState: Default }
                , do
                    simpleButton "Open user area" false unit
                    UserAreaAction <$> (userAreaWidget isOffline)
                ]
      interpretHomePageActions isOffline (Just cardView) result

    interpretHomePageActions :: Boolean -> Maybe CardView -> HomePageAction -> Widget HTML HomePageExitStatus
    interpretHomePageActions isOffline cv result =
      case result of
        UserAreaAction (Loaded (Right index)) -> homePage isOffline index NoCard         
        UserAreaAction (NoAction index) -> 
          homePage isOffline index (fromMaybe NoCard cv)
        UserAreaAction (GetIndexError err) -> 
          go (Error (show err)) isOffline
        UserAreaAction (Loaded (Left err)) -> do
          _ <- liftEffect $ log $ show err
          go (Error (prettyShow err)) isOffline
        UserAreaAction Lock -> do
          maybeUser <- liftEffect $ getUsername
          pure $ maybe Clean ReadyForLogin maybeUser
        UserAreaAction Logout -> pure $ Clean
        UserAreaAction DeleteAccount -> do
          _ <- liftAff $ runExceptT $ resetState
          pure $ Clean
        LogoutAction -> do
          _ <- liftAff $ runExceptT $ resetState
          pure $ Clean

    getUsername :: Effect (Maybe String)
    getUsername = (\as -> as >>= (\a -> a.username)) <$> (hush <$> getAppState)    
