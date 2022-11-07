module OperationalWidgets.HomePageWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT)
import Control.Semigroupoid ((<<<))
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), maybe)
import Data.PrettyShow (prettyShow)
import Data.Show (show)
import Data.Unit (Unit, unit)
import DataModel.Index (Index)
import DataModel.WidgetState (WidgetState(..))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Communication.Users (getIndex)
import Functions.JSState (getAppState)
import Functions.State (resetState)
import Views.CardsManagerView (CardView(..))
import Views.SimpleWebComponents (simpleButton, loadingDiv)
import OperationalWidgets.CardsManagerWidget (cardsManagerWidget)
import OperationalWidgets.UserAreaWidget (userAreaWidget, UserAreaAction(..))

data HomePageAction = UserAreaAction UserAreaAction | LogoutAction

data HomePageExitStatus = Clean | ReadyForLogin String

homePageWidget :: Widget HTML HomePageExitStatus
homePageWidget = go Loading
  where 
    go widgetState = do
      res <- case widgetState of
        Default -> div [] []
        Loading -> loadingDiv <|> ((UserAreaAction <<< Loaded) <$> (liftAff $ runExceptT $ getIndex))
        Error err -> div [] [text err, simpleButton "Go back to login" false LogoutAction]
      interpretHomePageActions Nothing Nothing res
    
    homePage :: Index -> CardView -> Widget HTML HomePageExitStatus
    homePage index cardView = do
      result <- div [Props._id "homePage"] [
                  cardsManagerWidget index { cardView: cardView, cardViewState: Default }
                , do
                    simpleButton "Open user area" false unit
                    UserAreaAction <$> userAreaWidget index
                ]
      interpretHomePageActions (Just index) (Just cardView) result

    interpretHomePageActions :: Maybe Index -> Maybe CardView -> HomePageAction -> Widget HTML HomePageExitStatus
    interpretHomePageActions ix cv result =
      case result of
        UserAreaAction (Loaded (Right index)) -> homePage index NoCard         
        UserAreaAction NoAction -> 
          case ix, cv of
            (Just ix), (Just cv) -> homePage ix cv
            _, _ -> go (Error "Inconsistent state")
        UserAreaAction (Loaded (Left err)) -> do
          _ <- liftEffect $ log $ show err
          go (Error (prettyShow err))
        UserAreaAction Lock -> do
          maybeUser <- liftEffect $ getUsername
          liftAff $ resetState
          pure $ maybe Clean ReadyForLogin maybeUser
        UserAreaAction Logout -> do
          liftAff $ resetState
          pure $ Clean
        UserAreaAction DeleteAccount -> do
          liftAff $ resetState
          pure $ Clean
        LogoutAction -> do
          liftAff $ resetState
          pure $ Clean

    getUsername :: Effect (Maybe String)
    getUsername = do
      state <- liftEffect $ getAppState
      case state of
        Left err -> pure $ Nothing
        Right r@{username} -> pure $ username      
