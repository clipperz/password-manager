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
import Data.PrettyShow (prettyShow)
import Data.Show (show)
import Data.Unit (Unit, unit)
import DataModel.Index (Index)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Communication.Users (getIndex)
import Functions.State (resetState)
import Views.CardsManagerView (CardView(..))
import Views.SimpleWebComponents (simpleButton, loadingDiv)
import OperationalWidgets.CardsManagerWidget (cardsManagerWidget)
import OperationalWidgets.UserAreaWidget (userAreaWidget, UserAreaAction(..))

data HomePageAction = UserAreaAction UserAreaAction | LogoutAction

homePageWidget :: Widget HTML Unit
homePageWidget = go Loading
  where 
    go widgetState = do
      res <- case widgetState of
        Default -> div [] []
        Loading -> loadingDiv <|> ((UserAreaAction <<< Loaded) <$> (liftAff $ runExceptT $ getIndex))
        Error err -> div [] [text err, simpleButton "Go back to login" false LogoutAction]
      interpretHomePageActions res
    
    homePage :: Index -> CardView -> Widget HTML Unit
    homePage index cardView = do
      result <- div [Props._id "homePage"] [
                  cardsManagerWidget index { cardView: cardView, cardViewState: Default }
                , UserAreaAction <$> userAreaWidget index
                ]
      interpretHomePageActions result

    interpretHomePageActions :: HomePageAction -> Widget HTML Unit
    interpretHomePageActions result =
      case result of
        UserAreaAction (Loaded (Right index)) -> homePage index NoCard         
        UserAreaAction (Loaded (Left err)) -> do
          _ <- liftEffect $ log $ show err
          go (Error (prettyShow err))
        UserAreaAction (Logout) -> do
          liftAff $ resetState
          pure unit
        UserAreaAction (DeleteAccount) -> do
          liftAff $ resetState
          pure unit
        LogoutAction -> do
          liftAff $ resetState
          pure unit
