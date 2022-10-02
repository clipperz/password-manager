module OperationalWidgets.CardWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$))
import Data.Semigroup ((<>))
import Data.Show (show, class Show)
import DataModel.Card (Card)
import DataModel.Index (CardReference)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Functions.Communication.Cards (getCard)
import Views.CardViews (cardView, CardAction(..))
import Views.SimpleWebComponents (loadingDiv)

data IndexUpdateAction = AddReference Card | DeleteReference Card | ChangeToReference Card Card | NoUpdate
instance showIndexUpdateAction :: Show IndexUpdateAction where
  show (AddReference c) = "Add reference to " <> show c
  show (DeleteReference c) = "Delete reference to " <> show c
  show (ChangeToReference c c') = "Change reference of " <> show c <> " to " <> show c'
  show NoUpdate = "No update"

cardWidget :: CardReference -> Widget HTML IndexUpdateAction
cardWidget reference = go Loading
  where 
    go state = do
      eitherCard <- case state of 
        Default -> div [] []
        Loading -> loadingDiv <|> (liftAff $ runExceptT $ getCard reference)
        Error err -> div [] [text $ "Card could't be loaded: " <> err]
      case eitherCard of
        Right c -> do 
          res <- cardView c
          case res of
            Edit cc -> pure $ ChangeToReference cc cc
            Clone cc -> pure $ AddReference cc
            Archive cc -> pure $ ChangeToReference cc cc
            Delete cc -> pure $ DeleteReference cc
        Left err -> do
          -- TODO: check error to decide what to do
          NoUpdate <$ div [] [text $ show err] 
