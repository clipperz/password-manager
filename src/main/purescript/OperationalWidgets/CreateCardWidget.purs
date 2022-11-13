module OperationalWidgets.CreateCardWidget
  ( createCardWidget
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (either)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (maybe)
import Data.PrettyShow (prettyShow)
import Data.Semigroup ((<>))
import Data.Show (show)
import DataModel.AppState (AppError)
import DataModel.Card (Card)
import DataModel.Index (CardEntry)
import DataModel.WidgetOperations (IndexUpdateAction(..), IndexUpdateData(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Communication.Cards (postCard)
import Views.CreateCardView (createCardView)

data CreateCardActions = JustCard Card | NewEntry CardEntry | FailedCreation AppError Card | NoAction

createCardWidget :: Card -> Array String -> WidgetState -> Widget HTML IndexUpdateData
createCardWidget startingCard tags state = do
  res <- case state of
    Default -> (maybe NoAction JustCard) <$> (createCardView startingCard tags Default)
    Loading -> ((maybe NoAction JustCard) <$> (createCardView startingCard tags Loading)) 
                <|> ((either (\err -> FailedCreation err startingCard) NewEntry) <$> (liftAff $ runExceptT $ postCard startingCard)) -- TODO: draw loadingDiv over form
    Error err -> (maybe NoAction JustCard) <$> (createCardView startingCard tags (Error ("Card could't be saved: " <> err)))
  case res of
    NoAction -> pure $ IndexUpdateData NoUpdate startingCard
    JustCard card -> do
      if card == startingCard then pure $ IndexUpdateData NoUpdate startingCard 
      else createCardWidget card tags Loading
    NewEntry e -> pure $ IndexUpdateData (AddReference e) startingCard
    FailedCreation err card -> do
      _ <- liftEffect $ log $ show err
      createCardWidget card tags (Error (prettyShow err))
