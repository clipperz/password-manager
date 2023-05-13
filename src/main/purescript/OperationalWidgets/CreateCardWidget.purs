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
import Data.Argonaut.Decode (fromJsonString)
import Data.Either (Either(..), either)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not, (&&), (||))
import Data.Maybe (Maybe(..), maybe)
import Data.PrettyShow (prettyShow)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppError)
import DataModel.Card (Card, emptyCard)
import DataModel.Index (CardEntry)
import DataModel.WidgetOperations (IndexUpdateAction(..), IndexUpdateData(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Clipboard (getClipboardContent)
import Functions.Communication.Cards (postCard)
import Views.CreateCardView (createCardView)

data CreateCardActions = JustCard Card | NewEntry CardEntry | FailedCreation AppError Card | NoAction

createCardWidget :: Maybe Card -> Array String -> WidgetState -> Widget HTML IndexUpdateData
createCardWidget maybeCard tags state = do
  Tuple startingCard isNew <- case maybeCard of
    Just card -> pure (Tuple card false)
    Nothing   -> do
      clipboard <- liftAff $ getClipboardContent
      case fromJsonString clipboard of
        Right card -> pure (Tuple card true)
        Left  _    -> pure (Tuple emptyCard true)
  res <- case state of
    Default -> (maybe NoAction JustCard) <$> (createCardView startingCard tags isNew Default)
    Loading -> ((maybe NoAction JustCard) <$> (createCardView startingCard tags isNew Loading)) 
                <|> ((either (\err -> FailedCreation err startingCard) NewEntry) <$> (liftAff $ runExceptT $ postCard startingCard)) -- TODO: draw loadingDiv over form
    Error err -> (maybe NoAction JustCard) <$> (createCardView startingCard tags isNew (Error ("Card could't be saved: " <> err)))
  case res of
    NoAction -> pure $ IndexUpdateData NoUpdate (if isNew then Nothing else (Just startingCard))
    JustCard card -> do
      if ((isNew && card == emptyCard) || (not isNew && card == startingCard)) then pure $ IndexUpdateData NoUpdate (Just startingCard) 
      else createCardWidget (Just card) tags Loading
    NewEntry e -> pure $ IndexUpdateData (AddReference e) (Just startingCard)
    FailedCreation err card -> do
      _ <- liftEffect $ log $ show err
      createCardWidget (Just card) tags (Error (prettyShow err))
