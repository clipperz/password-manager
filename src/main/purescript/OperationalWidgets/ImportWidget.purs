module OperationalWidgets.ImportWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT, ExceptT(..), except)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List (List(..), (:), concat, fromFoldable)
import Data.Show (show)
import Data.Traversable (sequence)
import DataModel.AppState (AppError)
import DataModel.Card (Card)
import DataModel.Index (Index(..), CardEntry)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Communication.Cards (postCard)
import Functions.Communication.Users (updateIndex)
import Functions.Import (decodeImport, parseHTMLImport, decodeHTML)
import Views.SimpleWebComponents (loadingDiv, simpleFileInputWidget)

importWidget :: Index -> Widget HTML (Either AppError Index)
importWidget index@(Index entries) = do
  result <- runExceptT $ do
    content <- ExceptT $ Right <$> simpleFileInputWidget "import" (text "Import")
    codedCardData <- except $ parseHTMLImport content
    decodedCardData <- except $ Right $ decodeHTML codedCardData
    -- liftEffect $ log decodedCardData
    ExceptT $ liftEffect $ decodeImport decodedCardData
  case result of
    Left err -> do
      liftEffect $ log $ show err
      importWidget index
    Right cards -> loadingDiv <|> (liftAff $ saveImport cards)

  where 
    saveImport :: Array Card -> Aff (Either AppError Index)
    saveImport cards = runExceptT $ do
      newEntries :: Array CardEntry <- sequence (postCard <$> cards)
      let newIndex = Index (concat $ entries : (fromFoldable newEntries) : Nil)
      _ <- updateIndex newIndex
      ExceptT $ pure $ Right newIndex
