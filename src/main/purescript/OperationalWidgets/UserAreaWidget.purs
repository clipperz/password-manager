module OperationalWidgets.UserAreaWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT, ExceptT(..), except)
import Data.Either (Either(..), note)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List (List(..), (:), concat, fromFoldable)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Traversable (sequence)
import DataModel.AppState (AppError(..))
import DataModel.Card (Card)
import DataModel.Index (Index(..), CardEntry)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Communication.Cards (postCard, updateIndex)
import Functions.Import (decodeImport, parseHTMLImport, decodeHTML)
import Views.SimpleWebComponents (textAreaWidget, loadingDiv, simpleFileInputWidget)
import Web.File.FileReader (fromEventTarget, FileReader)

userAreaWidget :: Index -> Widget HTML (Either AppError Index)
userAreaWidget index@(Index_v1 entries) = do
  result <- runExceptT $ do
    -- s <- textAreaWidget "" "Import data"
    -- reader <- ExceptT $ (note (ImportError "Could not load file")) <$> (simpleFileInputWidget "import" (text "Import"))
    content <- ExceptT $ Right <$> simpleFileInputWidget "import" (text "Import")
    codedCardData <- except $ parseHTMLImport content
    decodedCardData <- except $ Right $ decodeHTML codedCardData
    liftEffect $ log decodedCardData
    ExceptT $ liftEffect $ decodeImport decodedCardData
  case result of
    Left err -> do
      liftEffect $ log $ show err
      userAreaWidget index
    Right cards -> loadingDiv <|> (liftAff $ saveImport cards)

  where 
    saveImport :: Array Card -> Aff (Either AppError Index)
    saveImport cards = runExceptT $ do
      newEntries :: Array CardEntry <- sequence (postCard <$> cards)
      let newIndex = Index_v1 (concat $ entries : (fromFoldable newEntries) : Nil)
      _ <- updateIndex newIndex
      ExceptT $ pure $ Right newIndex
