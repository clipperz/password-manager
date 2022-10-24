module OperationalWidgets.UserAreaWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT, ExceptT(..))
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
import Effect.Class.Console (log)
import Functions.Communication.Cards (postCard, updateIndex)
import Functions.Import (decodeImport)
import Views.SimpleWebComponents (textAreaWidget, loadingDiv)

userAreaWidget :: Index -> Widget HTML (Either AppError Index)
userAreaWidget index@(Index_v1 entries) = do
  s <- textAreaWidget "" "Import data"
  let result = decodeImport s
  case result of
    Left err -> do
      log $ show err
      userAreaWidget index
    Right cards -> loadingDiv <|> (liftAff $ saveImport cards)

  where 
    saveImport :: Array Card -> Aff (Either AppError Index)
    saveImport cards = runExceptT $ do
      newEntries :: Array CardEntry <- sequence (postCard <$> cards)
      let newIndex = Index_v1 (concat $ entries : (fromFoldable newEntries) : Nil)
      _ <- updateIndex newIndex
      ExceptT $ pure $ Right newIndex