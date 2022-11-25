module OperationalWidgets.ExportWidget
  ( exportWidget
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, a, text, h1, h3, p, button)
import Concur.React.Props as Props
import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (runExceptT)
import Control.Semigroupoid ((<<<))
import Data.Either (Either(..), fromRight)
import Data.Function (($))
import Data.Functor ((<$>), (<$), void)
import Data.List (List(..))
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import DataModel.Index (Index(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.Users (getIndex)
import Functions.Export (prepareOfflineCopy, prepareUnencryptedCopy)
import Functions.JSState (getAppState)
import Functions.Time (getCurrentDateTime, formatDateTimeToDate)

exportWidget :: Widget HTML Unit
exportWidget = do
  newIndex <- ((Right (Index Nil)) <$ exportView (Tuple (Right Nothing) (Right Nothing))) <|> (liftAff $ runExceptT $ getIndex)
  case newIndex of
    Right index -> go index (Tuple (Right Nothing) (Right Nothing))
    Left err -> exportView  (Tuple (Left (show err)) (Left (show err)))

  where
    go _ tuple@(Tuple (Right (Just _)) (Right (Just _))) = exportView tuple
    go index tuple = do
      res <- (Nothing <$ exportView tuple) <|> (Just <$> liftAff (prepareDownloads index))
      go index $ fromMaybe tuple res

    exportView :: Tuple (Either String (Maybe String)) (Either String (Maybe String)) -> Widget HTML Unit
    exportView (Tuple offline unencrypted) = div [Props._id "exportPage"] [
      h1 [] [text "Export"]
    , div [] [ 
        h3 [] [text "Offline copy"]
      , p [] [text "Download a read-only portable version of Clipperz. Very convenient when no Internet connection is available."]
      , p [] [text "An offline copy is just a single HTML file that contains both the whole Clipperz web application and your encrypted data."]
      , p [] [text "It is as secure as the hosted Clipperz service since they both share the same code and security architecture."]
      , case offline of
          Left err -> (text $ "Could not prepare offline copy: " <> err) <|> (a [] [button [Props.disabled true] [text "Download"]]) 
          Right Nothing -> a [] [button [Props.disabled true] [text "Download"]] 
          Right (Just url) -> do
            dt <- liftEffect $ formatDateTimeToDate <$> getCurrentDateTime
            a [Props.download (dt <> "_Clipperz_Offline"), Props.href url, void Props.onClick] [button [Props.disabled false] [text "Download"]] 
      ]
    , div [] [ 
        h3 [] [text "HTML + JSON"]
      , p [] [text "Download a printer-friendly HTML file that lists the content of all your cards."]
      , p [] [text "This same file also contains all your data in JSON format. Please note that file attachments are not included."]
      , p [Props.className "important"] [text "Beware: all data are unencrypted! Therefore make sure to properly store and manage this file."]
      , case unencrypted of
          Left err -> (text $ "Could not prepare unencrypted copy: " <> err) <|> (a [] [button [Props.disabled true] [text "Download"]]) 
          Right Nothing -> a [] [button [Props.disabled true] [text "Download"]]
          Right (Just url) -> do
            dt <- liftEffect $ formatDateTimeToDate <$> getCurrentDateTime
            username <- liftEffect $ ((fromMaybe "") <<< (fromRight Nothing) <<< ((<$>) (\as -> as.username))) <$> getAppState
            a [Props.download (dt <> "_Clipperz_Export_" <> username), Props.href url, void Props.onClick] [button [Props.disabled false] [text "Download"]]
      ]
    ]

    prepareDownloads :: Index -> Aff (Tuple (Either String (Maybe String)) (Either String (Maybe String)))
    prepareDownloads index = do
      eitherOffline <- ((<$>) Just) <$> prepareOfflineCopy index
      eitherUc <- ((<$>) Just) <$> prepareUnencryptedCopy index
      pure $ Tuple eitherOffline eitherUc
