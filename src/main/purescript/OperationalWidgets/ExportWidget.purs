module OperationalWidgets.ExportWidget
  ( exportWidget
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, a, text, button)
import Concur.React.Props as Props
import Control.Alternative ((<|>))
import Control.Bind (bind)
import Control.Semigroupoid ((<<<))
import Data.Either (Either(..), fromRight)
import Data.Function (($))
import Data.Functor ((<$>), (<$), void)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Semigroup ((<>))
import Data.Unit (Unit)
import DataModel.Index (Index)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Export (prepareOfflineCopy, prepareUnencryptedCopy)
import Functions.JSState (getAppState)
import Functions.Time (getCurrentDateTime, formatDateTimeToDate)

exportWidget :: Index -> Widget HTML Unit
exportWidget index = div [] [
    div [] [ do
      _ <- button [Props.onClick, Props.disabled false] [text "Prepare offline copy"]
      downloadOfflineCopy index (void (button [Props.onClick, Props.disabled true] [text "Prepare offline copy"]))
    ]
  , div [] [ do
      _ <- button [Props.onClick, Props.disabled false] [text "Prepare unencrypted data"]
      downloadUnencryptedCopy index (void (button [Props.onClick, Props.disabled true] [text "Prepare unencrypted data"]))
    ]
  ]

downloadOfflineCopy :: Index -> Widget HTML Unit -> Widget HTML Unit
downloadOfflineCopy index placeholder = do
  eitherUrl <- (Left "" <$ placeholder) <|> (liftAff $ prepareOfflineCopy index)
  case eitherUrl of
    Right url -> do
      dt <- liftEffect $ formatDateTimeToDate <$> getCurrentDateTime
      a [Props.download (dt <> "_Clipperz_Offline"), Props.href url, void Props.onClick] [button [Props.disabled false] [text "Download"]] -- TODO: add date
    Left err -> text $ "Could not prepare offline copy: " <> err

downloadUnencryptedCopy :: Index -> Widget HTML Unit -> Widget HTML Unit
downloadUnencryptedCopy index placeholder = do
  eitherUrl <- (Left "" <$ placeholder) <|> (liftAff $ prepareUnencryptedCopy index)
  case eitherUrl of
    Right url -> do
      dt <- liftEffect $ formatDateTimeToDate <$> getCurrentDateTime
      username <- liftEffect $ ((fromMaybe "") <<< (fromRight Nothing) <<< ((<$>) (\as -> as.username))) <$> getAppState
      a [Props.download (dt <> "_Clipperz_Export_" <> username), Props.href url, void Props.onClick] [button [Props.disabled false] [text "Download"]] -- TODO: add date
    Left err -> text $ "Could not prepare offline copy: " <> err
