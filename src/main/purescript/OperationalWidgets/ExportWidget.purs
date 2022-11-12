module OperationalWidgets.ExportWidget
  ( exportWidget
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, a, text, h1, h3, p, button)
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
exportWidget index = div [Props._id "exportPage"] [
    h1 [] [text "Export"]
  , div [] [ 
      h3 [] [text "Offline copy"]
    , p [] [text "Download a read-only portable version of Clipperz. Very convenient when no Internet connection is available."]
    , p [] [text "An offline copy is just a single HTML file that contains both the whole Clipperz web application and your encrypted data."]
    , p [] [text "It is as secure as the hosted Clipperz service since they both share the same code and security architecture."]
    , do
        _ <- button [Props.onClick, Props.disabled false] [text "Prepare offline copy"]
        downloadOfflineCopy index (void (button [Props.onClick, Props.disabled true] [text "Prepare offline copy"]))
    ]
  , div [] [ 
      h3 [] [text "HTML + JSON"]
    , p [] [text "Download a printer-friendly HTML file that lists the content of all your cards."]
    , p [] [text "This same file also contains all your data in JSON format. Please note that file attachments are not included."]
    , p [Props.className "important"] [text "Beware: all data are unencrypted! Therefore make sure to properly store and manage this file."]
    , do
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
