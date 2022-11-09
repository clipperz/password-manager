module OperationalWidgets.ExportWidget
  ( exportWidget
  )
  where

import Affjax.ResponseFormat as RF
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, a, text, button)
import Concur.React.Props as Props
import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except.Trans (runExceptT, ExceptT(..), mapExceptT, except)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>), (<$), void)
import Data.Semigroup ((<>))
import Data.Unit (Unit, unit)
import DataModel.Index (Index(..))
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Functions.Export (prepareOfflineCopy)

exportWidget :: Index -> Widget HTML Unit
exportWidget index = div [] [
    div [] [ do
      _ <- button [Props.onClick, Props.disabled false] [text "Prepare offline copy"]
      downloadOfflineCopy index (void (button [Props.onClick, Props.disabled true] [text "Prepare offline copy"]))
    ]
  , div [] [
      void $ button [Props.onClick, Props.disabled true] [text "Prepare unencrypted data"]
    ]
  ]

downloadOfflineCopy :: Index -> Widget HTML Unit -> Widget HTML Unit
downloadOfflineCopy index placeholder = do
  url <- (Left "" <$ placeholder) <|> (liftAff $ prepareOfflineCopy index)
  case url of
    Right url -> a [Props.download "offline_copy", Props.href url, void Props.onClick] [button [Props.disabled false] [text "Download"]] -- TODO: add date
    Left err -> text $ "Could not prepare offline copy: " <> err
