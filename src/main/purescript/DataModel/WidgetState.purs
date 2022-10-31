module DataModel.WidgetState where

import Data.Semigroup ((<>))
import Data.Show (class Show, show)

data WidgetState = Default | Loading | Error String

instance showWidgetState :: Show WidgetState where
  show Default = "Default"
  show Loading = "Loading"
  show (Error e) = "Error: " <> e
