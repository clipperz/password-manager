module PasswordGeneratorMain
  ( main
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (p, text)
import Concur.React.Props as Props
import Concur.React.Run (runWidgetInDom)
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Data.Function (($))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import DataModel.AsyncValue (AsyncValue(..))
import DataModel.Password (PasswordGeneratorSettings, standardPasswordGeneratorSettings)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Functions.EnvironmentalVariables (currentCommit, shareURL)
import Views.PasswordGenerator (passwordGenerator)
import Web.HTML (window)
import Web.HTML.Location (assign)
import Web.HTML.Window (location)

wrapperWidget :: PasswordGeneratorSettings -> AsyncValue String -> Widget HTML Unit
wrapperWidget settings initialPassword = do
  version <- liftEffect currentCommit
  Tuple secret newSettings <- passwordGenerator settings initialPassword <> p [Props.className "version"] [text version]
  shareUrl <- liftEffect $ shareURL
  pure $ unsafePerformEffect (window >>= location >>= assign (shareUrl <> secret))
  wrapperWidget (fromMaybe standardPasswordGeneratorSettings newSettings) (Done secret)

main :: Effect Unit
main = runWidgetInDom "passwordGenerator" (wrapperWidget standardPasswordGeneratorSettings (Loading Nothing))