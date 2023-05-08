module PasswordGeneratorMain
  ( main
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.Run (runWidgetInDom)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Data.Function (($))
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import DataModel.Password (PasswordGeneratorSettings, standardPasswordGeneratorSettings)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Functions.EnvironmentalVariables (shareURL)
import Views.PasswordGenerator (passwordGenerator)
import Web.HTML (window)
import Web.HTML.Location (assign)
import Web.HTML.Window (location)

wrapperWidget :: PasswordGeneratorSettings -> Widget HTML Unit
wrapperWidget settings = do
  Tuple secret newSettings <- passwordGenerator settings
  shareUrl <- liftEffect $ shareURL
  pure $ unsafePerformEffect (assign (shareUrl <> secret) (unsafePerformEffect (location (unsafePerformEffect window))))
  wrapperWidget (fromMaybe standardPasswordGeneratorSettings newSettings)

main :: Effect Unit
main = runWidgetInDom "passwordGenerator" (wrapperWidget standardPasswordGeneratorSettings)