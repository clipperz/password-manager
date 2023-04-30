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
import Effect.Unsafe (unsafePerformEffect)
import Views.PasswordGenerator (passwordGenerator)
import Web.HTML (window)
import Web.HTML.Location (assign)
import Web.HTML.Window (location)

wrapperWidget :: PasswordGeneratorSettings -> Widget HTML Unit
wrapperWidget settings = do
  Tuple secret newSettings <- passwordGenerator settings
  -- pure $ unsafePerformEffect (assign ("https://clipperz.is/share#" <> secret) (unsafePerformEffect (location (unsafePerformEffect window))))
  pure $ unsafePerformEffect (assign ("http://localhost:8090/share_index.html#share=" <> secret) (unsafePerformEffect (location (unsafePerformEffect window))))
  wrapperWidget (fromMaybe standardPasswordGeneratorSettings newSettings)

main :: Effect Unit
main = runWidgetInDom "passwordGenerator" (wrapperWidget standardPasswordGeneratorSettings)