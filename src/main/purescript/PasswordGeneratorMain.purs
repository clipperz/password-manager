module PasswordGeneratorMain
  ( main
  )
  where

import Concur.React.Run (runWidgetInDom)
import Data.Unit (Unit)
import DataModel.Password (standardPasswordGeneratorSettings)
import Effect (Effect)
import Views.PasswordGenerator (passwordGenerator)

main :: Effect Unit
main = runWidgetInDom "passwordGenerator" (passwordGenerator standardPasswordGeneratorSettings)
