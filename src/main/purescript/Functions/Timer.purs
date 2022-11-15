module Functions.Timer where

import Data.Unit (Unit)
import Effect (Effect)

foreign import activateTimer :: Int -> Effect Unit

foreign import stopTimer :: Effect Unit
