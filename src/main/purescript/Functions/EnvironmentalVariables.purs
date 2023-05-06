module Functions.EnvironmentalVariables where

import Effect (Effect)

foreign import currentCommit :: Effect String
