module Functions.EnvironmentalVariables where

import Effect (Effect)

foreign import currentCommit :: Effect String

foreign import shareURL :: Effect String

foreign import redeemURL :: Effect String
