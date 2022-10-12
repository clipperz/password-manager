module Functions.Time where

import Control.Semigroupoid ((<<<))
import Data.DateTime.Instant (unInstant)
import Data.Functor ((<$>))
import Data.Int (ceil)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Now (now)

getCurrentTimestamp :: Effect Int
getCurrentTimestamp = (ceil <<< unwrap <<< unInstant) <$> now
