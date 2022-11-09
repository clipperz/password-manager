module Functions.Time where

import Control.Semigroupoid ((<<<))
import Data.DateTime (DateTime, date, year, month, day)
import Data.DateTime.Instant (unInstant, toDateTime)
import Data.Enum (fromEnum)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (ceil)
import Data.Newtype (unwrap)
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String (drop, length)
import Effect (Effect)
import Effect.Now (now)

getCurrentTimestamp :: Effect Int
getCurrentTimestamp = (ceil <<< unwrap <<< unInstant) <$> now

getCurrentDateTime :: Effect DateTime
getCurrentDateTime = toDateTime <$> now

formatDateTime :: DateTime -> String
formatDateTime dt =
  let d = date dt
      y = show $ fromEnum $ year d
      m = "00" <> (show $ fromEnum $ month d)
      dy = "00" <> (show $ fromEnum $ day d)
  in y <> (drop ((length m) - 2) m) <> (drop ((length dy) - 2) dy)
