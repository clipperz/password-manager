module Functions.Time where

import Control.Semigroupoid ((<<<))
import Data.DateTime (DateTime, date, year, month, day, time)
import Data.DateTime.Instant (unInstant, toDateTime)
import Data.Time (hour, minute)
import Data.Enum (fromEnum)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Newtype (unwrap)
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String (drop, length)
import Effect (Effect)
import Effect.Now (now)

getCurrentTimestamp :: Effect Number
getCurrentTimestamp = (unwrap <<< unInstant) <$> now

getCurrentDateTime :: Effect DateTime
getCurrentDateTime = toDateTime <$> now

formatDateTimeToDate :: DateTime -> String
formatDateTimeToDate dt =
  let d = date dt
      y = show $ fromEnum $ year d
      m = "00" <> (show $ fromEnum $ month d)
      dy = "00" <> (show $ fromEnum $ day d)
  in y <> (drop ((length m) - 2) m) <> (drop ((length dy) - 2) dy)

formatDateTimeToTime :: DateTime -> String
formatDateTimeToTime dt =
  let t = time dt
      h = "00" <> (show $ fromEnum $ hour t)
      m = "00" <> (show $ fromEnum $ minute t)
  in (drop ((length h) - 2) h) <> ":" <> (drop ((length m) - 2) m)
