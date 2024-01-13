module Functions.Clipboard
  ( copyToClipboard
  , getClipboardContent
  )
  where

import Control.Alt ((<#>))
import Control.Bind (bind, discard, pure, (=<<))
import Data.Either (hush)
import Data.Function (($))
import Data.Maybe (Maybe)
import Data.Show (show)
import Data.Unit (Unit)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Promise.Aff (toAffE)
import Web.Clipboard (clipboard, readText, writeText)
import Web.HTML (window)
import Web.HTML.Window (navigator)

copyToClipboard :: String -> Aff Unit
copyToClipboard string = toAffE $ writeText string =<< clipboard =<< navigator =<< window

getClipboardContent :: Aff (Maybe String)
getClipboardContent = do
  val <- attempt (toAffE $ readText =<< clipboard =<< navigator =<< window) <#> hush
  liftEffect $ log $ show val
  pure val