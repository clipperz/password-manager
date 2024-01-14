module Functions.Clipboard
  ( copyToClipboard
  , getClipboardContent
  )
  where

import Control.Alt ((<#>))
import Control.Bind ((=<<))
import Data.Either (hush)
import Data.Function (($))
import Data.Maybe (Maybe)
import Data.Unit (Unit)
import Effect.Aff (Aff, attempt)
import Promise.Aff (toAffE)
import Web.Clipboard (clipboard, readText, writeText)
import Web.HTML (window)
import Web.HTML.Window (navigator)

copyToClipboard :: String -> Aff Unit
copyToClipboard string = toAffE $ writeText string =<< clipboard =<< navigator =<< window

getClipboardContent :: Aff (Maybe String)
getClipboardContent = attempt (toAffE $ readText =<< clipboard =<< navigator =<< window) <#> hush