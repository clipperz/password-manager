module Functions.Clipboard
  ( copyToClipboard
  , getClipboardContent
  )
  where

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import copyToClipboard :: forall a. String -> a

foreign import _getClipboardContent :: Effect (Promise String)

getClipboardContent :: Aff String
getClipboardContent = toAffE _getClipboardContent