module Functions.Clipboard where

foreign import copyToClipboard :: forall a. String -> a