module Functions.Communication.BlobFromArrayBuffer where

import Data.ArrayBuffer.Types (ArrayBuffer)
import Web.File.Blob (Blob)

foreign import blobFromArrayBuffer :: ArrayBuffer -> Blob