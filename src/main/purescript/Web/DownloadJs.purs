module Web.DownloadJs
  ( download
  , class Downloadable
  , toDownloadData
  , Filename
  , MimeType
  ) where

import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView, DataView)
import Effect (Effect)
import Foreign (Foreign)
import Foreign as F
import Web.File.Blob (Blob)

-- | A class to represent types which can be downloaded using
-- | this library.
-- |
-- | You can extend it, adding an instance for your
-- | type and casting it to either String (possibly, with data URL),
-- | Blob, ArrayBuffer or a typed array. However, you'll have to use
-- | `unsafeToForeign` to finally cast your value. Hope we'll fix that.
class Downloadable a where
  toDownloadData :: a -> Foreign

instance dlString :: Downloadable String where
  toDownloadData = F.unsafeToForeign

instance dlBlob :: Downloadable Blob where
  toDownloadData = F.unsafeToForeign

instance dlArrayBuffer :: Downloadable ArrayBuffer where
  toDownloadData = F.unsafeToForeign

instance dlDataView :: Downloadable DataView where
  toDownloadData = F.unsafeToForeign

-- UintNArray, IntNArray, etc
instance dlArrayView :: Downloadable (ArrayView a) where
  toDownloadData = F.unsafeToForeign

type Filename = String
type MimeType = String

-- | Sets provided payload to be downloaded by browser, using
-- | the particular filename and MIME type.
-- |
-- | Example: `download "hello world" "hello.txt" "text/plain"`
download :: ∀ a. (Downloadable a)
         => a
         -> Filename
         -> MimeType
         -> Effect Boolean
download payload = download_ (toDownloadData payload)

foreign import download_ :: Foreign -> String -> String -> Effect Boolean