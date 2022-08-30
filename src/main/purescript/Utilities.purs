module Utilities where

import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.State (StateT(..))
import Control.Semigroupoid ((<<<))
import Data.ArrayBuffer.Builder (execPut, putArrayBuffer)
import Data.ArrayBuffer.Typed as ABTyped
import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView, Uint8)
import Data.BigInt (BigInt)
import Data.Binary.Base64 as Data.Binary.Base64
import Data.Either (Either)
import Data.Function (flip, ($))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Functor (class Functor, map, (<$>))
import Data.HexString (fromBigInt, toBigInt, fromArrayBuffer, toArrayBuffer)
import Data.List (List(..))
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (Error)

-- | Returns an empty ArrayBuffer of the desired size
foreign import emptyByteArrayBuffer :: Int -> ArrayBuffer

foreign import fn2xorLowBitAligned :: Fn2 ArrayBuffer ArrayBuffer ArrayBuffer

-- | Computes the xor between two ArrayBuffers, alignig them from the lower bit
xor :: ArrayBuffer -> ArrayBuffer -> ArrayBuffer
xor = runFn2 fn2xorLowBitAligned

base64Encoding :: ArrayBuffer -> Effect String
base64Encoding b = do
  v :: ArrayView Uint8 <- ABTyped.whole b
  let result = Data.Binary.Base64.encode v :: String
  pure result

base64Decoding :: String -> Either Error ArrayBuffer
base64Decoding s = map ABTyped.buffer $ Data.Binary.Base64.decode s

concatArrayBuffers :: List ArrayBuffer -> Effect ArrayBuffer
concatArrayBuffers (Cons ab Nil) = pure ab
concatArrayBuffers list = go (emptyByteArrayBuffer 0) list
  where 
    go :: ArrayBuffer -> List ArrayBuffer -> Effect ArrayBuffer
    go res Nil = pure $ res
    go res (Cons ab l) = do
      concatTwoAB res ab >>= flip go l
    concatTwoAB :: ArrayBuffer -> ArrayBuffer -> Effect ArrayBuffer
    concatTwoAB ab1 ab2 = execPut $ do
      putArrayBuffer ab1
      putArrayBuffer ab2

fromArrayBufferToArrayView :: ArrayBuffer -> Effect (ArrayView Uint8)
fromArrayBufferToArrayView = ABTyped.whole

bigIntToArrayBuffer :: BigInt -> ArrayBuffer
bigIntToArrayBuffer = toArrayBuffer <<< fromBigInt

arrayBufferToBigInt :: ArrayBuffer -> Maybe BigInt
arrayBufferToBigInt = toBigInt <<< fromArrayBuffer

makeStateT :: forall m a s. Functor m => m a -> StateT s m a
makeStateT value = StateT (\s -> ((\r -> Tuple r s) <$> value))
