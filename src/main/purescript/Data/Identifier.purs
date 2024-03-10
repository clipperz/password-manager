module Data.Identifier where

import Control.Alt ((<$>))
import Data.EuclideanRing ((/))
import Data.HexString (HexString, fromArrayBuffer)
import Effect.Aff (Aff)
import Functions.SRP (randomArrayBuffer)

identifierLength :: Int
identifierLength = 256

type Identifier = HexString

computeIdentifier :: Aff Identifier
computeIdentifier = fromArrayBuffer <$> randomArrayBuffer (256/8)