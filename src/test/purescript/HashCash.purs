module Test.HashCash where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, hex, fromArrayBuffer, toArrayBuffer)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.Unit (Unit)
import DataModel.SRP (hashFuncSHA256)
import Effect.Aff (Aff, invincible)
import Effect.Class.Console (log)
import Functions.HashCash (verifyReceipt, computeReceipt)
import Test.Spec (describe, it, SpecT)
import Test.Spec.Assertions (shouldEqual)
import Test.QuickCheck ((===), Result(..))
import TestUtilities (makeTestableOnBrowser, quickCheckAffInBrowser)

hashCashSpec :: SpecT Aff Unit Identity Unit
hashCashSpec =
  describe "HashCash" do
    let testReceipt = "test receipt"
    it testReceipt do
      quickCheckAffInBrowser testReceipt 2 testReceiptProp
    let computeReceiptTest = "compute receipt"
    it computeReceiptTest $ invincible $ do
      quickCheckAffInBrowser computeReceiptTest 2 computeReceiptProp

computeReceiptProp :: HexString -> Aff Result
computeReceiptProp toll = do
  let cost = 2
  receipt <- computeReceipt hashFuncSHA256 { toll, cost }
  verification <- verifyReceipt hashFuncSHA256 { toll, cost } receipt
  pure $ verification === true

testReceiptProp :: HexString -> Aff Result
testReceiptProp hexString = do
  challenge    <- fromArrayBuffer <$> (hashFuncSHA256 $ (toArrayBuffer hexString) : Nil)
  verification <- verifyReceipt hashFuncSHA256 { toll: challenge, cost: 2 } hexString
  pure $ verification === true
