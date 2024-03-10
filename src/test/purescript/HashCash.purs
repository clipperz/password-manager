module Test.HashCash where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, hex, fromArrayBuffer, toArrayBuffer)
import Data.HeytingAlgebra (not)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (Unit)
import DataModel.SRPVersions.SRP (hashFuncSHA256)
import Effect.Aff (Aff, invincible)
import Functions.HashCash (verifyReceipt, computeReceipt)
import Test.Spec (describe, it, SpecT)
import Test.QuickCheck (Result, (<?>))
import TestUtilities (quickCheckAffInBrowser)

hashCashSpec :: SpecT Aff Unit Identity Unit
hashCashSpec =
  describe "HashCash" do
    let samples = 50
    let testReceipt = "Correctly checks that a challenge is satisfied by a receipt"
    it testReceipt do
      quickCheckAffInBrowser testReceipt samples testReceiptProp
    let testReceiptFalse = "Correctly checks that a challenge is not satisfied by a receipt"
    it testReceiptFalse do
      quickCheckAffInBrowser testReceiptFalse samples testReceiptFalseProp
    let computeReceiptTest = "Correctly computes a receipt"
    it computeReceiptTest $ invincible $ do
      quickCheckAffInBrowser computeReceiptTest samples computeReceiptProp

computeReceiptProp :: HexString -> Aff Result
computeReceiptProp toll = do
  let cost = 2
  receipt <- computeReceipt hashFuncSHA256 { toll, cost }
  verification <- verifyReceipt hashFuncSHA256 { toll, cost } receipt
  pure $ verification <?> (show receipt) <> " does not satisfy " <> (show toll)

testReceiptProp :: HexString -> Aff Result
testReceiptProp hexString = do
  challenge    <- fromArrayBuffer <$> (hashFuncSHA256 $ (toArrayBuffer hexString) : Nil)
  verification <- verifyReceipt hashFuncSHA256 { toll: challenge, cost: 2 } hexString
  pure $ verification <?> "Hash of " <> (show hexString) <> " challenge is not solved by " <> (show hexString)

testReceiptFalseProp :: HexString -> Aff Result
testReceiptFalseProp hexString = do
  challenge    <- fromArrayBuffer <$> (hashFuncSHA256 $ (toArrayBuffer hexString) : Nil)
  let falseReceipt = (hex $ (show hexString) <> "1")
  verification <- verifyReceipt hashFuncSHA256 { toll: challenge, cost: 10 } falseReceipt
  pure $ (not verification) <?> "Hash of " <> (show hexString) <> " challenge (" <> (show challenge) <> ") is solved by " <> (show falseReceipt)
