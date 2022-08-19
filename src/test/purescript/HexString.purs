module Test.HexString where

import Control.Bind (discard)
import Data.Either (hush)
import Data.Function (($))
import Data.HexString (hex, isHex)
import Data.Identity (Identity)
import Data.Maybe (isJust)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (toLower)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (noFlags)
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Test.Spec (describe, it, SpecT)
import Test.Spec.Assertions (shouldEqual)
import TestUtilities (makeTestableOnBrowser)

hexSpec :: SpecT Aff Unit Identity Unit
hexSpec =
  describe "HexString" do
    let createRegex = "create regex"
    it createRegex do
      makeTestableOnBrowser createRegex (isJust $ hush $ regex "^[0-9a-fA-F]+$" noFlags) shouldEqual true
    let checksHex = "checks if a string is hex formatted"
    it checksHex do
      let hexString = "e1a3c425"
      let nonHexString = "fuffa"
      makeTestableOnBrowser (checksHex <> " - true")  (isHex hexString)    shouldEqual true
      makeTestableOnBrowser (checksHex <> " - false") (isHex nonHexString) shouldEqual false
    let constructor = "costructs hex"
    it constructor do
      let hexStringFromConstructor = hex $ "EEAF0AB9 ADB38DD6 9C33F80A FA8FC5E8 60726187 75FF3C0B 9EA2314C"
                <> "9C256576 D674DF74 96EA81D3 383B4813 D692C6E0 E0D5D8E2 50B98BE4"
                <> "8E495C1D 6089DAD1 5DC7D7B4 6154D6B6 CE8EF4AD 69B15D49 82559B29"
                <> "7BCF1885 C529F566 660E57EC 68EDBC3C 05726CC0 2FD4CBF4 976EAA9A"
                <> "FD5138FE 8376435B 9FC61D2F C0EB06E3"
      let hexString = "EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C"
                <> "9C256576D674DF7496EA81D3383B4813D692C6E0E0D5D8E250B98BE4"
                <> "8E495C1D6089DAD15DC7D7B46154D6B6CE8EF4AD69B15D4982559B29"
                <> "7BCF1885C529F566660E57EC68EDBC3C05726CC02FD4CBF4976EAA9A"
                <> "FD5138FE8376435B9FC61D2FC0EB06E3"
      makeTestableOnBrowser constructor (show hexStringFromConstructor) shouldEqual (toLower hexString)