module Test.HexString where

import Control.Applicative (pure)
import Control.Bind (discard)
import Data.Either (hush)
import Data.EuclideanRing (mod)
import Data.Eq ((==))
import Data.Function (($))
import Data.HexString (HexString, hex, isHex)
import Data.Identity (Identity)
import Data.Maybe (isJust)
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.CodeUnits (length)
import Data.String.Common (toLower)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (noFlags)
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Test.Spec (describe, it, SpecT)
import Test.Spec.Assertions (shouldEqual)
import Test.QuickCheck ((<?>), (===), Result)
import Test.QuickCheck.Gen (Gen)
import TestClasses (HexCharsString)
import TestUtilities (makeTestableOnBrowser, makeQuickCheckOnBrowser, quickCheckAffInBrowser)

hexSpec :: SpecT Aff Unit Identity Unit
hexSpec =
  describe "HexString" do
    let createRegex = "create regex"
    it createRegex do
      makeTestableOnBrowser createRegex (isJust $ hush $ regex "^[0-9a-fA-F]+$" noFlags) shouldEqual true
    let checksNotHex = "checks if a string is not hex formatted"
    it checksNotHex do
      let nonHexString = "fuffa"
      makeTestableOnBrowser checksNotHex (isHex nonHexString) shouldEqual false
    let checksHex = "checks if a string is hex formatted"
    it checksHex do
      -- let prop = \(h :: HexString) -> pure $ isHex (show h) <?> (show h <> " is not recognized as hex string")
      -- quickCheckAffInBrowser checksHex 10 prop
      let prop = (\(h :: HexString) -> isHex (show h) <?> (show h <> " is not recognized as hex string")) :: HexString -> Result
      makeQuickCheckOnBrowser checksHex 10 prop
    let constructor = "costructs hex"
    it constructor do
      let prop = (\hs -> let s = unwrap hs
                             s' = if mod (length s) 2 == 0 then s else "0" <> s
                          in show (hex s) === (toLower s')) :: HexCharsString -> Result
      makeQuickCheckOnBrowser constructor 10 prop
    let constructAfterShow = "constructs from show result"
    it constructAfterShow do
      let prop = (\(h :: HexString) -> h === (hex (show h))) :: HexString -> Result
      makeQuickCheckOnBrowser constructAfterShow 10 prop
