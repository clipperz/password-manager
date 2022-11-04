module Test.HexString where

import Control.Applicative (pure)
import Control.Bind (discard)
import Data.Either (hush)
import Data.EuclideanRing (mod)
import Data.Eq ((==))
import Data.Function (($))
import Data.HexString (HexString, Base(..), toString, hex, isHex, isHexRegex)
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
    let createRegex = "Correctly creates the isHex regex"
    it createRegex do
      makeTestableOnBrowser createRegex (isJust $ hush $ isHexRegex) shouldEqual true
    let checksNotHex = "Checks if a string is not hex formatted"
    it checksNotHex do
      let nonHexString = "fuffa"
      makeTestableOnBrowser checksNotHex (isHex nonHexString) shouldEqual false
    let checksHex = "Checks if a string is hex formatted"
    it checksHex do
      let prop = (\(h :: HexString) -> isHex (show h) <?> (show h <> " is not recognized as hex string")) :: HexString -> Result
      makeQuickCheckOnBrowser checksHex 10 prop
    let constructor = "Costructs HexString with hex from hex formatted string"
    it constructor do
      let prop = (\hs -> let s = unwrap hs
                             s' = if mod (length s) 2 == 0 then s else "0" <> s
                          in show (hex s) === (toLower s')) :: HexCharsString -> Result
      makeQuickCheckOnBrowser constructor 10 prop
    let constructAfterShow = "Constructs HexString from show HexString result"
    it constructAfterShow do
      let prop = (\(h :: HexString) -> h === (hex (show h))) :: HexString -> Result
      makeQuickCheckOnBrowser constructAfterShow 10 prop
    let compareHexStrings = "Compares HexStrings up to leading zeros"
    it compareHexStrings do
      let prop = (\h -> 
                  \h' ->
                    h === (hex ("0" <> show h))) :: HexString -> HexString -> Result
      makeQuickCheckOnBrowser compareHexStrings 10 prop
    let hexEncode = "Check correctness of creation from non hex string"
    it hexEncode do
      let fromHex = hex "74736368c3bc73"
      let fromString = hex "tschüs"
      makeTestableOnBrowser hexEncode fromHex shouldEqual fromString
    let hexDecode = "Check correctness of conversion to base 10"
    it hexDecode do
      let fromHex = hex "74736368c3bc73"
      let str = "tschüs"
      makeTestableOnBrowser hexDecode str shouldEqual (toString Dec fromHex)
    -- let hexDecodeQuickCheck = "Check correctness of conversion to base 10 of random strings"
    -- it hexDecodeQuickCheck do
    --   let prop = \s -> s === (toString Dec (hex s))
    --   makeQuickCheckOnBrowser hexDecodeQuickCheck 10 prop
