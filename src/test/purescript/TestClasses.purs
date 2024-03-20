module TestClasses where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array.NonEmpty (toArray, cons, singleton)
import Data.Char (fromCharCode)
import Data.Codec.Argonaut as CA
import Data.Enum (toEnumWithDefaults)
import Data.HexString (hexChars)
import Data.List (List, reverse, (:))
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Gen (genAsciiString, genString)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Random.LCG (Seed)
import Test.QuickCheck (Result(..))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, runGen, suchThat, chooseInt, arrayOf1, oneOf, elements)

class TestableAff prop where
  testAff :: prop -> Gen (Aff Result)

instance testableAffAff :: TestableAff (Aff Result) where
  testAff = pure 

instance testableAffResult :: TestableAff Result where
  testAff = pure <<< pure

instance testableAffGen :: TestableAff prop => TestableAff (Gen prop) where
  testAff = flip bind testAff

instance testableAffBoolean :: TestableAff Boolean where
  testAff true = pure $ pure $ Success
  testAff false = pure $ pure $ Failed "Test returned false"

instance testableAffFunction :: (Arbitrary t, TestableAff prop) => TestableAff (t -> prop) where
  testAff f = arbitrary >>= testAff <<< f


type PureLoopState = { seed:: Seed, index:: Int, results:: List (Tuple Seed Result)}

quickCheckPureAff :: forall prop. TestableAff prop => Seed -> Int -> prop -> Aff (List (Tuple Seed Result))
quickCheckPureAff s n prop = tailRecM loop { seed: s, index: 0, results: mempty }
  where
    loop :: PureLoopState -> Aff (Step PureLoopState (List (Tuple Seed Result)))
    loop { seed, index, results }
      | index == n = pure $ Done $ reverse results
      | otherwise = do
          case runGen (testAff prop) { newSeed: seed, size: 10 } of
            Tuple r {newSeed} -> do -- r :: Aff Result
              r' <- r
              pure $ Loop
                { seed: newSeed
                , index: index + 1
                , results: (Tuple seed r') : results
                }

newtype AsciiString = Ascii String

derive instance asciiStringNewtype :: Newtype AsciiString _

instance asciiStringArbitrary :: Arbitrary AsciiString where
  arbitrary = Ascii <$> genAsciiString 

{- Unicode strings without not defined codepoints -}
newtype UnicodeString = Unicode String

derive instance unicodeStringNewtype :: Newtype UnicodeString _

derive instance unicodeStringEq :: Eq UnicodeString

instance unicodeStringShow :: Show UnicodeString where
  show (Unicode s) = s

instance unicodeStringArbitrary :: Arbitrary UnicodeString where
  arbitrary = let ranges = cons (Tuple 63744 65535) $ singleton (Tuple 0 55295)
                  toCharGen = \(Tuple b t) -> toEnumWithDefaults (fromMaybe 'a' (fromCharCode b)) (fromMaybe 'a' (fromCharCode t)) <$> chooseInt b t
                  charGen = oneOf $ (toCharGen <$> ranges)
              in Unicode <$> genString charGen

unicodeStringCodec :: CA.JsonCodec UnicodeString
unicodeStringCodec = wrapIso Unicode $ CA.string

newtype HexCharsString = HexChars String

derive instance hexCharsStringNewtype :: Newtype HexCharsString _

instance hexCharsStringArbitrary :: Arbitrary HexCharsString where
  arbitrary = HexChars <$> (fromCharArray <<< toArray) <$> (arrayOf1 (elements hexChars))

newtype PositiveInt = Positive Int

derive instance positiveIntNewtype :: Newtype PositiveInt _

instance positiveIntArbitrary :: Arbitrary PositiveInt where
  arbitrary = Positive <$> (suchThat arbitrary (_ > 0)) 
