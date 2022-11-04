module TestClasses where

import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Semigroupoid ((<<<))
import Data.Boolean (otherwise)
import Data.Eq ((==))
import Data.Function (flip, ($))
import Data.Functor ((<$>))
import Data.List (List, reverse, (:))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype)
import Data.Ord ((>))
import Data.Semiring ((+))
import Data.String.Gen (genAsciiString)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Random.LCG (Seed, randomSeed)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck (Result(..))
import Test.QuickCheck.Gen (Gen, runGen, suchThat)

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

newtype PositiveInt = Positive Int

derive instance positiveIntNewtype :: Newtype PositiveInt _

instance positiveIntArbitrary :: Arbitrary PositiveInt where
  arbitrary = Positive <$> (suchThat arbitrary (_ > 0)) 
