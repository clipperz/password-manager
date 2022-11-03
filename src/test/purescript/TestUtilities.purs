module TestUtilities where

import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Error.Class (try)
import Control.Monad.Morph (generalize)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (State, runState)
import Control.Monad.State.Trans (mapStateT, StateT(..))
import Control.Semigroupoid ((<<<))
import Data.Boolean (otherwise)
import Data.Either (either)
import Data.Eq ((==))
import Data.Function (($), flip)
import Data.Functor ((<$>))
import Data.Functor.Coproduct.Inject (inj)
import Data.Identity (Identity)
import Data.List (List(..), (:), null, length, reverse)
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap, class Newtype)
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Show (show)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff, class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (message)
import Random.LCG (Seed, randomSeed)
import Test.Spec.Assertions (fail)
-- import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen(..), runGen, GenState, unGen)
import Test.QuickCheck (quickCheckPure', class Testable, Result(..), withHelp, checkResults, printSummary, randomSeed)
import TestClasses (class TestableAff, testAff, quickCheckPureAff)

parseErrorString :: String -> (String -> String)
parseErrorString s = \n -> "❌ Test '" <> n <> "' failed: " <> s

parseGoodString :: String -> (String -> String)
parseGoodString _ = \n -> "✅ Test '" <> n <> "' succeeded"

-- makeTestableOnBrowser :: forall e m t. MonadThrow Error m => MonadError e m => Show t => Eq t => t -> (t -> t -> m Unit) -> t -> m Unit
-- makeTestableOnBrowser :: forall t7 t9 b12 f36. Bind f36 => MonadError Error f36 => Show b12 => MonadEffect f36 => String -> t7 -> (t7 -> t9 -> f36 b12) -> t9 -> f36 b12 --TODO fix
makeTestableOnBrowser testName t1 testFunction t2 = do
  result <- try $ testFunction t1 t2
  let testLogString = either (parseErrorString <<< message) (parseGoodString <<< show) result
  log $ testLogString testName
  testFunction t1 t2

-- makeQuickCheckOnBrowser :: forall prop. Testable prop => Int -> String -> prop -> Aff Unit
makeQuickCheckOnBrowser n testName checkFunction = do
  seed <- liftEffect $ randomSeed
  let resultList = quickCheckPure' seed n checkFunction
  showQuickCheckResultsInBrowser testName resultList

showQuickCheckResultsInBrowser :: String -> List (Tuple Seed Result) -> Aff Unit
showQuickCheckResultsInBrowser testName resultList = do
  let result = checkResults resultList
  let errorLogStrings = ((flip parseErrorString) testName) <$> ((_.message) <$> result.failures)
  _ <- if null errorLogStrings then
        sequence $ (log <<< (parseGoodString "")) <$> ((testName <> " (" <> (show (length resultList)) <> " tests)") : Nil)
      else do
        log $ "Test '" <> testName <> "': " <> (printSummary result)
        sequence $ log <$> errorLogStrings
  fail $ show errorLogStrings

quickCheckAffInBrowser :: forall prop. TestableAff prop => String -> Int -> prop -> Aff Unit
quickCheckAffInBrowser testName n prop = do
  seed <- liftEffect $ randomSeed
  (quickCheckPureAff seed n prop) >>= (showQuickCheckResultsInBrowser testName)

-- failOnBrowser :: forall m. MonadThrow Error m => String -> m Unit
-- failOnBrowser :: forall m. Bind m => MonadEffect m => MonadThrow Error m => String -> String -> m Unit
failOnBrowser testName failString = do
  log $ (parseErrorString failString) testName
  fail failString
