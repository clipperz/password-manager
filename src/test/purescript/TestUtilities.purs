module TestUtilities where

import Control.Alternative (pure)
import Control.Bind (bind, discard, (>>=), class Bind)
import Control.Monad.Error.Class (try, class MonadThrow, class MonadError)
import Control.Semigroupoid ((<<<))
import Data.Either (either)
import Data.Function (($), flip)
import Data.Functor ((<$>))
import Data.List (List(..), (:), null, length)
import Data.Semigroup ((<>))
import Data.Show (show, class Show)
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Exception (message, Error)
import Random.LCG (Seed)
import Test.QuickCheck (quickCheckPure', Result, class Testable, checkResults, printSummary, randomSeed)
import Test.Spec.Assertions (fail)
import TestClasses (class TestableAff, quickCheckPureAff)

foreign import logTest :: String -> Unit

parseErrorString :: String -> (String -> String)
parseErrorString s = \n -> "❌ Test '" <> n <> "' failed: " <> s

parseGoodString :: String -> (String -> String)
parseGoodString _ = \n -> "✅ Test '" <> n <> "' succeeded"

makeTestableOnBrowser :: forall a b s m. Bind m => MonadError Error m => Show s => MonadEffect m => String -> a -> (a -> b -> m s) -> b -> m s
makeTestableOnBrowser testName t1 testFunction t2 = do
  result <- try $ testFunction t1 t2
  let testLogString = either (parseErrorString <<< message) (parseGoodString <<< show) result
  pure $ logTest $ testLogString testName
  testFunction t1 t2

makeQuickCheckOnBrowser :: forall prop. Testable prop => String -> Int -> prop -> Aff Unit
makeQuickCheckOnBrowser testName n checkFunction = do
  seed <- liftEffect $ randomSeed
  let resultList = quickCheckPure' seed n checkFunction
  showQuickCheckResultsInBrowser testName resultList

showQuickCheckResultsInBrowser :: String -> List (Tuple Seed Result) -> Aff Unit
showQuickCheckResultsInBrowser testName resultList = do
  let result = checkResults resultList
  let errorLogStrings = ((flip parseErrorString) testName) <$> ((_.message) <$> result.failures)
  _ <- if null errorLogStrings then
        sequence $ (pure <<< logTest <<< (parseGoodString "")) <$> ((testName <> " (" <> (show (length resultList)) <> " tests)") : Nil)
      else do
        pure $ logTest $ "Test '" <> testName <> "': " <> (printSummary result)
        pure $ logTest <$> errorLogStrings
  fail $ show errorLogStrings

quickCheckAffInBrowser :: forall prop. TestableAff prop => String -> Int -> prop -> Aff Unit
quickCheckAffInBrowser testName n prop = do
  seed <- liftEffect $ randomSeed
  (quickCheckPureAff seed n prop) >>= (showQuickCheckResultsInBrowser testName)

-- failOnBrowser :: forall m. MonadThrow Error m => String -> m Unit
-- failOnBrowser :: forall m. Bind m => MonadEffect m => MonadThrow Error m => String -> String -> m Unit
failOnBrowser :: forall m. Bind m => MonadEffect m => MonadThrow Error m => String -> String -> m Unit
failOnBrowser testName failString = do
  pure $ logTest $ (parseErrorString failString) testName
  fail failString
