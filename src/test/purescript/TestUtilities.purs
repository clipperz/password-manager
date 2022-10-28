module TestUtilities where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Error.Class (try)
import Control.Semigroupoid ((<<<))
import Data.Either (either)
import Data.Foldable (foldl)
import Data.Function (($), flip)
import Data.Functor ((<$>))
import Data.List (List(..), (:), null)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Traversable (sequence)
import Data.Unit (Unit, unit)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (message)
import Test.Spec.Assertions (fail)
-- import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck (class Testable, (<?>))
import Test.QuickCheck (quickCheck, quickCheckPure', checkResults, printSummary, randomSeed, Result(..))

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
  let result = checkResults $ quickCheckPure' seed n checkFunction
  let errorLogStrings = ((flip parseErrorString) testName) <$> ((_.message) <$> result.failures)
  _ <- if null errorLogStrings then
        sequence $ (log <<< (parseGoodString "")) <$> (testName : Nil)
      else do
        log $ "Test '" <> testName <> "': " <> (printSummary result)
        sequence $ log <$> errorLogStrings
  fail $ show errorLogStrings

  where
    foldFunction :: List String -> Result -> List String
    foldFunction list Success = list
    foldFunction list (Failed s) = s : list

-- failOnBrowser :: forall m. MonadThrow Error m => String -> m Unit
-- failOnBrowser :: forall m. Bind m => MonadEffect m => MonadThrow Error m => String -> String -> m Unit
failOnBrowser testName failString = do
  log $ (parseErrorString failString) testName
  fail failString
