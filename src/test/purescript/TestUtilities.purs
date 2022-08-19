module TestUtilities where

import Control.Bind (bind, discard)
import Control.Monad.Error.Class (try)
import Control.Semigroupoid ((<<<))
import Data.Either (either)
import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Show (show)
import Effect.Class.Console (log)
import Effect.Exception (message)
import Test.Spec.Assertions (fail)

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

-- failOnBrowser :: forall m. MonadThrow Error m => String -> m Unit
-- failOnBrowser :: forall m. Bind m => MonadEffect m => MonadThrow Error m => String -> String -> m Unit
failOnBrowser testName failString = do
    log $ (parseErrorString failString) testName
    fail failString