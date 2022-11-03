module Test.Main where

import Control.Bind (bind, discard)
import Data.Function (($))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Utilities (utilitiesSpec)
import Test.SRP (srpSpec)
import Test.EncodeDecode (encodeDecodeSpec)
import Test.HexString (hexSpec)
import Test.HashCash (hashCashSpec)
import Test.Import (importSpec)

main :: Effect Unit
main = launchAff_ $ do
  _ <- runSpec [consoleReporter] do
    -- utilitiesSpec
    srpSpec
    -- encodeDecodeSpec
    hexSpec
    -- hashCashSpec
    -- importSpec
  liftEffect $ log "END TESTS"
