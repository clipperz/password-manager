module Test.TestMain where

import Control.Bind (bind, discard)
import Data.Function (($))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Test.Codec (codecSpec)
import Test.Donations (donationSpec)
import Test.EncodeDecode (encodeDecodeSpec)
import Test.HashCash (hashCashSpec)
import Test.HexString (hexSpec)
import Test.Import (importSpec)
import Test.SRP (srpSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Utilities (utilitiesSpec)

main :: Effect Unit
main = launchAff_ $ do
  _ <- runSpec [consoleReporter] do
    utilitiesSpec
    srpSpec
    encodeDecodeSpec
    hexSpec
    hashCashSpec
    importSpec
    codecSpec
    donationSpec
  liftEffect $ log "END TESTS"
