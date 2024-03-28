module Test.Donations where

import Control.Bind

import Data.Array (foldl, replicate)
import Data.DateTime (DateTime, adjust)
import Data.Eq (eq)
import Data.Function (flip, (#), ($))
import Data.HexString (hex)
import Data.HeytingAlgebra ((&&))
import Data.Identity (Identity)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Ring ((-), (+))
import Data.Set (empty)
import Data.Time.Duration (negateDuration)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import DataModel.CardVersions.CurrentCardVersions (currentCardVersion)
import DataModel.IndexVersions.CurrentIndexVersions (currentIndexVersion)
import DataModel.IndexVersions.Index (CardEntry(..), CardReference(..), Index(..))
import DataModel.UserVersions.User (IndexReference(..), UserInfo(..), defaultUserPreferences)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Functions.Donations (DonationLevel(..), computeDonationLevel, lowerBoundaryNumberOfCards, lowerBoundaryNumberOfDays, upperBoundaryNumberOfCards, upperBoundaryNumberOfDays)
import Test.Spec (describe, it, SpecT)
import Test.Spec.Assertions (shouldEqual)
import TestUtilities (makeTestableOnBrowser)

donationSpec :: SpecT Aff Unit Identity Unit
donationSpec  =
  describe "Donation" do
    let donationLevel = "Correct computation of donation level"
    it donationLevel do
      now <- liftEffect nowDateTime
      let donationData = [
        Tuple (Tuple  Nothing                                                               (lowerBoundaryNumberOfCards - 1)) DonationOk
      , Tuple (Tuple  Nothing                                                               lowerBoundaryNumberOfCards) DonationInfo
      , Tuple (Tuple  Nothing                                                               (lowerBoundaryNumberOfCards + 1)) DonationInfo
      , Tuple (Tuple  Nothing                                                               upperBoundaryNumberOfCards) DonationWarning
      , Tuple (Tuple  Nothing                                                               upperBoundaryNumberOfCards) DonationWarning
      , Tuple (Tuple (Just now)                                                                                      2) DonationOk
      , Tuple (Tuple (Just now)                                                             upperBoundaryNumberOfCards) DonationOk
      , Tuple (Tuple (Just now)                                                             lowerBoundaryNumberOfCards) DonationOk
      , Tuple (Tuple ((lowerBoundaryNumberOfDays) <#> negateDuration >>= (flip adjust now))                          2) DonationOk
      , Tuple (Tuple ((lowerBoundaryNumberOfDays) <#> negateDuration >>= (flip adjust now)) lowerBoundaryNumberOfCards) DonationInfo
      , Tuple (Tuple ((lowerBoundaryNumberOfDays) <#> negateDuration >>= (flip adjust now)) upperBoundaryNumberOfCards) DonationWarning
      , Tuple (Tuple ((upperBoundaryNumberOfDays) <#> negateDuration >>= (flip adjust now))                          2) DonationOk
      , Tuple (Tuple ((upperBoundaryNumberOfDays) <#> negateDuration >>= (flip adjust now)) lowerBoundaryNumberOfCards) DonationWarning
      , Tuple (Tuple ((upperBoundaryNumberOfDays) <#> negateDuration >>= (flip adjust now)) upperBoundaryNumberOfCards) DonationWarning
      ]

      result <- liftEffect $ (\(Tuple (Tuple time cards) level) -> (computeDonationLevelFromRawData cards time) <#> (eq level)) <$> donationData # sequence <#> (foldl (\a b -> a && b) true)
      makeTestableOnBrowser donationLevel result shouldEqual true

computeDonationLevelFromRawData :: Int -> Maybe DateTime -> Effect DonationLevel
computeDonationLevelFromRawData numberOfCards dateOfLastDonation = 
  let emptyCardEntry      = CardEntry      { title: "", tags: empty, lastUsed: 0.0, archived: false, cardReference: CardReference {identifier: hex "", key: hex "", reference: hex "", version: currentCardVersion}}
      emptyIndexReference = IndexReference { reference: hex "", key: hex "", version: currentIndexVersion}
      index               = Index          { entries: fromFoldable $ replicate numberOfCards emptyCardEntry, identifier: hex "" }
      userInfo            = UserInfo       { dateOfLastDonation, identifier: hex "", userPreferences: defaultUserPreferences, indexReference: emptyIndexReference}
  in computeDonationLevel index userInfo
  
