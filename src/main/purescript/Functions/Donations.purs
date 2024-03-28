module Functions.Donations where

import Control.Alt ((<$>))
import Control.Alternative (pure)
import Control.Bind (bind)
import Data.Boolean (otherwise)
import Data.DateTime (diff)
import Data.Eq (class Eq)
import Data.Function (($))
import Data.HeytingAlgebra ((&&), (||))
import Data.List (length)
import Data.Maybe (Maybe(..))
import Data.Ord ((<), (>), (>=))
import Data.Time.Duration (Days(..))
import DataModel.IndexVersions.Index (Index(..))
import DataModel.UserVersions.User (UserInfo(..))
import Effect (Effect)
import Effect.Now (nowDateTime)

data DonationLevel = DonationOk | DonationInfo | DonationWarning

derive instance eqDonationLevel :: Eq DonationLevel

type NumberOfCards = Int

lowerBoundaryNumberOfCards :: NumberOfCards
lowerBoundaryNumberOfCards = 10

upperBoundaryNumberOfCards :: NumberOfCards
upperBoundaryNumberOfCards = 80

lowerBoundaryNumberOfDays :: Maybe Days
lowerBoundaryNumberOfDays = Just $ Days 30.0

upperBoundaryNumberOfDays :: Maybe Days
upperBoundaryNumberOfDays = Just $ Days 120.0

computeDonationLevel :: Index -> UserInfo -> Effect DonationLevel
computeDonationLevel (Index {entries}) (UserInfo {dateOfLastDonation}) = do
  now <- nowDateTime
  let numberOfDays  = (diff now) <$> dateOfLastDonation
  let numberOfCards = length entries
  pure $ donationLevel numberOfCards numberOfDays
    where
      donationLevel numberOfCards numberOfDays
        | (numberOfCards <  lowerBoundaryNumberOfCards) || (numberOfDays >  Nothing && numberOfDays < lowerBoundaryNumberOfDays) = DonationOk
        | (numberOfCards >= upperBoundaryNumberOfCards) || (numberOfDays >= upperBoundaryNumberOfDays                          ) = DonationWarning
        -- | (numberOfCards >= lowerBoundaryNumberOfCards && numberOfCards <= upperBoundaryNumberOfCards) && (isNothing numberOfDays || between lowerBoundaryNumberOfDays upperBoundaryNumberOfDays numberOfDays) = DonationInfo 
        |  otherwise                                                                                                             = DonationInfo

donationLevelClass :: DonationLevel -> String
donationLevelClass donationLevel = case donationLevel of
  DonationOk      -> "DonationOk"
  DonationInfo    -> "DonationInfo"
  DonationWarning -> "DonationWarning"