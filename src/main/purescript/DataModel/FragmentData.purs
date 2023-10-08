module DataModel.FragmentData where

import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import DataModel.Card (Card)
import DataModel.Credentials (Credentials)

data FragmentData = Registration | Login Credentials | AddCard Card | Unrecognized String | Empty
instance showFragmentData :: Show FragmentData where
  show Registration            = "Registration"
  show (Login cred)            = ("Login: "       <> show cred)
  show (AddCard card)          = ("AddCard: "     <> show card)
  show (Unrecognized fragment) = ("Urecognized: " <> fragment)
  show (Empty)                 = "Empty"

getCardToAdd :: FragmentData -> Maybe Card
getCardToAdd (AddCard c) = Just c
getCardToAdd _           = Nothing