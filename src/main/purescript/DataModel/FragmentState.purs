module DataModel.FragmentState where

import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import DataModel.CardVersions.Card (Card)
import DataModel.Credentials (Credentials)

data FragmentState = Registration | Login Credentials | AddCard Card | Unrecognized String | Empty
instance showFragmentData :: Show FragmentState where
  show Registration            = "Registration"
  show (Login cred)            = ("Login: "       <> show cred)
  show (AddCard card)          = ("AddCard: "     <> show card)
  show (Unrecognized fragment) = ("Urecognized: " <> fragment)
  show (Empty)                 = "Empty"