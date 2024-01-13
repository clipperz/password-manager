module DataModel.WidgetState where

import Data.Maybe (Maybe)
import Data.Show (class Show)
import DataModel.Credentials (Credentials)
import DataModel.Index (Index)
import DataModel.User (UserPreferences)
import Views.CardsManagerView (CardManagerState)
import Views.LoginFormView (LoginFormData)
import Views.OverlayView (OverlayInfo)
import Views.SignupFormView (SignupDataForm)
import Views.UserAreaView (UserAreaState)

data Page = Loading (Maybe Page) | Login LoginFormData | Signup SignupDataForm | Main MainPageWidgetState

instance showPage :: Show Page where
  show (Loading _) = "Loading"
  show (Login _)   = "Login"
  show (Signup _)  = "Signup"
  show (Main _)    = "Main"

type MainPageWidgetState = {
  index                         :: Index
, credentials                   :: Credentials
, pinExists                     :: Boolean
, userAreaState                 :: UserAreaState
, cardManagerState              :: CardManagerState
, userPreferences               :: UserPreferences
}

data WidgetState = WidgetState OverlayInfo Page

