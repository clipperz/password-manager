module DataModel.WidgetState where

import Data.Bounded (class Ord)
import Data.Either (Either)
import Data.Eq (class Eq)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Show (class Show)
import Data.Tuple (Tuple)
import DataModel.Card (Card)
import DataModel.Credentials (Credentials)
import DataModel.Index (CardEntry, Index)
import DataModel.User (UserPreferences)
import IndexFilterView (FilterData)
import Views.OverlayView (OverlayInfo)
import Views.SignupFormView (SignupDataForm)
import Web.File.File (File)

data Page = Loading (Maybe Page) | Login LoginFormData | Signup SignupDataForm | Main MainPageWidgetState

instance showPage :: Show Page where
  show (Loading _) = "Loading"
  show (Login _)   = "Login"
  show (Signup _)  = "Signup"
  show (Main _)    = "Main"

-- ========================================================================

type PIN = String

data LoginType = CredentialLogin | PinLogin

type LoginFormData = 
  { credentials :: Credentials
  , pin :: PIN
  , loginType :: LoginType
  }

-- ========================================================================

type UserAreaState = {
  showUserArea     :: Boolean
, userAreaOpenPage :: UserAreaPage
, importState      :: ImportState
, userAreaSubmenus :: Map UserAreaSubmenu Boolean
}

data UserAreaPage = Export | Import | Pin | Delete | Preferences | ChangePassword | About | None
derive instance eqUserAreaPage :: Eq UserAreaPage

data ImportStep = Upload | Selection | Confirm

type ImportState = {
  step      :: ImportStep
, content   :: Either (Maybe File) String
, selection :: Array (Tuple Boolean Card)
, tag       :: Tuple Boolean String
}

data UserAreaSubmenu = Account | Data
derive instance  eqUserAreaSubmenus :: Eq  UserAreaSubmenu
derive instance ordUserAreaSubmenus :: Ord UserAreaSubmenu

-- ========================================================================

type MainPageWidgetState = {
  index                         :: Index
, credentials                   :: Credentials
, pinExists                     :: Boolean
, userAreaState                 :: UserAreaState
, cardManagerState              :: CardManagerState
, userPreferences               :: UserPreferences
}

data WidgetState = WidgetState OverlayInfo Page

-- -------------------------------------

data CardFormInput = NewCard | NewCardFromFragment Card | ModifyCard Card CardEntry
derive instance eqCardFormInput :: Eq CardFormInput

data CardViewState = NoCard | Card Card CardEntry | CardForm CardFormInput
derive instance eqCardViewState :: Eq CardViewState

type CardManagerState = { 
  filterData    :: FilterData
, selectedEntry :: Maybe CardEntry
, cardViewState :: CardViewState
}
