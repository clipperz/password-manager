module OperationalWidgets.UserAreaWidget where

import Concur.Core (Widget)
import Concur.Core.FRP (loopS, fireOnce, demand)
import Concur.React (HTML)
import Concur.React.DOM (text, div, div', fieldset)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT, ExceptT(..), except)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HexString (HexString, fromArrayBuffer)
import Data.HeytingAlgebra ((&&), not)
import Data.List (List(..), (:), concat, fromFoldable)
import Data.Maybe (Maybe(..), isJust)
import Data.PrettyShow (prettyShow)
import Data.Show (show)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppError)
import DataModel.Card (Card)
import DataModel.Credentials (Credentials)
import DataModel.Index (Index(..), CardEntry)
import DataModel.SRP as SRP
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Communication.Cards (postCard, updateIndex)
import Functions.Import (decodeImport, parseHTMLImport, decodeHTML)
import Functions.JSState (getAppState)
import Functions.Password (standardPasswordStrengthFunction)
import Functions.SRP as SRP
import Functions.State (getSRPConf)
import Views.SimpleWebComponents (loadingDiv, simpleFileInputWidget, simpleButton, simpleUserSignal, simplePasswordSignal, simpleCheckboxSignal, simpleVerifiedPasswordSignal, PasswordForm)
import OperationalWidgets.ImportWidget (importWidget)
import OperationalWidgets.ChangePasswordWidget (changePasswordWidget, emptyChangePasswordDataForm)
import Record (merge)

data UserAreaAction = Loaded (Either AppError Index) | Logout

userAreaWidget :: Index -> Widget HTML UserAreaAction
userAreaWidget index@(Index entries) = 
  div [Props._id "userSidebar"] [
    Loaded <$> importWidget index
  , changePasswordWidget Default emptyChangePasswordDataForm
  , simpleButton "Logout" false Logout
  ]
