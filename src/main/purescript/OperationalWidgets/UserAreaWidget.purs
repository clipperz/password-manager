module OperationalWidgets.UserAreaWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text, div)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT, ExceptT(..), except)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List (List(..), (:), concat, fromFoldable)
import Data.Show (show)
import Data.Traversable (sequence)
import DataModel.AppState (AppError)
import DataModel.Card (Card)
import DataModel.Index (Index(..), CardEntry)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Communication.Cards (postCard, updateIndex)
import Functions.Import (decodeImport, parseHTMLImport, decodeHTML)
import Views.SimpleWebComponents (loadingDiv, simpleFileInputWidget, simpleButton)
import OperationalWidgets.ImportWidget (importWidget)

data UserAreaAction = Loaded (Either AppError Index) | Logout

userAreaWidget :: Index -> Widget HTML UserAreaAction
userAreaWidget index@(Index entries) = 
  div [Props._id "userSidebar"] [
    Loaded <$> importWidget index
  -- , changePasswordWidget Default emptyChangePasswordDataForm
  , simpleButton "Logout" false Logout
  ]

-- type ChangePasswordDataForm = { username       :: String
--                               , password       :: String
--                               , verifyPassword :: String
--                               , checkboxes     :: Tuple String Boolean
--                               }

-- changePasswordWidget :: WidgetState -> ChangePasswordDataForm -> forall a. Widget HTML a
-- changePasswordWidget state changeForm = do
