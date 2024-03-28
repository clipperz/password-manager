module Functions.Handler.DonationEventHandler
  ( handleDonationPageEvent
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alternative (pure)
import Control.Bind (bind, (=<<), (>>=))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Data.Function ((#), ($))
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import DataModel.AppError (AppError(..))
import DataModel.AppState (AppState, InvalidStateError(..), ProxyResponse(..))
import DataModel.FragmentState as Fragment
import DataModel.UserVersions.User (UserInfo(..))
import DataModel.WidgetState (CardFormInput(..), CardViewState(..), Page(..), WidgetState(..))
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Functions.Communication.Users (updateUserInfo)
import Functions.Donations (DonationLevel(..), computeDonationLevel)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultErrorPage, noOperation, handleOperationResult, runStep)
import Record (merge)
import Views.AppView (emptyMainPageWidgetState)
import Views.CardsManagerView (cardManagerInitialState)
import Views.DonationViews (DonationPageEvent(..))
import Views.OverlayView (OverlayColor(..), hiddenOverlayInfo, spinnerOverlay)
import Views.UserAreaView (userAreaInitialState)

handleDonationPageEvent :: DonationPageEvent -> AppState -> Fragment.FragmentState -> Widget HTML OperationState

handleDonationPageEvent donationPageEvent state@{username: Just username, password: Just password, index: Just index, userInfo: Just userInfo@(UserInfo {userPreferences}), pinEncryptedPassword, donationLevel: Just donationLevel} fragmentState = do
  let defaultPage = { index
                    , credentials:      {username, password}
                    , pinExists:        isJust pinEncryptedPassword
                    , userPreferences
                    , userAreaState:    userAreaInitialState
                    , cardManagerState: cardManagerInitialState
                    , donationLevel
                    }

  case donationPageEvent of
    UpdateDonationLevel ->
      do
        newUserInfo                     <- runStep ((\now -> pure $ UserInfo ((unwrap userInfo) {dateOfLastDonation = Just now})) =<< liftEffect nowDateTime) (WidgetState (spinnerOverlay "Update user info" Black) (Main emptyMainPageWidgetState { index = index, donationLevel = DonationOk }))
        ProxyResponse proxy stateUpdate <- runStep (updateUserInfo state newUserInfo)                                                                         (WidgetState (spinnerOverlay "Update user info" Black) (Main emptyMainPageWidgetState { index = index, donationLevel = DonationOk }))
        newDonationLevel                <- runStep (computeDonationLevel index newUserInfo # liftEffect)                                                      (WidgetState (spinnerOverlay "Update user info" Black) (Main emptyMainPageWidgetState { index = index, donationLevel = DonationOk }))
        
        let cardViewState = case fragmentState of
                        Fragment.AddCard card -> CardForm (NewCardFromFragment card)
                        _                     -> NoCard

        pure $ Tuple 
          (merge stateUpdate state {proxy = proxy, donationLevel = Just newDonationLevel})
          (WidgetState 
            hiddenOverlayInfo
            (Main emptyMainPageWidgetState  { index            = index
                                            , cardManagerState = cardManagerInitialState { cardViewState = cardViewState }
                                            , donationLevel    = newDonationLevel
                                            }
            )
          )

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black

    CloseDonationPage -> noOperation (Tuple state $ WidgetState hiddenOverlayInfo (Main defaultPage))

handleDonationPageEvent _ state _ = do
  throwError $ InvalidStateError (CorruptedState "DonationPage")
  # runExceptT
  >>= handleOperationResult state defaultErrorPage true White