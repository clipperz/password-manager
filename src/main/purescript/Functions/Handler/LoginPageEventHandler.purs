module Functions.Handler.LoginPageEventHandler where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Category ((<<<))
import Control.Monad.Except (except, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.CommutativeRing ((+))
import Data.Either (Either(..), note)
import Data.Function ((#), ($))
import Data.HexString (toArrayBuffer)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Ord ((<))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (Credentials, emptyCredentials)
import DataModel.FragmentState as Fragment
import DataModel.StatelessAppState (ProxyResponse(..), StatelessAppState)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.Login (PrepareLoginResult, loginStep1, loginStep2, prepareLogin)
import Functions.Communication.Users (getIndex, getStatelessUserPreferences)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultView, doNothing, handleOperationResult, runStep)
import Functions.Pin (decryptPassphraseWithPin, deleteCredentials, makeKey)
import Functions.SRP (checkM2)
import Functions.Timer (activateTimer)
import Record (merge)
import Views.AppView (Page(..), WidgetState(..), emptyMainPageWidgetState)
import Views.CardsManagerView (CardFormInput(..), CardViewState(..), cardManagerInitialState)
import Views.LoginFormView (LoginPageEvent(..), LoginType(..), emptyLoginFormData)
import Views.OverlayView (OverlayColor(..), OverlayStatus(..), hiddenOverlayInfo, spinnerOverlay)
import Views.SignupFormView (emptyDataForm)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

handleLoginPageEvent :: LoginPageEvent -> StatelessAppState -> Fragment.FragmentState -> Widget HTML OperationState 

handleLoginPageEvent (LoginEvent cred) state@{proxy, srpConf} fragmentState =
  do
    ProxyResponse proxy' prepareLoginResult <- runStep (prepareLogin proxy srpConf cred) (WidgetState (spinnerOverlay "Prepare login" Black) initialPage)
    res                                     <- loginSteps cred (state {proxy = proxy'}) fragmentState initialPage prepareLoginResult
    pure res
  
  # runExceptT 
  >>= handleOperationResult state initialPage Black

  where 
    initialPage = (Login emptyLoginFormData {credentials = cred})


handleLoginPageEvent (LoginPinEvent pin) state@{proxy, hash, srpConf, username, pinEncryptedPassword} fragmentState = do
  do
    cred                                    <- runStep (decryptPassphraseWithPin hash pin username pinEncryptedPassword) (WidgetState (spinnerOverlay "Decrypt with PIN" Black) initialPage)
    ProxyResponse proxy' prepareLoginResult <- runStep (prepareLogin proxy srpConf cred)                                 (WidgetState (spinnerOverlay "Prepare login"    Black) initialPage)
    res                                     <- loginSteps cred (state {proxy = proxy'}) fragmentState initialPage prepareLoginResult
    pure res
  
  # runExceptT
  >>= handlePinResult state initialPage Black
  >>= (\(Tuple page either) -> handleOperationResult state page Black either)

  where
    initialPage = Login emptyLoginFormData {pin = pin, loginType = PinLogin}


handleLoginPageEvent (GoToSignupEvent cred) state _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Signup emptyDataForm {username = cred.username, password = cred.password})))


handleLoginPageEvent (GoToCredentialLoginEvent username) state _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Login emptyLoginFormData {credentials = {username, password: ""}})))

-- ========================================================================================================================

loginSteps :: Credentials -> StatelessAppState -> Fragment.FragmentState -> Page -> PrepareLoginResult -> ExceptT AppError (Widget HTML) OperationState
loginSteps cred state@{proxy, hash: hashFunc, srpConf} fragmentState page prepareLoginResult = do

  ProxyResponse proxy'   loginStep1Result <- runStep (loginStep1         proxy   hashFunc srpConf prepareLoginResult.c)                                       (WidgetState {status: Spinner, color: Black, message: "SRP step 1"   } page)
  ProxyResponse proxy''  loginStep2Result <- runStep (loginStep2         proxy'  hashFunc srpConf prepareLoginResult.c prepareLoginResult.p loginStep1Result) (WidgetState {status: Spinner, color: Black, message: "SRP step 2"   } page)
  _                                       <- runStep ((liftAff $ checkM2 srpConf loginStep1Result.aa loginStep2Result.m1 loginStep2Result.kk (toArrayBuffer loginStep2Result.m2)) >>= (\result -> 
                                                      if result
                                                      then pure         unit
                                                      else throwError $ ProtocolError (SRPError "Client M2 doesn't match with server M2")
                                                     ))                                                                                                       (WidgetState {status: Spinner, color: Black, message: "Validate user"} page)

  let stateUpdate = { userInfoReferences: Just loginStep2Result.userInfoReferences 
                    , masterKey:          Just loginStep2Result.masterKey 
                    , username:           Just cred.username
                    , password:           Just cred.password
                    , s:                  Just loginStep1Result.s
                    , c:                  Just prepareLoginResult.c
                    , p:                  Just prepareLoginResult.p
                    }

  res                                     <- loadHomePageSteps (merge stateUpdate (state {proxy = proxy''})) fragmentState

  pure $ res

loadHomePageSteps :: StatelessAppState -> Fragment.FragmentState -> ExceptT AppError (Widget HTML) OperationState
loadHomePageSteps state@{hash: hashFunc, proxy, userInfoReferences: maybeUserInfoReferences} fragmentState = do
  userInfoReferences                    <- except $ note (InvalidStateError $ CorruptedState "UserInfoReferences is Nothing") maybeUserInfoReferences

  ProxyResponse proxy'  userPreferences <- runStep (getStatelessUserPreferences { proxy,            hashFunc } (unwrap userInfoReferences).preferencesReference) (WidgetState {status: Spinner, color: Black, message: "Get user preferences"} $ Main emptyMainPageWidgetState)
  ProxyResponse proxy'' index           <- runStep (getIndex                    { proxy: proxy',    hashFunc } (unwrap userInfoReferences).indexReference)       (WidgetState {status: Spinner, color: Black, message: "Get index"}            $ Main emptyMainPageWidgetState)
  
  case (unwrap userPreferences).automaticLock of
    Right n -> liftEffect (activateTimer n)
    Left  _ -> pure unit

  let cardViewState = case fragmentState of
                        Fragment.AddCard card -> CardForm (NewCard $ Just card)
                        _                     -> NoCard

  pure $ Tuple (state {proxy = proxy'', index = Just index, userPreferences = Just userPreferences}) (WidgetState {status: Hidden, color: Black, message: ""} (Main emptyMainPageWidgetState { index = index, cardManagerState = cardManagerInitialState { cardViewState = cardViewState } }))

type MaxPinAttemptsReached = Boolean

handlePinResult :: StatelessAppState -> Page -> OverlayColor -> Either AppError OperationState -> Widget HTML (Tuple Page (Either AppError OperationState))
handlePinResult state page color either = do
  storage <- liftEffect $ window >>= localStorage
  newPage <- case either of
    Right _ -> ( do
        liftEffect $ setItem (makeKey "failures") (show 0) storage
        pure $ page 
      ) <|> (defaultView (WidgetState {status: Spinner, color, message: "Reset PIN attempts"} page))
    Left  _ -> ( do
        failures <- liftEffect $ getItem (makeKey "failures") storage
        let count = (((fromMaybe 0) <<< fromString <<< (fromMaybe "")) failures) + 1
        if count < 3 then do
          liftEffect $ setItem (makeKey "failures") (show count) storage
          pure $ Login $ emptyLoginFormData {credentials = emptyCredentials {username = fromMaybe "" state.username}, loginType = PinLogin}
        else do
          liftEffect $ deleteCredentials storage
          pure $ Login $ emptyLoginFormData {credentials = emptyCredentials {username = fromMaybe "" state.username}, loginType = CredentialLogin}
      ) <|> (defaultView (WidgetState {status: Spinner, color, message: "Compute PIN attempts"} page))

  pure $ Tuple newPage either