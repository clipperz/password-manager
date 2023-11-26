module OperationalWidgets.App ( app ) where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alt ((<|>))
import Control.Alternative ((*>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (=<<), (>>=))
import Control.Category ((<<<))
import Control.Monad.Except (except, throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.CommutativeRing ((+))
import Data.Either (Either(..), either, note)
import Data.Function ((#), ($))
import Data.Functor ((<$))
import Data.HexString (toArrayBuffer)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Ord ((<))
import Data.Show (show)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (Credentials, emptyCredentials)
import DataModel.FragmentState as Fragment
import DataModel.StatelessAppState (ProxyResponse(..), StatelessAppState)
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.Login (PrepareLoginResult, loginStep1, loginStep2, prepareLogin)
import Functions.Communication.Signup (signupUser)
import Functions.Communication.Users (getStatelessIndex, getStatelessUserPreferences)
import Functions.Pin (decryptPassphraseWithPin, deleteCredentials, makeKey)
import Functions.SRP (checkM2)
import Functions.Timer (activateTimer)
import Record (merge)
import Unsafe.Coerce (unsafeCoerce)
import Views.AppView (CardManagerEvent, LoginFormData, LoginPageEvent(..), LoginType(..), Page(..), PageEvent(..), SignupPageEvent(..), UserAreaEvent, WidgetState(..), appView, emptyLoginFormData, emptyMainPageWidgetState)
import Views.OverlayView (OverlayStatus(..), hiddenOverlayInfo)
import Views.SignupFormView (SignupDataForm, emptyDataForm)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

-- =========================================================
--  manage application effects (state, local storage, api)
-- =========================================================

getLoginFormData :: StatelessAppState -> LoginFormData
getLoginFormData {username: Just username, pinEncryptedPassword: Just _} = emptyLoginFormData { credentials = {username, password: ""}, loginType = PinLogin }
getLoginFormData _ = emptyLoginFormData

app :: forall a. StatelessAppState -> Fragment.FragmentState -> Widget HTML a
app appState fragmentState = case fragmentState of
    Fragment.Login cred   -> appWithInitialOperation appState (LoginPageEvent $ LoginEvent cred)
    Fragment.Registration -> appLoop          (Tuple appState (WidgetState hiddenOverlayInfo (Signup emptyDataForm)))
    _                     -> appLoop          (Tuple appState (WidgetState hiddenOverlayInfo (Login  $ getLoginFormData appState)))
  
  where
    appWithInitialOperation :: StatelessAppState -> PageEvent -> Widget HTML a
    appWithInitialOperation state event = do
      appLoop =<< executeOperation event state

    appLoop :: (Tuple StatelessAppState WidgetState) -> Widget HTML a
    appLoop (Tuple state widgetState) = do

      resultEvent <- appView widgetState
        
      appLoop =<< executeOperation resultEvent state

type OperationState = Tuple StatelessAppState WidgetState

executeOperation :: PageEvent -> StatelessAppState -> Widget HTML OperationState
executeOperation (SignupPageEvent  event) = handleSignupPageEvent  event
executeOperation (LoginPageEvent   event) = handleLoginPageEvent   event
executeOperation (CardManagerEvent event) = handleCardManagerEvent event
executeOperation (UserAreaEvent    event) = handleUserAreaEvent    event


-- ============ HANDLE SIGNUP PAGE EVENTS ============

handleSignupPageEvent :: SignupPageEvent -> StatelessAppState -> Widget HTML OperationState 

handleSignupPageEvent (SignupEvent cred) state@{proxy, hash, srpConf} = 
  do
    ProxyResponse newProxy signupResult <- runStep (signupUser proxy hash srpConf cred) (WidgetState { status: Spinner, message: "registering" } initialPage)
    res                                 <- loginSteps cred (state {proxy = newProxy}) initialPage signupResult
    pure res
  
  # runExceptT 
  >>= handleOperationResult state initialPage

  where
    initialPage        = Signup $ getSignupDataFromCredentials cred

handleSignupPageEvent (GoToLoginEvent cred) state = doNothing $ Tuple state (WidgetState hiddenOverlayInfo (Login (getLoginFormData state) {credentials = cred}))


getSignupDataFromCredentials :: Credentials -> SignupDataForm
getSignupDataFromCredentials {username, password} = { username
                                                    , password
                                                    , verifyPassword: password
                                                    , checkboxes: [ Tuple "terms_of_service" true
                                                                  , Tuple "not_recoverable" true
                                                                  ]
                                                    }

-- ============ HANDLE LOGIN PAGE EVENTS ============

handleLoginPageEvent :: LoginPageEvent -> StatelessAppState -> Widget HTML OperationState 

handleLoginPageEvent (LoginEvent cred) state@{proxy, srpConf} =
  do
    ProxyResponse proxy' prepareLoginResult <- runStep (prepareLogin proxy srpConf cred) (WidgetState { status: Spinner, message: "Prepare login" } initialPage)
    res                                     <- loginSteps cred (state {proxy = proxy'}) initialPage prepareLoginResult
    pure res
  
  # runExceptT 
  >>= handleOperationResult state initialPage

  where 
    initialPage        = (Login  emptyLoginFormData {credentials = cred})

handleLoginPageEvent (LoginPinEvent pin) state@{proxy, hash, srpConf, username, pinEncryptedPassword} = do
  do
    cred                                    <- runStep (decryptPassphraseWithPin hash pin username pinEncryptedPassword) (WidgetState {status: Spinner, message: "Decrypt with PIN"} initialPage)
    ProxyResponse proxy' prepareLoginResult <- runStep (prepareLogin proxy srpConf cred)                                 (WidgetState {status: Spinner, message: "Prepare login"   } initialPage)
    res                                     <- loginSteps cred (state {proxy = proxy'}) initialPage prepareLoginResult
    pure res
  
  # runExceptT
  >>= handlePinResult state initialPage
  >>= (\(Tuple page either) -> handleOperationResult state page either)

  where
    initialPage = Login emptyLoginFormData {pin = pin, loginType = PinLogin}

handleLoginPageEvent (GoToSignupEvent cred) state = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Signup emptyDataForm {username = cred.username, password = cred.password})))

handleLoginPageEvent (GoToCredentialLoginEvent username) state = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Login emptyLoginFormData {credentials = {username, password: ""}})))

-- ============ HANDLE CARD MANAGER EVENTS ============

handleCardManagerEvent :: CardManagerEvent -> StatelessAppState -> Widget HTML OperationState
handleCardManagerEvent _ state = doNothing (Tuple state (WidgetState { status: Hidden , message: "" } (Login emptyLoginFormData))) --TODO

-- ============ HANDLE USER AREA EVENTS ============

handleUserAreaEvent :: UserAreaEvent -> StatelessAppState -> Widget HTML OperationState
handleUserAreaEvent _ state = doNothing (Tuple state (WidgetState { status: Hidden , message: "" } (Login emptyLoginFormData))) --TODO

-- ============

doNothing :: OperationState -> Widget HTML OperationState 
doNothing operationState@(Tuple _ widgetState) = (pure operationState) <|> (unsafeCoerce unit <$ appView widgetState)

loginSteps :: Credentials -> StatelessAppState -> Page -> PrepareLoginResult -> ExceptT AppError (Widget HTML) OperationState
loginSteps cred state@{proxy, hash: hashFunc, srpConf} page prepareLoginResult = do

  ProxyResponse proxy'   loginStep1Result <- runStep (loginStep1         proxy   hashFunc srpConf prepareLoginResult.c)                                       (WidgetState {status: Spinner, message: "SRP step 1"   } page)
  ProxyResponse proxy''  loginStep2Result <- runStep (loginStep2         proxy'  hashFunc srpConf prepareLoginResult.c prepareLoginResult.p loginStep1Result) (WidgetState {status: Spinner, message: "SRP step 2"   } page)
  _                                       <- runStep ((liftAff $ checkM2 srpConf loginStep1Result.aa loginStep2Result.m1 loginStep2Result.kk (toArrayBuffer loginStep2Result.m2)) >>= (\result -> 
                                                      if result
                                                      then pure         unit
                                                      else throwError $ ProtocolError (SRPError "Client M2 doesn't match with server M2")
                                                     ))                                                                                                       (WidgetState {status: Spinner, message: "Validate user"} page)

  let stateUpdate = { userInfoReferences: Just loginStep2Result.userInfoReferences 
                    , masterKey:          Just loginStep2Result.masterKey 
                    , username:           Just cred.username
                    , password:           Just cred.password
                    , s:                  Just loginStep1Result.s
                    , c:                  Just prepareLoginResult.c
                    , p:                  Just prepareLoginResult.p
                    }

  res                                     <- loadHomePageSteps (merge stateUpdate (state {proxy = proxy''}))

  pure $ res

loadHomePageSteps :: StatelessAppState -> ExceptT AppError (Widget HTML) OperationState
loadHomePageSteps state@{hash: hashFunc, proxy, userInfoReferences: maybeUserInfoReferences} = do
  let page = Main emptyMainPageWidgetState
  userInfoReferences                    <- except $ note (InvalidStateError $ CorruptedState "UserInfoReferences is Nothing") maybeUserInfoReferences
  ProxyResponse proxy'  userPreferences <- runStep (getStatelessUserPreferences { proxy,            hashFunc } (unwrap userInfoReferences).preferencesReference) (WidgetState {status: Spinner, message: "Get user preferences"} page)
  ProxyResponse proxy'' index           <- runStep (getStatelessIndex           { proxy: proxy',    hashFunc } (unwrap userInfoReferences).indexReference)       (WidgetState {status: Spinner, message: "Get index"}            page)
  
  case (unwrap userPreferences).automaticLock of
    Right n -> liftEffect (activateTimer n)
    Left  _ -> pure unit


  pure $ Tuple (state {proxy = proxy'', index = Just index}) (WidgetState {status: Hidden, message: ""} (Main { index, cardToAdd: Nothing, showUserArea: false })) -- TODO: change Nothing in cardToAdd from fragment data [fsolaroli - 26/11/2023]

runStep :: forall a. ExceptT AppError Aff a -> WidgetState -> ExceptT AppError (Widget HTML) a
-- runStep step widgetState = ExceptT $ ((step # runExceptT # liftAff) <* (liftAff $ delay (Milliseconds 1000.0))) <|> (defaultView widgetState)
runStep step widgetState = ExceptT $ (step # runExceptT # liftAff) <|> (defaultView widgetState)

defaultView :: forall a. WidgetState -> Widget HTML a
defaultView widgetState = (unsafeCoerce unit <$ appView widgetState)

type MaxPinAttemptsReached = Boolean

handlePinResult :: StatelessAppState -> Page -> Either AppError OperationState -> Widget HTML (Tuple Page (Either AppError OperationState))
handlePinResult state page either = do
  storage <- liftEffect $ window >>= localStorage
  newPage <- case either of
    Right _ -> ( do
        liftEffect $ setItem (makeKey "failures") (show 0) storage
        pure $ page 
      ) <|> (defaultView (WidgetState {status: Spinner, message: "Reset PIN attempts"} page))
    Left  _ -> ( do
        failures <- liftEffect $ getItem (makeKey "failures") storage
        let count = (((fromMaybe 0) <<< fromString <<< (fromMaybe "")) failures) + 1
        if count < 3 then do
          liftEffect $ setItem (makeKey "failures") (show count) storage
          pure $ Login $ emptyLoginFormData {credentials = emptyCredentials {username = fromMaybe "" state.username}, loginType = PinLogin}
        else do
          liftEffect $ deleteCredentials storage
          pure $ Login $ emptyLoginFormData {credentials = emptyCredentials {username = fromMaybe "" state.username}, loginType = CredentialLogin}
      ) <|> (defaultView (WidgetState {status: Spinner, message: "Compute PIN attempts"} page))

  pure $ Tuple newPage either

handleOperationResult :: StatelessAppState -> Page -> Either AppError OperationState -> Widget HTML OperationState
handleOperationResult state page = either
                                        manageError
                                        (\res@(Tuple _ (WidgetState _ page')) -> delayOperation 500 (WidgetState { status: Done, message: "" } page') *> pure res)
                                                
  where
    manageError :: AppError -> Widget HTML OperationState
    manageError error = 
      case error of
        -- _ -> ErrorPage --TODO
        _ -> do
          delayOperation 500 (WidgetState { status: Failed,  message: "error" } page)
          pure $ Tuple state (WidgetState { status: Hidden,  message: ""      } page)

delayOperation :: Int -> WidgetState -> Widget HTML Unit
delayOperation time widgetState = ((liftAff $ delay (Milliseconds $ toNumber time)) <|> (unit <$ appView widgetState))
