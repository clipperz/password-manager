module Functions.Handler.LoginPageEventHandler where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (=<<), (>>=))
import Control.Category ((<<<))
import Control.Monad.Except (throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.CommutativeRing ((+))
import Data.Either (Either(..))
import Data.Function ((#), ($))
import Data.HexString (hex, toArrayBuffer)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Ord ((<))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (unit)
import DataModel.AppError (AppError(..))
import DataModel.AppState (InvalidStateError(..), ProxyResponse(..), AppState)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (Credentials, emptyCredentials)
import DataModel.FragmentState as Fragment
import DataModel.WidgetState (CardFormInput(..), CardViewState(..), LoginType(..), Page(..), WidgetState(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.Login (PrepareLoginResult, loginStep1, loginStep2, prepareLogin)
import Functions.Communication.Users (extractUserInfoReference, getUserInfo)
import Functions.Donations (DonationLevel(..), computeDonationLevel)
import Functions.EncodeDecode (importCryptoKeyAesGCM)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultView, noOperation, handleOperationResult, runStep)
import Functions.Index (getIndex)
import Functions.Pin (decryptPassphraseWithPin, deleteCredentials, makeKey)
import Functions.SRP (checkM2)
import Functions.Timer (activateTimer)
import Record (merge)
import Views.AppView (emptyMainPageWidgetState)
import Views.CardsManagerView (cardManagerInitialState)
import Views.LoginFormView (LoginPageEvent(..), emptyLoginFormData)
import Views.OverlayView (OverlayColor(..), OverlayStatus(..), hiddenOverlayInfo, spinnerOverlay)
import Views.SignupFormView (emptyDataForm)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

handleLoginPageEvent :: LoginPageEvent -> AppState -> Fragment.FragmentState -> Widget HTML OperationState 

handleLoginPageEvent (LoginEvent cred) state@{srpConf} fragmentState =
  do
    prepareLoginResult <- runStep (prepareLogin srpConf cred) (WidgetState (spinnerOverlay "Prepare login" Black) initialPage)
    res                <- loginSteps cred state fragmentState initialPage prepareLoginResult
    pure res
  
  # runExceptT 
  >>= handleOperationResult state initialPage true Black

  where 
    initialPage = (Login emptyLoginFormData {credentials = cred})


handleLoginPageEvent (LoginPinEvent pin) state@{hash, srpConf, username, pinEncryptedPassword} fragmentState = do
  do
    cred               <- runStep (decryptPassphraseWithPin hash pin username pinEncryptedPassword) (WidgetState (spinnerOverlay "Decrypt with PIN" Black) initialPage)
    prepareLoginResult <- runStep (prepareLogin srpConf cred)                                       (WidgetState (spinnerOverlay "Prepare login"    Black) initialPage)
    res                <- loginSteps cred state fragmentState initialPage prepareLoginResult
    pure res
  
  # runExceptT
  >>= handlePinResult state initialPage Black
  >>= (\(Tuple page either) -> handleOperationResult state page true Black either)

  where
    initialPage = Login emptyLoginFormData {pin = pin, loginType = PinLogin}


handleLoginPageEvent (GoToSignupEvent cred) state _ = noOperation (Tuple state (WidgetState hiddenOverlayInfo (Signup emptyDataForm {username = cred.username, password = cred.password})))


handleLoginPageEvent (GoToCredentialLoginEvent username) state _ = noOperation (Tuple state (WidgetState hiddenOverlayInfo (Login emptyLoginFormData {credentials = {username, password: ""}})))

-- ========================================================================================================================

loginSteps :: Credentials -> AppState -> Fragment.FragmentState -> Page -> PrepareLoginResult -> ExceptT AppError (Widget HTML) OperationState
loginSteps cred state@{proxy, hash: hashFunc, srpConf} fragmentState page prepareLoginResult = do
  let connectionState = {proxy, hashFunc, srpConf, c : hex "", p: hex ""}
  ProxyResponse proxy'   loginStep1Result <- runStep (loginStep1         connectionState                 prepareLoginResult.c                                      ) (WidgetState {status: Spinner, color: Black, message: "SRP step 1"   } page)
  ProxyResponse proxy''  loginStep2Result <- runStep (loginStep2         connectionState{proxy = proxy'} prepareLoginResult.c prepareLoginResult.p loginStep1Result) (WidgetState {status: Spinner, color: Black, message: "SRP step 2"   } page)
  _                                       <- runStep ((liftAff $ checkM2 srpConf loginStep1Result.aa loginStep2Result.m1 loginStep2Result.kk (toArrayBuffer loginStep2Result.m2)) >>= (\result -> 
                                                      if result
                                                      then pure         unit
                                                      else throwError $ ProtocolError (SRPError "Client M2 doesn't match with server M2")
                                                     ))                                                                                                              (WidgetState {status: Spinner, color: Black, message: "Validate user"} page)
  userInfoReferences                      <- runStep ( extractUserInfoReference loginStep2Result.masterKey 
                                                       =<< 
                                                      (importCryptoKeyAesGCM (prepareLoginResult.p # toArrayBuffer) # liftAff)
                                                     )                                                                                                               (WidgetState {status: Spinner, color: Black, message: "Validate user"} page)
  let stateUpdate = { masterKey:          Just loginStep2Result.masterKey 
                    , userInfoReferences: Just userInfoReferences
                    , username:           Just cred.username
                    , password:           Just cred.password
                    , s:                  Just loginStep1Result.s
                    , c:                  Just prepareLoginResult.c
                    , p:                  Just prepareLoginResult.p
                    }

  res                                     <- loadHomePageSteps (merge stateUpdate (state {proxy = proxy''})) page fragmentState

  pure $ res

loadHomePageSteps :: AppState -> Page -> Fragment.FragmentState -> ExceptT AppError (Widget HTML) OperationState

loadHomePageSteps state@{hash: hashFunc, proxy, srpConf, c: Just c, p: Just p, masterKey: Just (Tuple _ masterKeyEncodingVersion), userInfoReferences: Just userInfoReferences} page fragmentState = do
  let connectionState = {proxy, hashFunc, srpConf, c, p}

  ProxyResponse proxy'  userInfo <- runStep (getUserInfo connectionState                                userInfoReferences (masterKeyEncodingVersion)) (WidgetState {status: Spinner, color: Black, message: "Get user info"} page)
  ProxyResponse proxy'' index    <- runStep (getIndex    connectionState{ proxy = proxy'} (unwrap userInfo).indexReference                           ) (WidgetState {status: Spinner, color: Black, message: "Get index"    } page)
  donationLevel                  <- runStep (computeDonationLevel index userInfo # liftEffect                                                        ) (WidgetState {status: Spinner, color: Black, message: "Get index"    } page)
  case (unwrap (unwrap userInfo).userPreferences).automaticLock of
    Right n -> liftEffect (activateTimer n)
    Left  _ -> pure unit

  let cardViewState = case fragmentState of
                        Fragment.AddCard card -> CardForm (NewCardFromFragment card)
                        _                     -> NoCard

  pure $ Tuple 
    (state {proxy = proxy'', index = Just index, userInfo = Just userInfo, donationLevel = Just donationLevel})
    (WidgetState 
      hiddenOverlayInfo
      case donationLevel of
        DonationWarning -> (Donation donationLevel)
        _               -> (Main emptyMainPageWidgetState { index = index, cardManagerState = cardManagerInitialState { cardViewState = cardViewState }, donationLevel = donationLevel })
    )

loadHomePageSteps _ _ _ = do
  throwError (InvalidStateError $ CorruptedState "")

type MaxPinAttemptsReached = Boolean

handlePinResult :: AppState -> Page -> OverlayColor -> Either AppError OperationState -> Widget HTML (Tuple Page (Either AppError OperationState))
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