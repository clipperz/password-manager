module WidgetManagers.HomePageManager where

import Affjax.ResponseFormat as RF
import Concur.Core (Widget)
import Concur.Core.FRP (loopW, demandLoop)
import Concur.React (HTML)
import Concur.React.DOM (div', text)
import Control.Applicative (pure)
import Control.Bind (discard, bind)
import Control.Monad.Except.Trans (ExceptT(..), withExceptT, runExceptT, except)
import Control.Monad.State (StateT, get, runStateT, modify_, mapStateT)
import Control.Semigroupoid ((>>>))
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (encrypt, decrypt, raw, unwrapKey, CryptoKey)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Function (($), flip)
import Data.Functor ((<$>), void)
import Data.HexString (HexString, toArrayBuffer, fromArrayBuffer, splitHexInHalf)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import DataModel.AppState (AppState, AppError(..))
import DataModel.Card (Card)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (CardReference(..), Index, IndexReference)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception as EX
import Functions.EncodeDecode (decryptArrayBuffer)
import Functions.Communication.BackendCommunication (manageGenericRequest)
import Functions.Communication.Blobs (getDecryptedBlob, getDecryptedBlob')
import Functions.Communication.Cards (getIndex)
import Functions.JSState (getAppState)
import Functions.State (makeStateT, extractExceptT, computeInitialState)
import Widgets.HomePage (HomePageAction(..), CardsViewAction(..), homePage)

homePageManager :: IndexReference -> Widget HTML Unit
homePageManager indexReference = do
  eitherIndex <- liftAff $ runExceptT $ getIndex indexReference
  case eitherIndex of
    Right index -> void $ homePage index Nothing
    Left err -> div' [text ("Couldn't load cards list: " <> (show err))]
  pure unit
  -- currentState <- get
  -- case currentState of
  --   { p: Just p, sessionKey: _, c: _, proxy: _, toll: _ } -> do
  --     eitherIndex <- mapStateT (\e -> liftAff e) $ getIndex p indexReference
  --     newCurrentState <- get
  --     case eitherIndex of
  --       Right index -> do
  --         newState <- makeStateT $ demandLoop (Tuple (CardsViewAction (ShowCard Nothing)) newCurrentState) (\t -> loopW (Left t) (\loop ->
  --           case loop of
  --             Left (Tuple hva s)->
  --               case hva of 
  --                 CardsViewAction (ShowCard Nothing) -> (\r -> Left $ Tuple r s) <$> homePage index Nothing
  --                 CardsViewAction (ShowCard (Just ref)) -> do
  --                   Tuple either newState <- liftAff $ runStateT (getCard ref) s
  --                   case either of
  --                     Left err -> do
  --                       _ <- log $ show err
  --                       (\r -> Left $ Tuple r newState) <$> homePage index Nothing
  --                     Right card -> (\r -> Left $ Tuple r newState) <$> homePage index (Just card)
  --                 CardsViewAction (ActOnCard _ a) -> do
  --                   _ <- log $ show a
  --                   (\r -> Left $ Tuple r s) <$> homePage index Nothing
  --                 LogoutAction -> do
  --                   _ <- liftAff $ runExceptT $ runStateT (manageGenericRequest "logout" POST Nothing RF.string) s
  --                   initialState <- liftEffect computeInitialState
  --                   pure $ Right initialState
  --             Right state -> do
  --               pure $ Right state
  --         ))
  --         modify_ (\_ -> newState)
  --         pure unit
  --       Left err -> do
  --         _ <- log $ show err
  --         pure unit
  --   _ -> do
  --     log $ show currentState
  --     makeStateT $ pure $ unit -- TODO: manage error
