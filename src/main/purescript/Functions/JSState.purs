module Functions.JSState where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad (class Monad)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Semigroupoid ((>>>))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.BigInt (BigInt)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (Unit, unit)
import DataModel.AppState (AppError(..), AppState, HashState, InvalidStateError(..), KDFState)
import DataModel.AsyncValue (AsyncValue)
import DataModel.Card (Card)
import DataModel.Proxy (Proxy)
import DataModel.User (MasterKey, UserInfoReferences, UserPreferences)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Prim.Row (class Nub, class Union)
import Record (merge)

foreign import getJsonState :: Unit -> Effect String

getAppState :: Effect (Either AppError AppState)
getAppState = do
  json <- jsonParser <$> (getJsonState unit)
  let appstate = decodeJson <$> json 
  case appstate of
    Left _ -> pure $ Left $ InvalidStateError $ CorruptedState "The state currently saved is not a valid JSON string"
    Right (Left s) -> pure $ Left $ InvalidStateError $ CorruptedState $ "The state currently saved is not JSON string representing a state: " <> show s
    Right (Right a) -> pure $ Right a

foreign import updateJsonState :: String -> Effect Unit

saveAppState :: AppState -> Effect Unit
saveAppState = encodeJson >>> stringify >>> updateJsonState

updateAppState :: forall m r1 r3.
  Monad m => MonadEffect m => Union r1
                                      ( c :: Maybe HexString
                                      , cardsCache :: Map HexString Card
                                      , currentChallenge :: Maybe
                                                              { cost :: Int
                                                              , toll :: HexString
                                                              }
                                      , hash :: HashState
                                      , p :: Maybe HexString
                                      , s :: Maybe HexString
                                      , password :: Maybe String
                                      , proxy :: Proxy
                                      , sessionKey :: Maybe HexString
                                      , srpInfo :: { group :: { g :: BigInt
                                                              , nn :: BigInt
                                                              }
                                                   , k :: BigInt
                                                   , hash :: HashState
                                                   , kdf :: KDFState
                                                   }
                                      , toll :: AsyncValue HexString
                                      , masterKey :: Maybe MasterKey
                                      , userInfoReferences :: Maybe UserInfoReferences
                                      , userPreferences :: Maybe UserPreferences
                                      , username :: Maybe String
                                      )
                                      r3
                           => Nub   r3
                                      ( c :: Maybe HexString
                                      , cardsCache :: Map HexString Card
                                      , currentChallenge :: Maybe
                                                              { cost :: Int
                                                              , toll :: HexString
                                                              }
                                      , hash :: HashState
                                      , p :: Maybe HexString
                                      , s :: Maybe HexString
                                      , password :: Maybe String
                                      , proxy :: Proxy
                                      , sessionKey :: Maybe HexString
                                      , srpInfo :: { group :: { g :: BigInt
                                                              , nn :: BigInt
                                                              }
                                                    , k :: BigInt
                                                    , hash :: HashState
                                                    , kdf :: KDFState
                                                    }
                                      , toll :: AsyncValue HexString
                                      , masterKey :: Maybe (MasterKey)
                                      , userInfoReferences :: Maybe UserInfoReferences
                                      , userPreferences :: Maybe UserPreferences
                                      , username :: Maybe String
                                      )
                                      => Record r1 -> m (Either AppError Unit)
updateAppState partialState = runExceptT $ do
  stateToUpdate <- ExceptT $ liftEffect $ getAppState
  ExceptT $ Right <$> (liftEffect $ saveAppState (merge partialState stateToUpdate))
