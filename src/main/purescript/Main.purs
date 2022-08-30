module Main where

import Prelude

import Concur.React.Run (runWidgetInDom)
import Data.Unit (Unit)
import Effect (Effect)
-- import Affjax.RequestHeader
import Data.HTTP.Method (Method(..))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Either(Either(..))
import Affjax.ResponseFormat as RF
import WidgetManagers.App (app)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log)
import Effect.Exception (Error(..))
import Control.Monad.State.Trans (StateT(..), runStateT, modify_)
import RestBackendCommunication (doGenericRequest)

type OurState = { requests :: Int, errors :: Int }

-- doGenericRequest :: forall a. Url -> Method -> Array RE.RequestHeader -> Maybe RequestBody -> RF.ResponseFormat a -> Aff (Either ProtocolError (AXW.Response a))
-- 

makeStateT :: forall m a s. Functor m => m a -> StateT s m a
makeStateT value = StateT (\s -> ((\r -> Tuple r s) <$> value))

ourStateFunction :: StateT OurState Aff String
ourStateFunction = do
  -- res <- liftEffect $ liftAff $ doGenericRequest "http://www.randomnumberapi.com/api/v1.0/random" GET [] Nothing RF.string 
  res <- makeStateT $ doGenericRequest "http://www.randomnumberapi.com/api/v1.0/random" GET [] Nothing RF.string
  case res of
    Left err -> do
      modify_ (\{requests, errors} -> { requests: requests+1, errors: errors+1})
      pure $ show err
    Right actualRes -> do
      modify_ (\{requests, errors} -> { requests: requests+1, errors })
      pure actualRes.body

main :: Effect Unit
main = do
  runWidgetInDom "app" app
  -- let initialState = { requests: 0, errors: 0 }
  -- log $ "State: " <> show initialState
  -- log "Do request"
  -- runAff_ callback (runStateT ourStateFunction initialState)

callback :: Either Error (Tuple String OurState) -> Effect Unit
callback (Left err) = log $ show err
callback (Right (Tuple val st)) = do
  log $ "Value: " <> val
  log $ "State: " <> show st

-- crazyFunction :: State Int String
-- crazyFunction = do
--   value1 <- modify (_ + 1)
--   modify_ (_ + (length $ show value1))
--   gets show

-- main :: Effect Unit
-- main =
--   case (runState crazyFunction 0) of
--     Tuple theString theInt -> do
--       log $ "theString was: " <> theString  -- "2"
--       log $ "theInt was: " <> show theInt   --  2

-- runState :: forall s a. StateT s Identity a -> s -> Tuple a s
-- runState stateT initialState =
--   let (Identity tuple) = runStateT stateT initialState
--   in tuple

-- runStateT :: forall s m a. StateT s m a -> s -> m Tuple a s
-- runStateT (StateT f) initialState = f initialState