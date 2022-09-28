module Functions.JSState where

import Control.Bind ((>>=), bind)
import Control.Applicative (pure)
import Control.Monad.Except (Except, runExcept)
import Control.Semigroupoid ((>>>))
import Data.Argonaut.Core (stringify, Json)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List.NonEmpty (NonEmptyList)
import Data.Show (show)
import Data.Unit (Unit, unit)
import DataModel.AppState (AppState, AppError(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign, ForeignError)

foreign import getJsonState :: Unit -> Effect String

getAppState :: Effect (Either AppError AppState)
getAppState = do
  json <- jsonParser <$> (getJsonState unit)
  let appstate = decodeJson <$> json 
  case appstate of
    Left s -> pure $ Left $ InvalidStateError s
    Right (Left s) -> pure $ Left $ InvalidStateError $ show s
    Right (Right a) -> pure $ Right a

foreign import updateJsonState :: String -> Effect Unit

updateAppState :: AppState -> Aff Unit
updateAppState = encodeJson >>> stringify >>> updateJsonState >>> liftEffect
