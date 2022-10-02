module Functions.CardsCache where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..))
import Data.HexString (HexString)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Map.Internal (lookup, insert)
import Data.Maybe (Maybe)
import Data.Unit (Unit)
import DataModel.Card (Card)
import DataModel.AppState (AppError)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.JSState (getAppState, updateAppState)

getCardFromCache :: HexString -> ExceptT AppError Aff (Maybe Card)
getCardFromCache reference = do
  { cardsCache } <- ExceptT $ liftEffect getAppState
  pure $ lookup reference cardsCache

addCardToCache :: HexString -> Card -> ExceptT AppError Aff Unit
addCardToCache refence card = do
  state@{ cardsCache } <- ExceptT $ liftEffect getAppState
  ExceptT $ Right <$> updateAppState (state { cardsCache = insert refence card cardsCache})