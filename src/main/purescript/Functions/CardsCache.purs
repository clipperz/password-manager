module Functions.CardsCache where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..))
import Data.Function (($))
import Data.HexString (HexString)
import Data.Map.Internal (delete, insert, lookup)
import Data.Maybe (Maybe)
import Data.Unit (Unit)
import DataModel.AppState (AppError)
import DataModel.Card (Card)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.JSState (getAppState, saveAppState)

getCardFromCache :: HexString -> ExceptT AppError Aff (Maybe Card)
getCardFromCache reference = do
  { cardsCache } <- ExceptT $ liftEffect getAppState
  pure $ lookup reference cardsCache

addCardToCache :: HexString -> Card -> ExceptT AppError Aff Unit
addCardToCache reference card = do
  state@{ cardsCache } <- ExceptT $ liftEffect getAppState
  liftAff (liftEffect $ saveAppState (state { cardsCache = insert reference card cardsCache}))

removeCardFromCache :: HexString -> ExceptT AppError Aff Unit
removeCardFromCache reference = do
  state@{ cardsCache } <- ExceptT $ liftEffect getAppState
  liftAff (liftEffect $ saveAppState (state { cardsCache = delete reference cardsCache}))