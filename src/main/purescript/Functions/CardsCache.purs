module Functions.CardsCache where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..))
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString)
import Data.Map.Internal (delete, insert, lookup)
import Data.Maybe (Maybe)
import Data.Unit (Unit)
import DataModel.AppState (AppError)
import DataModel.Card (Card)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.JSState (getAppState, modifyAppState)

getCardFromCache :: HexString -> ExceptT AppError Aff (Maybe Card)
getCardFromCache reference = do
  { cardsCache } <- ExceptT $ liftEffect getAppState
  pure $ lookup reference cardsCache

addCardToCache :: HexString -> Card -> ExceptT AppError Aff Unit
addCardToCache reference card = do
  state@{ cardsCache } <- ExceptT $ liftEffect getAppState
  ExceptT $ Right <$> (liftEffect $ modifyAppState (state { cardsCache = insert reference card cardsCache}))

removeCardFromCache :: HexString -> ExceptT AppError Aff Unit
removeCardFromCache reference = do
  state@{ cardsCache } <- ExceptT $ liftEffect getAppState
  ExceptT $ Right <$> (liftEffect $ modifyAppState (state { cardsCache = delete reference cardsCache}))