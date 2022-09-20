module Functions.State where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad (class Monad)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.State (StateT(..), runStateT, modify_)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor (class Functor, (<$>))
import Data.Tuple (Tuple(..))

makeStateT :: forall m a s. Functor m => m a -> StateT s m a
makeStateT value = StateT (\s -> ((\r -> Tuple r s) <$> value))

extractExceptT :: forall a s e m. Monad m => StateT s (ExceptT e m) a -> s -> StateT s m (Either e a)
extractExceptT monad state = do
  result <- makeStateT $ runExceptT $ runStateT monad state
  case result of
    Left err -> do
      modify_ (\_ -> state)
      makeStateT $ pure $ Left err
    Right (Tuple value newState) -> do
      modify_ (\_ -> newState)
      makeStateT $ pure $ Right value
