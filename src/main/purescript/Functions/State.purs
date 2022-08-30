module Functions.State where

import Control.Monad.State (StateT(..))
import Data.Functor (class Functor, (<$>))
import Data.Tuple (Tuple(..))

makeStateT :: forall m a s. Functor m => m a -> StateT s m a
makeStateT value = StateT (\s -> ((\r -> Tuple r s) <$> value))
