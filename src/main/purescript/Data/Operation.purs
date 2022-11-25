module Data.Operation where

import Control.Alt ((<|>), class Alt)
import Control.Applicative (pure, class Applicative)
import Control.Bind ((>>=), class Bind, bind, discard)
import Data.Either (Either(..))
import Data.Functor (class Functor, (<$>))
import Data.List (List(..))

data OperationStep a b m = IntermediateStep (a -> m a) | LastStep (a -> m b)

extractResult :: forall a b m. Functor m => a -> OperationStep a b m -> Either (m b) (m a)
extractResult a (IntermediateStep f) = Right (f a)
extractResult a (LastStep f) = Left (f a)

runOperation :: forall a b m. Bind m => Applicative m => a -> List (OperationStep a b m) -> Either (m b) (m a)
runOperation iv list = go (Right (pure iv)) list
  where
    go :: Either (m b) (m a) -> List (OperationStep a b m) -> Either (m b) (m a)
    go result    Nil               = result
    go (Left b)  _                 = Left b
    go (Right a) (Cons step steps) = 
      let res = case step of
                  IntermediateStep f -> Right (bind a f)
                  LastStep f -> Left (bind a f)
      in go res steps 
