module Data.Operation where

import Control.Alt ((<|>), class Alt)
import Control.Applicative (pure, class Applicative)
import Control.Bind ((>>=), class Bind, bind, discard)
import Data.Either (Either(..))
import Data.Functor (class Functor, (<$>))
import Data.List (List(..))

data OperationStep a b m = IntermediateStep (a -> m a) (m a) | LastStep (a -> m b) (m b)

extractResult :: forall a b m. Functor m => a -> OperationStep a b m -> Either (m b) (m a)
extractResult a (IntermediateStep f _) = Right (f a)
extractResult a (LastStep f _) = Left (f a)

runOperation :: forall a b m. Bind m => Applicative m => Alt m => a -> List (OperationStep a b m) -> m (Either b a)
runOperation iv list = go (pure (Right iv)) list
  where
    go :: m (Either b a) -> List (OperationStep a b m) -> m (Either b a)
    go result Nil               = result
    go result (Cons step steps) = do
      res <- result
      case res of
        Left b -> pure res
        Right a -> do
          res' <- case step of
                    IntermediateStep f pl -> Right <$> ((f a) <|> pl)
                    LastStep f pl -> Left <$> ((f a) <|> pl)
          go (pure res') steps 
