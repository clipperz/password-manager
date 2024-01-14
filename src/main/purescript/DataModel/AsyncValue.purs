module DataModel.AsyncValue where

import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Show (class Show, show)

data AsyncValue a = Loading (Maybe a) | Done a
instance showAsyncValue :: (Show a) => Show (AsyncValue a) where
    show (Loading p) = "loading[" <> show p <> "]"
    show (Done p) = "done[" <> show p <> "]"

arrayFromAsyncValue :: forall a. AsyncValue a -> Array a
arrayFromAsyncValue (Loading _) = []
arrayFromAsyncValue (Done a) = [a]

toLoading :: forall a. AsyncValue a -> AsyncValue a
toLoading async =
  case async of
    Done value    -> Loading (Just value)
    Loading value -> Loading value