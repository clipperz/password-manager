module Main
  ( main
  , registration
  , share
  , test
  )
  where

import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import OperationalWidgets.App (app, Page(..), SharedCardReference)


import Data.Function (($))
import Concur.Core.FRP (Signal, demand, loopW, always, fireOnce, hold, dyn, loopS, step)
import Concur.Core.Types (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text, ul, li, h1, h3, footer, header, label, input, span, a, button, div_, footer_, p_, ul_, li_, a_, h1_, h3_, header_, span_)
import Concur.React.Props as Props
import Control.Bind (bind, (>>=), discard)
import Control.Monad.Rec.Class (forever)
import Data.Functor (map, (<$), void, (<$>))
import Control.Applicative (pure)
import Data.Unit (unit, Unit)

main :: Effect Unit
main = runWidgetInDom "app" (app Login)

registration :: Effect Unit
registration = runWidgetInDom "app" (app Signup)

share :: String -> Effect Unit
share token = runWidgetInDom "app" (app (Share (Just token)))

-- loopW      :: forall   a m. Monad m =>                                     a -> (a -> m a        )                         -> SignalT m a            -- Create a signal which repeatedly invokes a widget function for values, looping in the prev value.
-- loopS      :: forall   a m. Monad m =>                                     a -> (a -> SignalT m a)                         -> SignalT m a            -- Loop a signal so that the return value is passed to the beginning again.
-- step       :: forall   a m.                                                a -> m (SignalT m a)                            -> SignalT m a            -- Construct a signal from an initial value, and a step widget
-- stateLoopS :: forall s a m. Monad m => Alternative m =>                    s -> (s -> SignalT m (Either s a))              -> SignalT m (Maybe a)    -- 
-- foldp      :: forall b a m. Functor m =>                       (a -> b -> a) -> a      -> SignalT m b                      -> SignalT m a            -- Loop a signal so that the return value is passed to the beginning again. Folding signals. Similar to how signals used to work in Elm. This can be used to implement simple stateful Signals. e.g. counter = fold (\n _ -> n+1) 0 clicks
-- always     :: forall   a m. Monad m => Alternative m =>                    a                                               -> SignalT m a            -- A constant signal
-- hold       :: forall   a m. Monad m =>                                     a -> m a                                        -> SignalT m a            -- Create a signal which repeatedly invokes a widget for values. E.g. signal False checkbox will return a signal which reflects the current value of the checkbox.
-- poll       :: forall   a m. Monad m =>                                          SignalT m (m a)                            -> m (SignalT m a)        -- Construct a signal by polling a signal with a nested widget for values
-- update     :: forall   a m.                                                     SignalT m a                                -> m (SignalT m a)        -- Update signal to a new value
-- display    :: forall     m.                                                     m (SignalT m Unit)                         -> SignalT m Unit         -- Display a widget which returns a continuation
-- dyn        :: forall b a m. Monad m =>                                          SignalT m a                                -> m b                    -- Consume a closed signal to make a widget
-- oneShot    :: forall   a m. Monad m =>                                          SignalT m (Maybe a)                        -> m a                    -- Run a signal once and return its value
-- demand     :: forall   a m. Monad m =>                                          SignalT m (Maybe a)                        -> m a
-- demandLoop :: forall s a m. Monad m => Alternative m =>                    s -> (s -> SignalT m (Either s a))              -> m a
-- fireOnce   :: forall   a m. Monad m => Plus m        =>                  m a                                               -> SignalT m (Maybe a)    -- Fires a widget once then stop. This will reflow when a parent signal reflows Starts as Nothing. Then switches to Just returnVal after the Widget is done
-- debounce   :: forall   a m. Monad m => Alt m => MonadAff m =>              Number -> a -> (a -> m a)                       -> SignalT m a
-- justWait   :: forall b a m. Monad m => Alternative m =>                    b -> SignalT m (Maybe a)  -> (a -> SignalT m b) -> SignalT m b            -- Wait until we get a Just value from a signal
-- justEffect :: forall b a m. Monad m => Alternative m => MonadEffect m =>   b -> Effect a             -> (a -> SignalT m b) -> SignalT m b

-- map      :: forall f a b.   Functor f     =>                  (a ->   b) -> f a ->    f b
-- traverse :: forall t a b m. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)

test :: Effect Unit
test = runWidgetInDom "app" test'
{-
test = runWidgetInDom "app" (dyn $ { - loopS "" (\v -> - }
  div_ [Props.className "container"] do
    div_ [Props.className "container_1", Props.style {backgroundColor: "green" }] do
      -- loopW :: forall a m. Monad m => a -> (a -> m a) -> SignalT m a
      -- Create a signal which repeatedly invokes a widget function for values, looping in the prev value.
      _ <- loopW "" (\v' ->  div [Props.className "field"] [
      -- _ <- hold $ div [Props.className "field"] [
            label [Props.htmlFor "username"] [text "Username"],
            (Props.unsafeTargetValue) <$> input [Props._type "text", Props._id "username", Props.placeholder "username", Props.value v', Props.disabled false, Props.onChange]
      ])
      pure unit
)
-}

test' :: forall a. Widget HTML a
test' = do
  -- dyn $ do
  --   div_ [Props.className "container"] do
  --     div_ [Props.className "container_1", Props.style {backgroundColor: "green" }] do
  --       -- loopW :: forall a m. Monad m => a -> (a -> m a) -> SignalT m a
  --       -- Create a signal which repeatedly invokes a widget function for values, looping in the prev value.
  --       _ <- loopW "" (\v' ->  div [Props.className "field"] [
  --       -- _ <- hold $ div [Props.className "field"] [
  --             label [Props.htmlFor "username"] [text "Username"],
  --             (Props.unsafeTargetValue) <$> input [Props._type "text", Props._id "username", Props.placeholder "username", Props.value v', Props.disabled false, Props.onChange]
  --       ])
  --       pure unit

  div [Props.className "container"] [
    div [Props.className "container_1", Props.style {backgroundColor: "green" }] [
      -- loopW :: forall a m. Monad m => a -> (a -> m a) -> SignalT m a
      -- Create a signal which repeatedly invokes a widget function for values, looping in the prev value.
      step "" $ div [Props.className "field"] [
      -- _ <- hold $ div [Props.className "field"] [
            label [Props.htmlFor "username"] [text "Username"],
            (Props.unsafeTargetValue) <$> input [Props._type "text", Props._id "username", Props.placeholder "username", Props.value v', Props.disabled false, Props.onChange]
      ]
    ]
  ]