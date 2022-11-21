module Main
  ( main
  , registration
  , share
  -- , test
  )
  where

import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import OperationalWidgets.App (app, Page(..), SharedCardReference)
import Concur.Core.FRP (loopS, demand, fireOnce, dyn)
-- import Concur.React.DOM (text)
import Views.SimpleWebComponents (simpleButton, simpleTextInputWidget, dragAndDropListSignal)
import Data.Tuple (Tuple(..))
import Data.Function (($))
-- import Control.Bind (bind, discard)
import Effect.Class.Console (log)
import Data.Show (show)
import Data.Functor ((<$>))
import Data.Tuple (fst)

import Debug (traceM)

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
main = runWidgetInDom "app" (app (Loading (Just Login)))

registration :: Effect Unit
registration = runWidgetInDom "app" (app Signup)

share :: String -> Effect Unit
share token = runWidgetInDom "app" (app (Share (Just token)))

-- main :: Effect Unit
-- main = runWidgetInDom "app" $ do
--   let initialValues = [
--     Tuple "" (simpleTextInputWidget "input1" (text "Input 1") "")
--   , Tuple "" (simpleTextInputWidget "input2" (text "Input 2") "")
--   , Tuple "" (simpleTextInputWidget "input3" (text "Input 3") "")
--   , Tuple "" (simpleTextInputWidget "input4" (text "Input 4") "")  
--   , Tuple "" (simpleTextInputWidget "input5" (text "Input 5") "")
--   ]
--   res <- demand $ do
--     results <- dragAndDropListSignal initialValues
--     fireOnce $ simpleButton "Exit" false results
--   log $ show $ fst <$> res

-- import Data.Function (($))
-- import Concur.Core.FRP (demand, hold, loopS, dyn)
-- import Concur.React.DOM (div_, text, div)
-- import Views.LoginView ( loginViewSignal )
-- import Views.SimpleWebComponents (simplePasswordSignal, simplePasswordInputWidget)
-- import React.DOM as D
-- import Concur.Core (mkLeafWidget, mkNodeWidget)
-- import Concur.Core.Props (Props, mkProp)
-- import Control.ShiftMap (class ShiftMap, shiftMap)
-- import Data.Functor (map)
-- import Control.Semigroupoid ((<<<))
-- import Control.Bind (bind, discard)
-- import Concur.React.Props as Props
-- import Control.Applicative (pure)
-- import Debug (traceM)

-- main :: Effect Unit
-- main = runWidgetInDom "app" $ div [] [dyn $ do
--   traceM "01"
--   r <- loopS "initial" $ \i -> div_ [Props.className "general"] $ div_ [Props.className "ciao"] do
--     traceM "1"
--     _ <- div_ [Props.className "pwd0"] $ simplePasswordSignal ""
--     traceM "1.5"
--     _ <- div_ [Props.className "pwd1"] $ simplePasswordSignal ""
--     res <- div_ [Props.className "pwd2"] $ simplePasswordSignal ""
--     traceM "2"
--     pure res
--   traceM "02"
--   pure r
-- ]
-- -- main = runWidgetInDom "app" $ div [] [div_ [Props.className "ciao"] (do
-- --   _ <- simplePasswordInputWidget "password" (text "password") ""
-- --   text "ok")]

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

-- test :: Effect Unit
-- test = runWidgetInDom "app" test'
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

-- test' :: forall a. Widget HTML a
-- test' = do
--   -- dyn $ do
--   --   div_ [Props.className "container"] do
--   --     div_ [Props.className "container_1", Props.style {backgroundColor: "green" }] do
--   --       -- loopW :: forall a m. Monad m => a -> (a -> m a) -> SignalT m a
--   --       -- Create a signal which repeatedly invokes a widget function for values, looping in the prev value.
--   --       _ <- loopW "" (\v' ->  div [Props.className "field"] [
--   --       -- _ <- hold $ div [Props.className "field"] [
--   --             label [Props.htmlFor "username"] [text "Username"],
--   --             (Props.unsafeTargetValue) <$> input [Props._type "text", Props._id "username", Props.placeholder "username", Props.value v', Props.disabled false, Props.onChange]
--   --       ])
--   --       pure unit

--   div [Props.className "container"] [
--     div [Props.className "container_1", Props.style {backgroundColor: "green" }] [
--       -- loopW :: forall a m. Monad m => a -> (a -> m a) -> SignalT m a
--       -- Create a signal which repeatedly invokes a widget function for values, looping in the prev value.
--       step "" $ div [Props.className "field"] [
--       -- _ <- hold $ div [Props.className "field"] [
--             label [Props.htmlFor "username"] [text "Username"],
--             (Props.unsafeTargetValue) <$> input [Props._type "text", Props._id "username", Props.placeholder "username", Props.value v', Props.disabled false, Props.onChange]
--       ]
--     ]
--   ]

-- -- viewAdapter
--   -- :: forall ps vs res
--   -- .  (ps -> vs -> res)
--   -- -> (ps -> vs -> Array res)
-- viewAdapter f = \ps vs -> [f ps vs]

-- -- el
--   -- :: forall m a p v
--   -- .  ShiftMap (Widget (Array v)) m
--   -- => (Array p -> Array v -> v)
--   -- -> Array (Props p a)
--   -- -> m a
--   -- -> m a
-- -- el f props = cdel (viewAdapter f) props
-- el f' props = shiftMap (\f w -> mkNodeWidget (\h v -> ((viewAdapter f') (map (mkProp h <<< map f) props) v)) w)

-- -- mkNodeWidget :: forall v a. ((a -> Effect Unit) -> v -> v) -> Widget v a -> Widget v a
-- -- mkProp :: forall a p. (a -> Effect Unit) -> Props p a -> p

-- -- cdel
-- --   :: forall f p v m a
-- --   .  ShiftMap (Widget v) m
-- --   => Functor f
-- --   => (f p -> v -> v)
-- --   -> f (Props p a)
-- --   -> m a
-- --   -> m a
-- cdel e props = shiftMap (\f w -> mkNodeWidget (\h v -> (e (map (mkProp h <<< map f) props) v)) w)

-- --     shiftMap :: forall a. (forall b. (a -> b) -> s b -> s b) -> t a -> t a
-- --     shiftMap :: forall a. (forall b. (a -> b) -> Widget HTML b -> Widget HTML b) -> Widget HTML a -> Widget HTML a
-- -- instance widgetShiftMap :: ShiftMap (Widget v) (Widget v) where
--   -- shiftMap f = f identity

--   -- identity :: a -> Identity a
