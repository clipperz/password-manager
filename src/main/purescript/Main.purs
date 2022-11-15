module Main
  ( main
  -- , registration
  -- , share
  )
  where

import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import OperationalWidgets.App (app, Page(..), SharedCardReference)

main :: Effect Unit
main = runWidgetInDom "app" (app (Loading (Just Login)))

-- registration :: Effect Unit
-- registration = runWidgetInDom "app" (app Signup)

-- share :: String -> Effect Unit
-- share token = runWidgetInDom "app" (app (Share (Just token)))

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
