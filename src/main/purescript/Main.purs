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
import Concur.Core.FRP (Signal, demand, loopW, always, fireOnce, hold, dyn, loopS)
import Concur.React.DOM (div, text, ul, li, h1, h3, footer, header, label, input, span, a, button, div_, footer_, p_, ul_, li_, a_, h1_, h3_, header_, span_)
import Concur.React.Props as Props
import Control.Bind (bind, (>>=), discard)
import Data.Functor (map, (<$), void, (<$>))
import Control.Applicative (pure)
import Data.Unit (unit, Unit)

main :: Effect Unit
main = runWidgetInDom "app" (app Login)

registration :: Effect Unit
registration = runWidgetInDom "app" (app Signup)

share :: String -> Effect Unit
share token = runWidgetInDom "app" (app (Share (Just token)))


test :: Effect Unit
test = runWidgetInDom "app" (dyn $
  div_ [Props.className "container"] do
    div_ [Props.className "container_1", Props.style {backgroundColor: "green" }] do
      _ <- loopW "" (\v ->  div [Props.className "field"] [
            label [Props.htmlFor "username"] [text "Username"]
          , (Props.unsafeTargetValue) <$> input [
              Props._type "text"
            , Props._id "username"
            , Props.placeholder "username"
            , Props.value v
            , Props.disabled false
            , Props.onChange
            ]
      ])
      pure unit
)
