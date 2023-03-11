module Views.Components
  ( ClassName(..)
  , Enabled(..)
  , InputType(..)
  , Label(..)
  , Placeholder(..)
  , dynamicWrapper
  , verySimpleInputWidget
  )
  where

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Concur.Core (Widget)
import Concur.Core.FRP (Signal, loopW, demand, debounce, loopS, display, step)
import Concur.React (HTML)
import Concur.React.DOM (text, textarea, input, label, div', div, button, ul, li, span)
import Concur.React.Props as Props

newtype ClassName = ClassName String
derive instance newtypeCharacterSet :: Newtype ClassName _
newtype Label = Label String
newtype Enabled = Enabled Boolean
newtype Placeholder = Placeholder String
newtype InputType = InputType String

verySimpleInputWidget :: InputType -> ClassName -> Label -> Enabled -> Placeholder -> (String -> Maybe ClassName) -> String -> Widget HTML String
verySimpleInputWidget (InputType t) (ClassName className) (Label lbl) (Enabled enabled) (Placeholder placeholder) dynamicClassName value = do
  let c = dynamicClassName value :: Maybe ClassName
  let c' = unwrap <$> c :: Maybe String
  -- label [Props.classList [Just className, unwrap <$> (dynamicClassName value)]] [
  label [Props.classList [Just className, c']] [
    span [Props.className "label"] [text lbl]
  , (Props.unsafeTargetValue) <$> input [
      Props._type t
    , Props.placeholder placeholder
    , Props.value value
    , Props.disabled (not enabled)
    , Props.onChange
    ]
  ]

dynamicWrapper :: forall a. String -> Widget HTML a -> Widget HTML a
dynamicWrapper content elem = div [Props.className "dynamicWrap", Props.unsafeMkProp "replicatedvalue" content] [ elem ]