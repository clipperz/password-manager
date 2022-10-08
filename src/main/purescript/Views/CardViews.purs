module Views.CardViews where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, loopS, loopW, demand, fireOnce)
import Concur.React (HTML)
import Concur.React.DOM (div, h3, li', p, text, ul)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Semigroupoid ((<<<))
import Data.Array (snoc)
import Data.DateTime.Instant (unInstant)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (ceil)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show, class Show)
import Data.Traversable (sequence)
import DataModel.Card (CardField(..), CardValues(..), Card(..), emptyCardField)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Functions.Clipboard (copyToClipboard)
import Views.SimpleWebComponents (simpleButton, simpleTextInputWidget, simpleCheckboxSignal)

-- -----------------------------------

data CardAction = Edit Card | Clone Card | Archive Card | Delete Card
instance showCardAction :: Show CardAction where
  show (Edit _)    = "Edit"
  show (Clone _)   = "Clone"
  show (Archive _) = "Archive"
  show (Delete _)  = "Delete"

-- -----------------------------------

cardView :: Card -> Widget HTML CardAction
cardView c@(Card_v1 r) = div [] [
    cardActions c
  , cardContent r.content
]

cardActions :: Card -> Widget HTML CardAction
cardActions c = div [] [
    simpleButton (show (Edit c))    false (Edit c)
  , simpleButton (show (Clone c))   false (Clone c)
  , simpleButton (show (Archive c)) false (Archive c)
  , simpleButton (show (Delete c))  false (Delete c)
]

cardContent :: forall a. CardValues -> Widget HTML a
cardContent (CardValues_v1 {title: t, tags: ts, fields: fs, notes: n}) = div [Props.className "card_content"] [
  h3  [Props.className "card_title"]  [text t]
, ul  [Props.className "card_tags"]   $ (\s -> li' [text s]) <$> ts
, div [Props.className "card_fields"] $ cardField <$> fs
, div [Props.className "card_notes"]  [text n]
]

cardField :: forall a. CardField -> Widget HTML a
cardField (CardField_v1 {name, value, locked}) = div [] [
  text name
, p ((if locked then [Props.className "PASSWORD"] else []) <> [(\_ -> copyToClipboard value) <$> Props.onClick]) [text value]
] --TODO add class based on content for urls and emails

-----------------------

createCardView :: Card -> Widget HTML Card
createCardView card = do
  (Card_v1 { content, timestamp: _ }) <- div [] [demand formSignal] 
  timestamp' <- liftEffect $ (ceil <<< unwrap <<< unInstant) <$> now
  pure $ Card_v1 { content: content, timestamp: timestamp' }

  where 
    cardFieldSignal :: CardField -> Signal HTML CardField
    cardFieldSignal field = loopS field $ \(CardField_v1 { name, value, locked }) -> do
      name' :: String <- loopW name (simpleTextInputWidget "name" (text "Name"))
      value' :: String <- loopW value (simpleTextInputWidget "value" (text "Value"))
      locked' :: Boolean <- simpleCheckboxSignal "locked" (text "Locked") locked
      pure $ CardField_v1 {name: name', value: value', locked: locked'}

    formSignal = do
      formValues :: Card <- loopS card $ \(Card_v1 {content: (CardValues_v1 {title, tags, fields, notes}), timestamp}) -> do
        title' :: String <- loopW title (simpleTextInputWidget "title" (text "Title"))
        -- TODO: tags
        let tags' = tags
        fields' <- sequence $ cardFieldSignal <$> fields
        addField <- fireOnce $ simpleButton "Add field" false AddField
        notes' :: String <- loopW notes (simpleTextInputWidget "notes" (text "Notes"))
        let fields'' = case addField of
                        Nothing -> fields'
                        Just _  -> snoc fields' emptyCardField
        pure $ Card_v1 { content: (CardValues_v1 {title: title', tags: tags', fields: fields'', notes: notes'})
                       , timestamp
                       }
      res <- fireOnce $ simpleButton "Save" false formValues
      -- TODO: add check for form validity
      pure res

data FormSignalAction = AddField
