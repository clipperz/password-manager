module Views.CardViews where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, loopS, loopW, demand, fireOnce)
import Concur.React (HTML)
import Concur.React.DOM (div, h3, li', p, text, ul)
import Concur.React.Props as Props
import Control.Alt((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (=<<))
import Control.Semigroupoid ((<<<))
import Data.Array (snoc, filter, singleton, sort)
import Data.DateTime.Instant (unInstant)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (ceil)
import Data.Maybe (Maybe(..), isJust, fromJust, maybe)
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show, class Show)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import DataModel.Card (CardField(..), CardValues(..), Card(..), emptyCardField)
-- import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Functions.Clipboard (copyToClipboard)
import Views.SimpleWebComponents (simpleButton, simpleTextInputWidget, simpleCheckboxSignal)

import Effect.Class.Console (log)

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

createCardView :: Card -> Widget HTML (Maybe Card)
createCardView card = do
  mCard <- div [] [demand formSignal]
  case mCard of
    Just (Card_v1 { content, timestamp: _ }) -> do
      timestamp' <- liftEffect $ (ceil <<< unwrap <<< unInstant) <$> now
      pure $ Just $ Card_v1 { content: content, timestamp: timestamp' }
    Nothing -> pure Nothing

  where 
    cardFieldSignal :: CardField -> Signal HTML (Maybe CardField)
    cardFieldSignal field = do
      removeField <- fireOnce $ simpleButton "Remove field" false RemoveField
      cardField <- loopS field $ \(CardField_v1 { name, value, locked }) -> do
        name' :: String <- loopW name (simpleTextInputWidget "name" (text "Name"))
        value' :: String <- loopW value (simpleTextInputWidget "value" (text "Value"))
        locked' :: Boolean <- simpleCheckboxSignal "locked" (text "Locked") locked
        pure $ CardField_v1 {name: name', value: value', locked: locked'}
      case removeField of
        Nothing -> pure $ Just cardField
        Just _  -> pure $ Nothing

    fieldsSignal :: Array CardField -> Signal HTML (Array CardField)
    fieldsSignal fields = do
      fields' :: Array CardField <- (\fs -> (maybe [] singleton) =<< filter isJust fs) <$> (sequence $ cardFieldSignal <$> fields)
      addField <- fireOnce $ simpleButton "Add field" false AddField
      case addField of
        Nothing -> pure fields'
        Just _  -> pure $ snoc fields' emptyCardField

    tagSignal :: String -> Signal HTML (Maybe String)
    tagSignal tag = do
      removeTag <- fireOnce $ simpleButton "x" false RemoveTag
      tag' <- loopW tag text
      case removeTag of
        Nothing -> pure $ Just tag'
        Just _  -> pure $ Nothing

    tagsSignal :: String -> Array String -> Signal HTML (Tuple String (Array String))
    tagsSignal newTag tags = do
      tags' <- (\ts -> ((maybe [] singleton) =<< filter isJust ts)) <$> (sequence $ tagSignal <$> sort tags)
      newTag' <- loopW newTag (simpleTextInputWidget "" (text "add tag"))
      addTag <- fireOnce $ simpleButton "Add tag" false AddTag --TODO change with form that returns with `return` key
      case addTag of
        Nothing -> pure $ Tuple newTag' tags'
        Just _  -> pure $ Tuple "" $ snoc tags' newTag

    formSignal :: Signal HTML (Maybe (Maybe Card))
    formSignal = do
      Tuple _ formValues <- loopS (Tuple "" card) $ \(Tuple newTag (Card_v1 {content: (CardValues_v1 {title, tags, fields, notes}), timestamp})) -> do
        title' :: String <- loopW title (simpleTextInputWidget "title" (text "Title"))
        Tuple newTag' tags' <- tagsSignal newTag tags
        fields' <- fieldsSignal fields
        notes' :: String <- loopW notes (simpleTextInputWidget "notes" (text "Notes"))
        pure $ Tuple newTag' $ Card_v1 { content: (CardValues_v1 {title: title', tags: tags', fields: fields', notes: notes'})
                                       , timestamp
                                       }
      res <- fireOnce (simpleButton "Cancel" false Nothing <|> simpleButton "Save" false (Just formValues))
      -- TODO: add check for form validity
      pure res

data FormSignalAction = AddField | RemoveField | AddTag | RemoveTag
