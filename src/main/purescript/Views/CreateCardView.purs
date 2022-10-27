module Views.CreateCardView where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, loopS, loopW, demand, fireOnce)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Concur.React.Props as Props
import Control.Alt((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, (=<<))
import Data.Array (snoc, filter, singleton, sort)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unit (unit)
import DataModel.Card (CardField(..), CardValues(..), Card(..), emptyCardField)
import DataModel.WidgetState (WidgetState(..))
import Effect.Class (liftEffect)
import Functions.Time (getCurrentTimestamp)
import Views.SimpleWebComponents (loadingDiv, simpleButton, simpleTextInputWidget, simpleCheckboxSignal, disableOverlay)

createCardView :: Card -> WidgetState -> Widget HTML (Maybe Card)
createCardView card state = do
  mCard <- case state of
    Default -> div [] [disableOverlay, div [Props.className "cardForm"] [demand formSignal]]
    Loading -> div [] [disableOverlay, loadingDiv, div [Props.className "cardForm"] [demand formSignal]] -- TODO: deactivate form
    Error err -> div [] [disableOverlay, text err, div [Props.className "cardForm"] [demand formSignal]]
  case mCard of
    Just (Card_v1 { content, timestamp: _ }) -> do
      timestamp' <- liftEffect $ getCurrentTimestamp
      pure $ Just $ Card_v1 { content: content, archived: false, timestamp: timestamp' }
    Nothing -> pure Nothing

  where 
    cardFieldSignal :: CardField -> Signal HTML (Maybe CardField)
    cardFieldSignal field = do
      removeField <- fireOnce $ simpleButton "Remove field" false unit
      field' <- loopS field $ \(CardField { name, value, locked }) -> do
        name' :: String <- loopW name (simpleTextInputWidget "name" (text "Name"))
        value' :: String <- loopW value (simpleTextInputWidget "value" (text "Value"))
        locked' :: Boolean <- simpleCheckboxSignal "locked" (text "Locked") locked
        pure $ CardField {name: name', value: value', locked: locked'}
      case removeField of
        Nothing -> pure $ Just field'
        Just _  -> pure $ Nothing

    fieldsSignal :: Array CardField -> Signal HTML (Array CardField)
    fieldsSignal fields = do
      fields' :: Array CardField <- (\fs -> (maybe [] singleton) =<< filter isJust fs) <$> (sequence $ cardFieldSignal <$> fields)
      addField <- fireOnce $ simpleButton "Add field" false unit
      case addField of
        Nothing -> pure fields'
        Just _  -> pure $ snoc fields' emptyCardField

    tagSignal :: String -> Signal HTML (Maybe String)
    tagSignal tag = do
      removeTag <- fireOnce $ simpleButton "x" false unit
      tag' <- loopW tag text
      case removeTag of
        Nothing -> pure $ Just tag'
        Just _  -> pure $ Nothing

    tagsSignal :: String -> Array String -> Signal HTML (Tuple String (Array String))
    tagsSignal newTag tags = do
      tags' <- (\ts -> ((maybe [] singleton) =<< filter isJust ts)) <$> (sequence $ tagSignal <$> sort tags)
      newTag' <- loopW newTag (simpleTextInputWidget "" (text "add tag"))
      addTag <- fireOnce $ simpleButton "Add tag" false unit --TODO change with form that returns with `return` key
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
                                       , archived: false
                                       , timestamp
                                       }
      res <- fireOnce (simpleButton "Cancel" false Nothing <|> simpleButton "Save" false (Just formValues))
      -- TODO: add check for form validity
      pure res
