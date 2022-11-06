module Views.CreateCardView where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, loopS, loopW, demand, fireOnce)
import Concur.React (HTML)
import Concur.React.DOM (div, div', text, div_, label, input, datalist, option)
import Concur.React.Props as Props
import Control.Alt((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, (=<<), discard)
import Data.Array (snoc, filter, singleton, sort)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Show (show)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unit (unit)
import DataModel.Card (CardField(..), CardValues(..), Card(..), emptyCardField)
import DataModel.WidgetState (WidgetState(..))
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Time (getCurrentTimestamp)
import Views.PasswordGenerator (passwordGenerator)
import Views.SimpleWebComponents (loadingDiv, simpleButton, simpleTextInputWidget, simpleCheckboxSignal, disableOverlay, simpleTextAreaSignal)

createCardView :: Card -> Array String -> WidgetState -> Widget HTML (Maybe Card)
createCardView card allTags state = do
  mCard <- div [Props._id "cardForm"] do
    case state of
      Default   -> [disableOverlay, div [Props.className "cardForm"] [demand formSignal]]
      Loading   -> [disableOverlay, loadingDiv, div [Props.className "cardForm"] [demand formSignal]] -- TODO: deactivate form
      Error err -> [disableOverlay, text err, div [Props.className "cardForm"] [demand formSignal]]
  case mCard of
    Just (Card { content, timestamp: _ }) -> do
      liftEffect $ log $ show content
      timestamp' <- liftEffect $ getCurrentTimestamp
      pure $ Just $ Card { content: content, archived: false, timestamp: timestamp' }
    Nothing -> pure Nothing

  where 
    cardFieldSignal :: CardField -> Signal HTML (Maybe CardField)
    cardFieldSignal field = div_ [Props.className "cardField"] do
      removeField <- fireOnce $ simpleButton "x" false unit
      field' <- loopS field $ \(CardField { name, value, locked }) -> do
        { name', value' } <- div_ [Props.className "inputs"] do
          name' :: String <- loopW name (simpleTextInputWidget "name" (text "Name") "Field name")
          value' :: String <- loopW value (simpleTextInputWidget "value" (text "Value") "Field value")
          pure { name', value' }
        { generatePassword, locked' } <- div_ [] do
          generatePassword <- case locked of
            false -> pure $ Nothing 
            true ->  fireOnce $ do
              simpleButton "Gen Pass" false unit
              div [Props.className "passwordGeneratorOverlay"] [
                div [value <$ Props.onClick] []
              , passwordGenerator
              ]
          locked' :: Boolean <- simpleCheckboxSignal "locked" (text "Locked") false locked
          pure { generatePassword, locked' }
        pure $ case generatePassword of
          Nothing -> CardField {name: name', value: value', locked: locked'}
          Just generatedValue -> CardField {name: name', value: generatedValue, locked: locked'}
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

    inputTagSignal :: String -> Signal HTML String
    inputTagSignal newTag = loopW newTag (\value -> div' [
      label [Props.htmlFor "new-tag", Props.className "hide-element"] [text "New Tag"]
    , (Props.unsafeTargetValue) <$> input [
        Props._type "text"
      , Props._id "new-tag"
      , Props.placeholder "add tag"
      , Props.value value
      , Props.onChange
      , Props.list "tags-list"
      ]
    , datalist [Props._id "tags-list"] ((\t -> option [] [text t]) <$> allTags)
    ])

    tagsSignal newTag tags = div_ [] do
      tags' <- (\ts -> ((maybe [] singleton) =<< filter isJust ts)) <$> (sequence $ tagSignal <$> sort tags)
      newTag' <- inputTagSignal newTag
      -- newTag' <- loopW newTag (simpleTextInputWidget "" (text "add tag"))
      addTag  <- fireOnce $ simpleButton "Add tag" (newTag' == "") unit --TODO change with form that returns with `return` key
      case addTag of
        Nothing -> pure $ Tuple newTag' tags'
        Just _  -> pure $ Tuple "" $ snoc tags' newTag

    formSignal :: Signal HTML (Maybe (Maybe Card))
    formSignal = do
      Tuple _ formValues <- loopS (Tuple "" card) $ \(Tuple newTag (Card {content: (CardValues {title, tags, fields, notes}), timestamp})) ->
        div_ [Props.className "cardFormFields"] do
          title' :: String <- loopW title (simpleTextInputWidget "title" (text "Title") "Card title")
          Tuple newTag' tags' <- tagsSignal newTag tags
          fields' <- fieldsSignal fields
          notes' :: String <- simpleTextAreaSignal "notes" (text "Notes") "notes" notes
          pure $ Tuple newTag' $ Card { content: (CardValues {title: title', tags: tags', fields: fields', notes: notes'})
                                      , archived: false
                                      , timestamp
                                      }
      res <- fireOnce (div [Props.className "submitButtons"] [simpleButton "Cancel" false Nothing <|> simpleButton "Save" false (Just formValues)])
      -- TODO: add check for form validity
      pure res
