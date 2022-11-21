module Views.CreateCardView where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, loopS, loopW, demand, display, justWait, hold, fireOnce)
import Concur.React (HTML)
import Concur.React.DOM (div, div', text, div_, label, input, datalist, option)
import Concur.React.Props as Props
import Control.Alt((<|>), class Alt)
import Control.Applicative (pure)
import Control.Bind (bind, (=<<), (>>=), discard)
import Control.Semigroupoid ((<<<))
import Data.Array (snoc, filter, catMaybes, singleton, sort, length, range, zipWith)
import Data.Either (Either(..), fromRight)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>), (<$), class Functor)
import Data.Maybe (Maybe(..), isJust, maybe, fromMaybe)
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Show (show, class Show)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)
import Data.Unit (unit)
import DataModel.Card (CardField(..), CardValues(..), Card(..), emptyCardField)
import DataModel.Password (PasswordGeneratorSettings, standardPasswordGeneratorSettings)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (never)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.JSState (getAppState)
import Functions.Time (getCurrentTimestamp)
import React.SyntheticEvent (SyntheticMouseEvent)
import Views.PasswordGenerator (passwordGenerator)
import Views.SimpleWebComponents (loadingDiv, simpleButton, dragAndDropList', confirmationWidget, simpleTextInputWidget, simpleCheckboxSignal, simpleCheckboxWidget, disableOverlay, simpleTextAreaSignal)

data CardFieldWidgetAction = Remove | Update CardField

createCardView :: Card -> Array String -> WidgetState -> Widget HTML (Maybe Card)
createCardView card allTags state = do
  let fromAppStateToPasswordSettings = \as -> fromMaybe standardPasswordGeneratorSettings $ (\up -> up.passwordGeneratorSettings) <$> as.userPreferences
  passwordGeneratorSettings <- ((fromRight standardPasswordGeneratorSettings) <<< ((<$>) fromAppStateToPasswordSettings)) <$> (liftEffect getAppState)
  mCard <- div [Props._id "cardForm"] do
    case state of
      Default   -> [disableOverlay, div [Props.className "cardForm"] [demand (formSignal passwordGeneratorSettings)]]
      Loading   -> [disableOverlay, loadingDiv, div [Props.className "cardForm"] [demand (formSignal passwordGeneratorSettings)]] -- TODO: deactivate form
      Error err -> [disableOverlay, text err, div [Props.className "cardForm"] [demand (formSignal passwordGeneratorSettings)]]
  case mCard of
    Just (Card { content, timestamp: _ }) -> do
      liftEffect $ log $ show content
      timestamp' <- liftEffect $ getCurrentTimestamp
      pure $ Just $ Card { content: content, archived: false, timestamp: timestamp' }
    Nothing -> pure Nothing

  where 
    cardFieldSignal :: PasswordGeneratorSettings -> {index :: Int, field :: CardField} -> Signal HTML (Maybe CardField)
    cardFieldSignal settings {index, field} = div_ [Props.className "cardField"] do
      let strIndex = show index
      removeField <- fireOnce $ simpleButton "x" false unit
      field' <- loopS field $ \(CardField { name, value, locked }) -> do
        { name', value' } <- div_ [Props.className "inputs"] do
          name'  :: String <- loopW name  (simpleTextInputWidget ("name" <> strIndex)  (text "Name")  "Field name")
          value' :: String <- loopW value (simpleTextInputWidget ("value" <> strIndex) (text "Value") "Field value")
          pure { name', value' }
        { generatePassword, locked' } <- div_ [] do
          generatePassword <- case locked of
            false -> pure $ Nothing 
            true ->  fireOnce $ do
              simpleButton "Gen Pass" false unit
              div [Props.className "passwordGeneratorOverlay"] [
                div [value <$ Props.onClick] []
              , passwordGenerator settings
              ]
          locked' :: Boolean <- simpleCheckboxSignal "locked" (text "Locked") false locked
          pure { generatePassword, locked' }
        pure $ case generatePassword of
          Nothing -> CardField {name: name', value: value', locked: locked'}
          Just generatedValue -> CardField {name: name', value: generatedValue, locked: locked'}
      case removeField of
        Nothing -> pure $ Just field'
        Just _  -> pure $ Nothing
    
    cardFieldWidget :: PasswordGeneratorSettings -> Maybe CardField -> Widget HTML (Maybe CardField)
    cardFieldWidget _ Nothing = pure Nothing
    cardFieldWidget settings (Just (CardField r@{ name, value, locked })) = do
      let generatePasswordWidgets = case locked of
                                      false -> []
                                      true -> [
                                        (\v -> Update (CardField $ r { value = v })) <$> do
                                                                                          simpleButton "Gen Pass" false unit
                                                                                          div [Props.className "passwordGeneratorOverlay"] [
                                                                                            div [value <$ Props.onClick] []
                                                                                          , passwordGenerator settings
                                                                                          ]
                                      ]
      res <- div [Props.className "fieldForm"] [
        simpleButton "x" false Remove
      , div [Props.className "inputs"] [
          (\v -> Update (CardField $ r { name  = v })) <$> simpleTextInputWidget ("name")  (text "Name")  "Field name"  name
        , (\v -> Update (CardField $ r { value = v })) <$> simpleTextInputWidget ("value") (text "Value") "Field value" value
        ]
      , div [] $ generatePasswordWidgets <> [(\v -> Update (CardField $ r { locked = v })) <$> (simpleCheckboxWidget "locked" (text "Loacked") false locked)]
      ]
      case res of
        Remove -> pure Nothing
        Update f -> pure $ Just f

    fieldsSignal :: PasswordGeneratorSettings -> Array CardField -> Signal HTML (Array CardField)
    fieldsSignal settings fields = do
      -- let indexedFields = zipWith (\i -> \f -> { index: i, field: f}) (range 0 ((length fields - 1))) fields
      -- let signals = (cardFieldSignal settings) <$> indexedFields
      -- fields' :: Array CardField <- (\fs -> (maybe [] singleton) =<< filter isJust fs) <$> (sequence $ signals)
      -- addField <- fireOnce $ simpleButton "Add field" false unit
      -- case addField of
      --   Nothing -> pure fields'
      --   Just _  -> pure $ snoc fields' emptyCardField
      let loopables = (\f -> Tuple (Just f) (cardFieldWidget settings)) <$> fields 
      fields' <- loopW loopables (\ls -> do
                                    let newLoopables = filter (\(Tuple v _) -> isJust v) ls
                                    dragAndDropList' ls
                                  )
      addField <- fireOnce $ simpleButton "Add field" false unit
      let newFields = catMaybes $ fst <$> fields'
      case addField of
        Nothing -> pure newFields 
        Just _  -> pure $ snoc newFields emptyCardField

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

    formSignal :: PasswordGeneratorSettings -> Signal HTML (Maybe (Maybe Card))
    formSignal settings = do
      Tuple _ formValues <- loopS (Tuple "" card) $ \(Tuple newTag (Card {content: (CardValues {title, tags, fields, notes}), timestamp})) ->
        div_ [Props.className "cardFormFields"] do
          title' :: String <- loopW title (simpleTextInputWidget "title" (text "Title") "Card title")
          Tuple newTag' tags' <- tagsSignal newTag tags
          fields' <- fieldsSignal settings fields
          notes' :: String <- simpleTextAreaSignal "notes" (text "Notes") "notes" notes
          pure $ Tuple newTag' $ Card { content: (CardValues {title: title', tags: tags', fields: fields', notes: notes'})
                                      , archived: false
                                      , timestamp
                                      }
      res <- fireOnce $ div [Props.className "submitButtons"] [cancelButton <|> simpleButton "Save" false (Just formValues)]
      -- TODO: add check for form validity
      pure res

    cancelButton = do
      _ <- simpleButton "Cancel" false Nothing 
      confirmation <- confirmationWidget "Are you sure you want to exit without saving?"
      if confirmation then pure Nothing else cancelButton
