module Views.CreateCardView where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, loopS, loopW, demand, display, justWait, hold, fireOnce)
import Concur.React (HTML)
import Concur.React.DOM (div, div', text, div_, ul_, li_, form, label, input, datalist, option, span, textarea, button)
import Concur.React.Props as Props
import Control.Alt((<|>), class Alt)
import Control.Applicative (pure)
import Control.Bind (bind, (=<<), (>>=), discard)
import Control.Monad.Except.Trans (runExceptT)
import Control.Semigroupoid ((<<<))
import Data.Array (snoc, filter, catMaybes, singleton, sort, length, range, zipWith)
import Data.Either (Either(..), fromRight, hush)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>), (<$), class Functor, void)
import Data.HeytingAlgebra (not)
import Data.Maybe (Maybe(..), isJust, maybe, fromMaybe)
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Show (show, class Show)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)
import Data.Unit (unit)
import DataModel.Card (CardField(..), CardValues(..), Card(..), emptyCardField)
import DataModel.Password (PasswordGeneratorSettings, standardPasswordGeneratorSettings)
import DataModel.User (UserPreferences(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (never)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.JSState (getAppState)
import Functions.Time (getCurrentTimestamp)
import Functions.Communication.Users (getUserPreferences)
import React.SyntheticEvent (SyntheticMouseEvent)
import Views.PasswordGenerator (passwordGenerator)
import Views.SimpleWebComponents (loadingDiv, simpleButton, dragAndDropAndRemoveList, confirmationWidget, simpleTextInputWidget, simpleCheckboxSignal, simpleCheckboxWidget, simpleTextAreaSignal)

import Debug (traceM)

createCardView :: Card -> Array String -> WidgetState -> Widget HTML (Maybe Card)
createCardView card allTags state = do
  maybeUp <- hush <$> (liftAff $ runExceptT getUserPreferences)
  let fromAppStateToPasswordSettings = \as -> fromMaybe standardPasswordGeneratorSettings $ (\(UserPreferences up) -> up.passwordGeneratorSettings) <$> maybeUp
  passwordGeneratorSettings <- ((fromRight standardPasswordGeneratorSettings) <<< ((<$>) fromAppStateToPasswordSettings)) <$> (liftEffect getAppState)
  mCard <- div [Props._id "cardForm"] do
    case state of
      Default   -> [mask, form [Props.className "cardForm"] [demand (formSignal passwordGeneratorSettings)]]
      Loading   -> [mask, loadingDiv, form [Props.className "cardForm"] [demand (formSignal passwordGeneratorSettings)]] -- TODO: deactivate form
      Error err -> [mask, text err, form [Props.className "cardForm"] [demand (formSignal passwordGeneratorSettings)]]
  case mCard of
    Just (Card { content, timestamp: _ }) -> do
      -- liftEffect $ log $ show content
      timestamp' <- liftEffect $ getCurrentTimestamp
      pure $ Just $ Card { content: content, archived: false, timestamp: timestamp' }
    Nothing -> pure Nothing

  where 
    mask = div [Props.className "mask"] []

    cardFieldWidget :: PasswordGeneratorSettings -> CardField -> Widget HTML CardField
    cardFieldWidget settings (CardField r@{ name, value, locked }) = do
      let generatePasswordWidgets = [(\v -> CardField $ r { value = v })
        <$> do
              simpleButton "password generator" "passwordGenerator" false unit
              div [Props.className "passwordGeneratorOverlay"] [
                div [value <$ Props.onClick] []
              , passwordGenerator settings
              ]
      ]

      --   div [Props.className "fieldForm"] [
      div [Props.classList ([Just "fieldForm", if (locked) then Just "locked" else Nothing])] [
        div [Props.className "inputs"] [
          -- (\v -> CardField $ r { name  = v }) <$> simpleTextInputWidget ("name")  (text "Name")  "Field name"  name
          ((\v -> CardField $ r { name  = v }) <<< (Props.unsafeTargetValue)) <$> label [Props.className "label"] [
            span [] [text "Field label"]
          , input [Props._type "text", Props.placeholder "label", Props.value name, Props.onChange]
          ]
        -- , (\v -> CardField $ r { value = v }) <$> simpleTextInputWidget ("value") (text "Value") "Field value" value
        , ((\v -> CardField $ r { value  = v }) <<< (Props.unsafeTargetValue)) <$> label [Props.className "value"] [
            span [] [text "Field value"]
          -- , input [Props._type "text", Props.placeholder "value", Props.value value, Props.onChange]
          , textarea [Props.placeholder "value", Props.value value, Props.onChange] []
          ]
        ]
      -- , div [Props.className "fieldActions"] $ generatePasswordWidgets <> [(\v -> CardField $ r { locked = v }) <$> (simpleCheckboxWidget "locked" (text "Locked") false locked)]
      , div [Props.className "fieldActions"] $ generatePasswordWidgets <> [
          (\v -> CardField $ r { locked = v })
          <$>
          button [not locked <$ Props.onClick, Props.className "lock"] [text if locked then "locked" else "unlocked"]
        ]
      ]

    fieldsSignal :: PasswordGeneratorSettings -> Array CardField -> Signal HTML (Array CardField)
    fieldsSignal settings fields = do
      let loopables = (\f -> Tuple f (cardFieldWidget settings)) <$> fields 
      fields' <- loopS loopables $ \ls -> do
                                            es <- loopW ls dragAndDropAndRemoveList
                                            -- addField <- fireOnce $ simpleButton "Add field" false unit
                                            addField <- fireOnce $ div [Props.className "newCardField", Props.onClick] [
                                              div [Props.className "fieldGhostShadow"] [
                                                div [Props.className "label"] []
                                              , div [Props.className "value"] []
                                              ]
                                            , button [Props.className "addNewField"] [text "add new field"]
                                            ]
                                            case addField of
                                              Nothing -> pure es
                                              Just _  -> pure $ snoc es (Tuple emptyCardField (cardFieldWidget settings))
      pure $ fst <$> fields'

    tagSignal :: String -> Signal HTML (Maybe String)
    tagSignal tag = li_ [] do
      removeTag <- fireOnce $ simpleButton "remove" "remove tag" false unit
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

    tagsSignal :: String -> Array String -> Signal HTML (Tuple String (Array String))
    tagsSignal newTag tags = div_ [Props.className "tags"] do
      ul_ [] do
        tags' <- (\ts -> ((maybe [] singleton) =<< filter isJust ts)) <$> (sequence $ tagSignal <$> sort tags)
        li_ [Props.className "addTag"] do
          newTag' <- inputTagSignal newTag
          addTag  <- fireOnce $ simpleButton "add tag" "Add tag" (newTag' == "") unit --TODO change with form that returns with `return` key
          case addTag of
            Nothing -> pure $ Tuple newTag' tags'
            Just _  -> pure $ Tuple ""    $ snoc tags' newTag

    formSignal :: PasswordGeneratorSettings -> Signal HTML (Maybe (Maybe Card))
    formSignal settings = do
      Tuple _ formValues <- loopS (Tuple "" card) $ \(Tuple newTag (Card {content: (CardValues {title, tags, fields, notes}), archived, timestamp})) ->
        div_ [Props.className "cardFormFields"] do
          title' :: String <- loopW title (simpleTextInputWidget "title" (text "Title") "Card title")
          Tuple newTag' tags' <- tagsSignal newTag tags
          fields' <- fieldsSignal settings fields
          
          -- notes' :: String <- simpleTextAreaSignal "notes" (text "Notes") "notes" notes
          notes' :: String <- loopW notes (\v -> Props.unsafeTargetValue <$> label [Props.className "notes"] [
              span [] [text "Notes"]
            , textarea [Props.value v, Props.onChange, Props.placeholder "notes", Props.style { height: "40px" }] []
            ])

          pure $ Tuple newTag' $ Card { content: (CardValues {title: title', tags: tags', fields: fields', notes: notes'})
                                      , archived: archived
                                      , timestamp
                                      }
      res <- fireOnce $ div [Props.className "submitButtons"] [(cancelButton formValues) <|> (saveButton formValues)]
      -- TODO: add check for form validity
      pure res

    cancelButton v = 
      if card == v then 
        simpleButton "inactive cancel" "cancel" false Nothing 
      else do
        _ <- simpleButton "active cancel" "cancel" false Nothing 
        confirmation <- (false <$ simpleButton "cancel" "active cancel" false Nothing) <|> (confirmationWidget "Are you sure you want to exit without saving?")
        if confirmation then pure Nothing else (cancelButton v)

    saveButton v = 
      simpleButton "save" "save" (card == v) (Just v)
