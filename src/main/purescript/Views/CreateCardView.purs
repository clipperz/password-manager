module Views.CreateCardView where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, loopS, loopW, demand, display, justWait, hold, fireOnce)
import Concur.Core.Props (filterProp)
import Concur.React (HTML)
import Concur.React.DOM (a, div, div', text, div_, ul_, li_, form_, form, label, input, datalist, option, span, textarea, button)
import Concur.React.Props as Props
import Control.Alt((<|>), class Alt)
import Control.Applicative (pure)
import Control.Bind (bind, (=<<), (>>=), discard)
import Control.Monad.Except.Trans (runExceptT)
import Control.Semigroupoid ((<<<))
import Data.Array (snoc, filter, catMaybes, singleton, sort, length, range, zipWith)
import Data.Either (Either(..), fromRight, hush)
import Data.Eq ((==), (/=))
import Data.Function (($))
import Data.Functor ((<$>), (<$), class Functor, void)
import Data.HeytingAlgebra (not, (&&), (||))
import Data.Maybe (Maybe(..), isJust, maybe, fromMaybe)
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Show (show, class Show)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)
import Data.Unit (Unit, unit)
import DataModel.Card (CardField(..), CardValues(..), Card(..), emptyCardField)
import DataModel.Password (PasswordGeneratorSettings, standardPasswordGeneratorSettings)
import DataModel.User (UserPreferences(..))
import DataModel.WidgetState (WidgetState(..))
import Effect (Effect)
import Effect.Aff (never)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Card (getFieldType, FieldType(..))
import Functions.JSState (getAppState)
import Functions.Time (getCurrentTimestamp)
import Functions.Communication.Users (getUserPreferences)
import React.SyntheticEvent (SyntheticMouseEvent)
import Views.PasswordGenerator (passwordGenerator)
import Views.SimpleWebComponents (loadingDiv, simpleButton, dragAndDropAndRemoveList, confirmationWidget, simpleTextInputWidget, simpleCheckboxSignal, simpleCheckboxWidget, simpleTextAreaSignal)
import Views.Components (dynamicWrapper)

import Debug (traceM)

import Unsafe.Coerce (unsafeCoerce)
import Concur.Core.Props (handleProp)
import React.SyntheticEvent (NativeEvent, SyntheticKeyboardEvent)

createCardView :: Card -> Array String -> WidgetState -> Widget HTML (Maybe Card)
createCardView card allTags state = do
  maybeUp <- hush <$> (liftAff $ runExceptT getUserPreferences)
  let fromAppStateToPasswordSettings = \as -> fromMaybe standardPasswordGeneratorSettings $ (\(UserPreferences up) -> up.passwordGeneratorSettings) <$> maybeUp
  passwordGeneratorSettings <- ((fromRight standardPasswordGeneratorSettings) <<< ((<$>) fromAppStateToPasswordSettings)) <$> (liftEffect getAppState)
  mCard <- div [Props._id "cardForm"] do
    case state of
      Default   -> [mask, div [Props.className "cardForm"] [demand (formSignal passwordGeneratorSettings)]]
      Loading   -> [mask, loadingDiv, div [Props.className "cardForm"] [demand (formSignal passwordGeneratorSettings)]] -- TODO: deactivate form
      Error err -> [mask, text err, div [Props.className "cardForm"] [demand (formSignal passwordGeneratorSettings)]]
      -- Default   -> [mask, form [Props.className "cardForm"] [demand (formSignal passwordGeneratorSettings)]]
      -- Loading   -> [mask, loadingDiv, form [Props.className "cardForm"] [demand (formSignal passwordGeneratorSettings)]] -- TODO: deactivate form
      -- Error err -> [mask, text err, form [Props.className "cardForm"] [demand (formSignal passwordGeneratorSettings)]]
  case mCard of
    Just (Card { content, timestamp: _ }) -> do
      -- liftEffect $ log $ show content
      timestamp' <- liftEffect $ getCurrentTimestamp
      pure $ Just $ Card { content: content, archived: false, timestamp: timestamp' }
    Nothing -> pure Nothing

  where 
    mask = div [Props.className "mask"] []

    getActionButton :: CardField -> Widget HTML Unit
    getActionButton cardField@(CardField { name, value, locked }) =
      case getFieldType cardField of
        Passphrase  -> button [unit <$ Props.onClick, Props.disabled false, Props.className "action passwordGenerator" ] [span [] [text "password generator"]]
        Email       -> button [Props.disabled true, Props.className "action email"] [span [] [text "email"]]
        -- Url         -> button [Props.className "action url"] [span [] [a [Props.href value, Props.target "_blank"] [text "url"]]]
        Url         -> button [Props.className "action url", Props.disabled true]  [span [] [text "url"]]
        None        -> button [Props.className "action none", Props.disabled true] [span [] [text "none"]]

    cardFieldWidget :: PasswordGeneratorSettings -> CardField -> Widget HTML CardField
    cardFieldWidget settings cf@(CardField r@{ name, value, locked }) = do
      let fieldActionWidget = [(\v -> CardField $ r { value = v })
        <$> do
              getActionButton cf
              if locked then
                div [] [
                  button [Props.disabled true, Props.className "action passwordGenerator" ] [span [] [text "password generator"]]
                , (div [Props.className "passwordGeneratorOverlay"] [
                    div [value <$ Props.onClick] []
                  , passwordGenerator settings
                  ])
                ]
              else div [] []
      ]

      div [Props.classList ([Just "fieldForm", if (locked) then Just "locked" else Nothing])] [
        div [Props.className "inputs"] [
          ((\v -> CardField $ r { name  = v }) <<< (Props.unsafeTargetValue)) <$> label [Props.className "label"] [
            span [Props.className "label"] [text "Field label"]
          , input [Props._type "text", Props.placeholder "label", Props.value name, Props.onChange]
          ]
        , ((\v -> CardField $ r { value  = v }) <<< (Props.unsafeTargetValue)) <$> label [Props.className "value"] [
            span [Props.className "label"] [text "Field value"]
          , dynamicWrapper value $ textarea [Props.rows 1, Props.placeholder (if locked then "" else "value"), Props.value value, Props.onChange] []
          ]
        ]
      , div [Props.className "fieldActions"] $ fieldActionWidget <> [
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

    inputTagSignal :: String -> Signal HTML (Tuple String Boolean)
    inputTagSignal newTag = do

      loopW (Tuple newTag false) (\(Tuple value _) -> do
        -- log value
        result@(Tuple value enter) <- form [(\e -> Tuple value (value /= "")) <$> Props.onSubmit] [
          label [] [
            span [Props.className "label"] [text "New Tag"]
            , input [
                Props._type "text"
              , Props.placeholder "add tag"
              , Props.value value
              , Props.list "tags-list"
              , (\e -> Tuple (Props.unsafeTargetValue e) false) <$> Props.onChange
              ]
            , datalist [Props._id "tags-list"] ((\t -> option [] [text t]) <$> allTags)
          ]
        ]
        pure result
      )

    tagsSignal :: String -> Array String -> Signal HTML (Tuple String (Array String))
    tagsSignal newTag tags = div_ [Props.className "tags"] do
      ul_ [] do
        tags' <- (\ts -> ((maybe [] singleton) =<< filter isJust ts)) <$> (sequence $ tagSignal <$> sort tags)
        li_ [Props.className "addTag"] do
          Tuple newTag' addTag <- inputTagSignal newTag
          case addTag of
            false -> pure $ Tuple newTag' tags'
            true  -> do
              traceM "newTag field exited"
              pure $ Tuple ""    $ snoc tags' newTag'

    formSignal :: PasswordGeneratorSettings -> Signal HTML (Maybe (Maybe Card))
    formSignal settings = do
      Tuple _ formValues <- loopS (Tuple "" card) $ \(Tuple newTag (Card {content: (CardValues {title, tags, fields, notes}), archived, timestamp})) ->
        div_ [Props.className "cardFormFields"] do
          title' :: String <- loopW title (simpleTextInputWidget "title" (text "Title") "Card title")
         
          Tuple newTag' tags' <- tagsSignal newTag tags
          
          fields' <- fieldsSignal settings fields

          notes' :: String <- loopW notes (\v -> Props.unsafeTargetValue <$> label [Props.className "notes"] [
            span [Props.className "label"] [text "Notes"]
          , dynamicWrapper v $ textarea [Props.rows 1, Props.value v, Props.onChange, Props.placeholder "notes"] []
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
