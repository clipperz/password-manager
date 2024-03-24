module Views.CreateCardView where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, demand, fireOnce, loopS, loopW)
import Concur.React (HTML)
import Concur.React.DOM (button, datalist, div, div_, form, input, label, li_, option, span, text, textarea, ul_)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (=<<))
import Control.Semigroupoid ((<<<))
import Data.Array (filter, singleton, snoc, sort)
import Data.Eq ((==), (/=))
import Data.Function (($))
import Data.Functor ((<$), (<$>))
import Data.HeytingAlgebra (not)
import Data.Maybe (Maybe(..), isJust, maybe, fromMaybe)
import Data.Semigroup ((<>))
import Data.Set (Set, difference, fromFoldable, member, toUnfoldable)
import Data.String (null)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)
import Data.Unit (Unit, unit)
import DataModel.AsyncValue as Async
import DataModel.CardVersions.Card (Card(..), CardField(..), CardValues(..), FieldType(..), emptyCardField)
import DataModel.Password (PasswordGeneratorSettings)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Functions.Card (getFieldType)
import Functions.Time (getCurrentTimestamp)
import MarkdownIt (renderString)
import Views.Components (dynamicWrapper, entropyMeter)
import Views.PasswordGenerator (passwordGenerator)
import Views.SimpleWebComponents (confirmationWidget, dragAndDropAndRemoveList, simpleButton)

createCardView :: Card -> Set String -> PasswordGeneratorSettings -> Widget HTML (Maybe Card)
createCardView card allTags passwordGeneratorSettings = do
  mCard <- div [Props._id "cardForm"] [
    mask
  , div [Props.className "cardForm"] [
      demand (formSignal passwordGeneratorSettings)
    ]
  ]
  case mCard of
    Just (Card { content, secrets }) -> do
      timestamp' <- liftEffect $ getCurrentTimestamp
      pure $ Just (Card { content, secrets, archived: false, timestamp: timestamp' })
    Nothing -> pure Nothing

  where 
    mask = div [Props.className "mask"] []

    getActionButton :: CardField -> Widget HTML Unit
    getActionButton cardField =
      case getFieldType cardField of
        Passphrase  -> button [unit <$ Props.onClick, Props.disabled false, Props.className "action passwordGenerator" ] [span [] [text "password generator"]]
        Email       -> button [Props.disabled true, Props.className "action email"]                                      [span [] [text "email"]]
        Url         -> button [Props.className "action url",  Props.disabled true]                                       [span [] [text "url"]]
        None        -> button [Props.className "action none", Props.disabled true]                                       [span [] [text "none"]]

    cardFieldWidget :: PasswordGeneratorSettings -> CardField -> Widget HTML CardField
    cardFieldWidget defaultSettings cf@(CardField r@{ name, value, locked, settings}) = do
      let fieldActionWidget = [(\(Tuple v s) -> CardField $ r { value = v, settings = s })
        <$> do
              getActionButton cf
              if locked then
                button [Props.disabled true, Props.className "action passwordGenerator" ] [span [] [text "password generator"]]
                <>
                (div [Props.className "passwordGeneratorOverlay"] [
                  div [(Tuple value settings) <$ Props.onClick, Props.className "passwordGeneratorMask"] []
                , div [Props.className "passwordGeneratorPopup"] [passwordGenerator (fromMaybe defaultSettings settings) (if null value then (Async.Loading Nothing) else (Async.Done value))]
                ])
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
          , dynamicWrapper Nothing value $ textarea [Props.rows 1, Props.placeholder (if locked then "" else "value"), Props.value value, Props.onChange] []
          , (if locked
            then (entropyMeter value)
            else (text "")
            )
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

    inputTagSignal :: String -> Set String -> Signal HTML (Tuple String Boolean)
    inputTagSignal newTag tags = do

      loopW (Tuple newTag false) (\(Tuple value _) -> do
        result <- form [(\_ -> Tuple value (value /= "")) <$> Props.onSubmit] [
          label [] [
            span [Props.className "label"] [text "New Tag"]
            , input [
                Props._type "text"
              , Props.placeholder "add tag"
              , Props.value value
              , Props.list "tags-list"
              , (\e -> Tuple (Props.unsafeTargetValue e) (member (Props.unsafeTargetValue e) (difference allTags tags))) <$> Props.onInput
              ]
            , datalist [Props._id "tags-list"] ((\t -> option [] [text t]) <$> (toUnfoldable $ difference allTags tags))
          ]
        ] 
        pure result
      )

    tagsSignal :: String -> Set String -> Signal HTML (Tuple String (Array String))
    tagsSignal newTag tags = div_ [Props.className "tags"] do
      ul_ [] do
        tags' <- (\ts -> ((maybe [] singleton) =<< filter isJust ts)) <$> (sequence $ tagSignal <$> (sort $ toUnfoldable tags))
        li_ [Props.className "addTag"] do
          Tuple newTag' addTag <- inputTagSignal newTag tags
          case addTag of
            false -> pure $ Tuple newTag' tags'
            true  -> do
              pure $ Tuple ""    $ snoc tags' newTag'
      
    notesSignal :: String -> Boolean -> Signal HTML (Tuple String Boolean)
    notesSignal notes preview = do
      loopW (Tuple notes preview) (\(Tuple _notes _preview) ->
        button [(Tuple _notes (not _preview)) <$ Props.onClick, Props.className "preview"] [text if _preview then "Edit" else "Preview Markdown"]
        <>
        (if _preview 
        then
          (Tuple _notes _preview) <$ div [Props.className "card_notes"] [
            div [Props.className "markdown-body", Props.dangerouslySetInnerHTML { __html: unsafePerformEffect $ renderString notes}] []
          ]       
        else
          (\e -> (Tuple (Props.unsafeTargetValue e) _preview)) <$> label [Props.className "notes"] [
            span [Props.className "label"] [text "Notes"]
          , dynamicWrapper Nothing _notes $ textarea [Props.rows 1, Props.value _notes, Props.onChange, Props.placeholder "notes"] []
          ]
        )
      )

    formSignal :: PasswordGeneratorSettings -> Signal HTML (Maybe (Maybe Card))
    formSignal settings = do
      { card: formValues } <- loopS { newTag: "", preview: false, card: card } $ \{ newTag, preview, card: Card {content: (CardValues {title, tags, fields, notes}), archived, timestamp} } ->
        div_ [Props.className "cardFormFields"] do
          title' :: String <- loopW title (\_title -> label [Props.className "title"] [
            span [Props.className "label"] [text "Title"]
          , dynamicWrapper Nothing _title $ textarea [Props.rows 1, Props.placeholder "Card title", Props.autoFocus true, Props.value _title, Props.unsafeTargetValue <$> Props.onChange] []
          ])

          Tuple newTag' tags' <- tagsSignal newTag tags
          
          fields' <- fieldsSignal settings fields

          Tuple notes' preview' <- notesSignal notes preview

          pure $ {
            newTag: newTag'
          , preview: preview'
          , card: Card { content: (CardValues {title: title', tags: fromFoldable tags', fields: fields', notes: notes'})
                       , secrets: []
                       , archived: archived
                       , timestamp
                       }
          }
      res <- fireOnce $ div [Props.className "submitButtons"] [(cancelButton formValues) <|> (saveButton formValues)]

      pure res

    cancelButton v = 
      if (card == v) then 
      -- if ((card == v && not isNew) || (v == emptyCard && isNew)) then 
        simpleButton "inactive cancel" "cancel" false Nothing 
      else do
        _ <- simpleButton "active cancel" "cancel" false Nothing 
        confirmation <- (false <$ simpleButton "active cancel" "cancel" false Nothing) <|> (confirmationWidget "Are you sure you want to exit without saving?")
        if confirmation then pure Nothing else (cancelButton v)

    saveButton v =
      -- simpleButton "save" "save" ((not isNew && card == v) || (isNew && v == emptyCard)) (Just v)
      simpleButton "save" "save" (card == v) (Just v)
