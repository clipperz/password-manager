module Views.ImportView where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, fireOnce, loopS, loopW)
import Concur.React (HTML)
import Concur.React.DOM (a, a', br', button, dd, div, div_, dl, dt, form, h1, h3, h5, input, label, li', li_, p, span, text, ul, ul_)
import Concur.React.Props as Props
import Control.Alt (map, ($>), (<#>), (<|>))
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Category ((>>>))
import Data.Array (concat, filter, head, length)
import Data.Either (Either(..), fromRight)
import Data.Function (flip, (#), ($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra (not)
import Data.Maybe (Maybe(..), maybe)
import Data.Semigroup ((<>))
import Data.Set (insert, toUnfoldable)
import Data.Show (show)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd, swap)
import DataModel.CardVersions.Card (Card(..), CardValues(..), CardField(..))
import DataModel.WidgetState (ImportState, ImportStep(..))
import React.SyntheticEvent (NativeEventTarget, SyntheticEvent_)
import Unsafe.Coerce (unsafeCoerce)
import Views.SimpleWebComponents (simpleButton, simpleTextAreaSignal, simpleTextInputWidget)
import Web.File.File (File)
import Web.File.FileList (FileList, items)

data QuickSelection = All | None | Archived | NonArchived

data SelectionAction = Cards (Array (Tuple Boolean Card)) | NewQuickSelection QuickSelection

type CardSelectionInfo = { tag :: Maybe String, selectedCards :: (Array (Tuple Boolean Card)) }

initialImportState :: ImportState
initialImportState = {
  step:      Upload
, content:   Right ""
, selection: []
, tag:       Tuple false ""
}

data DragFileEvents a = DragEnter a | DragLeave a | Drop a | File (Maybe File)

data NavigationAction = Back ImportState | Next ImportState 

importView :: ImportState -> Widget HTML ImportState
importView state@{step, content, selection, tag} = do
  res <- div [Props._id "importPage"] [
    form [Props.className "importPage"] [
      h1 [] [text "Import"]
    , importInternalView step
    ]
  ]
  case res of
    Back state' -> importView state'
    Next state' -> pure       state'

  where
    importInternalView :: ImportStep -> Widget HTML NavigationAction


    importInternalView Upload = do
      p [Props.className "description"] [text "Import data from another Clipperz account using a JSON/HTML export file created by Clipperz."]
      <|>
      div [Props.className "importInput"] [
        dragAndDropFileInputWidget <#> (\file -> Next $ state {content = Left file})
      , p [Props.className "description"] [text "Alternatively you may type or paste any properly formatted JSON data."]
      , demand do
        textContent <- simpleTextAreaSignal "importText" (text "Import") "Type or copy your data here" (fromRight "" content)
        fireOnce (navigateImportStepsView state (state {content = Right textContent}))
      ]
    

    importInternalView Selection = demand $ do
      selectionInfo <- loopS { filter: NonArchived, tag: snd tag, tagSelected: true } $ \v -> do
        filter' <- loopW v.filter (\_ -> div [Props.className "selectButtons"] [
                                          span [] [text "Select:"]
                                        , a [Props.className "all",          Props.onClick] [text "All"]          $> All
                                        , a [Props.className "none",         Props.onClick] [text "None"]         $> None
                                        , a [Props.className "archived",     Props.onClick] [text "Archived"]     $> Archived
                                        , a [Props.className "not_archived", Props.onClick] [text "Not Archived"] $> NonArchived
                                        ])
        div_ [Props.className "tagButtons"] do
          tagSelected' <- loopW v.tagSelected (\v_ -> label [Props.className "apply_tag"] [
                          (not v_) <$  input [
                            Props._type "checkbox"
                          , Props.checked v_
                          , Props.onChange
                          ]
                        , span [Props.className "label"] [text "Apply the following tag to imported cards:"]
                        ])
          tag'   <- loopW v.tag (simpleTextInputWidget "tag" (text "Tag") "Tag")
          pure $ { filter: filter', tag: tag', tagSelected: tagSelected' }
      
      selectedCards <- ul_ [] do
        ((\(Tuple selected card) -> 
            li_ [] do
              loopW selected (\v ->
                label [Props.className "select_card"] [
                  span  [Props.className "label"] [
                    cardContentView card $ if selectionInfo.tagSelected 
                                           then Just selectionInfo.tag
                                           else Nothing
                  ]
                , input [Props._type "checkbox", Props.checked v, Props.onChange] $> (not v)
                ]
              ) <#> (Tuple card >>> swap)
        ) <$> filterCards (selectionInfo.filter) selection) # sequence

      let updatedState = state {selection = selectedCards, tag = Tuple selectionInfo.tagSelected selectionInfo.tag}

      fireOnce (navigateImportStepsView (updatedState {step = Upload}) updatedState)

    
    importInternalView Confirm = do
      h5 [] [text $ "Import " <> (show $ length $ filter fst selection) <> " cards (of " <> (show $ length selection) <> ")?"]
      <|>
      simpleButton "import" "Import" false (Next state)
      <|>
      navigateImportStepsView (state {step = Selection}) state

    -- --------------------------------------------------

    navigateImportStepsView :: ImportState -> ImportState -> Widget HTML NavigationAction
    navigateImportStepsView backState nextState = 
      div [Props.className "importButtons"] [
        button [Props.onClick, Props.disabled isBackDisabled, Props.classList [Just "back", if isBackDisabled then Just "hide" else Nothing]] [span [] [text "previous"]] $> Back backState
      , button [Props.onClick, Props.disabled isNextDisabled, Props.classList [Just "next", if isNextDisabled then Just "hide" else Nothing]] [span [] [text "next"]]     $> Next nextState
      ]

      where
        isBackDisabled :: Boolean
        isBackDisabled = case step of
                          Upload -> true
                          _      -> false

        isNextDisabled :: Boolean
        isNextDisabled = case step of
                          Confirm -> true
                          _       -> false

    filterCards :: QuickSelection -> Array (Tuple Boolean Card) -> Array (Tuple Boolean Card)
    filterCards All         = map (\(Tuple _ c)          -> Tuple  true            c)
    filterCards None        = map (\(Tuple _ c)          -> Tuple  false           c)
    filterCards Archived    = map (\(Tuple _ c@(Card r)) -> Tuple  r.archived      c)
    filterCards NonArchived = map (\(Tuple _ c@(Card r)) -> Tuple (not r.archived) c)

    cardContentView :: forall a. Card -> Maybe String -> Widget HTML a
    cardContentView (Card {content: (CardValues {title: t, tags: ts, fields: fs, notes: n})}) newTag = 
      div [Props.className "cardContent"] [
        h3 [Props.className "card_title"]    [text t]
      , ul [Props.className "card_tags"]   $ (\s -> li' [text s]) <$> (maybe ts (flip insert ts) newTag # toUnfoldable)
      , dl [Props.className "card_fields"] $  concat $ (\(CardField {name, value, locked}) -> [
            dt [] [text name]
          , dd [Props.classList [if locked then (Just "password") else Nothing]] [
              text value
            ]
        ]) <$> fs
      , p [Props.className "card_notes"] [text n]
      ]

    dragAndDropFileInputWidget :: Widget HTML (Maybe File)
    dragAndDropFileInputWidget = do
      dropDiv false

      where 
        dropDiv highlight = do
          res <- div  [ Props.classList (Just <$> (["dropArea"] <> if highlight then ["highlight"] else []))
                      , Props._id "import"
                      , Props.onDragEnter   <#> DragEnter 
                      , Props.onDragLeave   <#> DragLeave 
                      , Props.onDropCapture <#> Drop      
                      ]
                      [ span [] [text "Drag your Clipperz export file here"], br'
                      , span [] [text "or"], br'
                      , label [Props.className "importButton"] [
                          span [Props.className "label"] [a' [text "select it manually"]]
                        , input [
                            Props._type "file"
                          , Props.onChange
                          , Props.accept ".html"
                          ] >>= fromSyntheticEvent
                        ] <#> (items >>> head >>> File)
                      ]
          case res of
            DragEnter _    -> dropDiv true
            DragLeave _    -> dropDiv false
            Drop      a    -> (getFileFromDrop a) <#> (items >>> head)
            File      file -> pure file

        fromSyntheticEvent :: forall r. SyntheticEvent_ (currentTarget :: NativeEventTarget | r) -> Widget HTML FileList
        fromSyntheticEvent  se = pure $ (unsafeCoerce se).target.files

        getFileFromDrop :: forall r. SyntheticEvent_ (currentTarget :: NativeEventTarget | r) -> Widget HTML FileList
        getFileFromDrop se = pure $ (unsafeCoerce se).dataTransfer.files
