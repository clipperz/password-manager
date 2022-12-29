module Views.SimpleWebComponents
  ( LoopableWidget
  , PasswordForm
  , SubMenuAction(..)
  , SubmenuVoice
  , checkboxesSignal
  , clickableListItemWidget
  , complexMenu
  , complexMenu'
  , MenuStatus(..)
  , confirmationWidget
  , disableOverlay
  , disabledSimpleTextInputWidget
  , dragAndDropAndRemoveList
  , dragAndDropFileInputWidget
  , dragAndDropList
  , dragAndDropList'
  , dragAndDropListSignal
  , loadingBar
  , loadingDiv
  , passwordStrengthShow
  , simpleButton
  , simpleButtonWithClass
  , simpleButtonWithId
  , simpleCheckboxSignal
  , simpleCheckboxWidget
  , simpleFileInputWidget
  , simpleInputWidget
  , simpleNumberInputWidget
  , simplePasswordInputWidget
  , simplePasswordSignal
  , simpleTextAreaSignal
  , simpleTextAreaWidget
  , simpleTextInputWidget
  , simpleTextInputWidgetWithFocus
  , simpleUserSignal
  , simpleVerifiedPasswordSignal
  , submenu
  , submenu'
  )
  where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, loopW, demand, debounce, loopS, display, step)
import Concur.React (HTML)
import Concur.React.DOM (text, textarea, input, label, div', div, button, ul, li)
import Concur.React.Props as Props
import Control.Alt (class Alt, (<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (=<<), (>>=))
import Control.Semigroupoid ((<<<))
import Data.Array (length, zipWith, range, catMaybes, updateAt, deleteAt, insertAt, filter, intersperse, drop, take, sortWith, union, dropEnd, takeEnd, (!!), (:))
import Data.Bifunctor (lmap, rmap)
import Data.Boolean (otherwise)
import Data.Either (Either(..), hush)
import Data.Enum (enumFromThenTo)
import Data.EuclideanRing ((/))
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$), (<$>), class Functor)
import Data.HeytingAlgebra (not)
import Data.Int (even, odd)
import Data.Map (Map, lookup)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Monoid (power)
import Data.Ord ((>))
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Semiring ((+), (*))
import Data.Show (show, class Show)
import Data.String (joinWith)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)
import Data.Unit (unit)
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Events (readFile, readFileFromDrop, getClickCoordinates, printEvent)
import Functions.Password (PasswordStrengthFunction, PasswordStrength)
import Functions.Time (getCurrentTimestamp)
import React.SyntheticEvent (currentTarget, preventDefault, SyntheticEvent_, NativeEventTarget, SyntheticMouseEvent)

import Debug (traceM)

loadingBar :: Int -> Int -> Int -> String
loadingBar current total width = 
  let fullCh = (width * current) / total
      full = power "█" fullCh
      empty = power "_" (width - fullCh)
  in full <> empty <> "% (" <> (show current) <> " of " <> (show total) <> ")"

simpleTextAreaWidget :: String -> Widget HTML String -> String -> String -> Widget HTML String
simpleTextAreaWidget id lbl placeholder content = do
  div [Props.className "textarea"] [
    label [Props.htmlFor id, Props.className "hide-element"] [lbl]
    , Props.unsafeTargetValue <$> textarea [
        Props.value content
      , Props.onChange
      , Props.placeholder placeholder
    ] []
  ]

simpleInputWidget :: String -> Widget HTML String -> Boolean -> String -> String -> String -> Widget HTML String
simpleInputWidget id lbl disable placeholder value t = do
  res <- div' [
      label [Props.htmlFor id] [lbl]
    , (Props.unsafeTargetValue) <$> input [
        Props._type t
      , Props._id id
      , Props.placeholder placeholder
      , Props.value value
      , Props.disabled disable
      , Props.onChange
      ]
  ]
  pure res

simpleFileInputWidget :: String -> Widget HTML String -> Widget HTML String
simpleFileInputWidget id lbl = do
  div' [
      label [Props.htmlFor id] [lbl]
    , fromSyntheticEvent =<< input [
        Props._type "file"
      , Props._id id
      , Props.onChange
      ]
  ]

  where 
    fromSyntheticEvent :: forall r. SyntheticEvent_ (currentTarget :: NativeEventTarget | r) -> Widget HTML String
    fromSyntheticEvent se = do
      nve <- liftEffect $ currentTarget se
      liftAff $ readFile nve

data DragFileEvents a = DragEnter a | DragLeave a | Drop a | FileContent String

dragAndDropFileInputWidget :: String -> String -> Widget HTML String
dragAndDropFileInputWidget id lbl = do
  dropDiv false

  where 
    dropDiv highlight = do
      res <- div [ Props.classList (Just <$> (["dropFile"] <> if highlight then ["highlight"] else []))
                 , Props._id id
                 , DragEnter <$> Props.onDragEnter
                 , DragLeave <$> Props.onDragLeave
                 , Drop <$> Props.onDropCapture] [ FileContent <$> (simpleFileInputWidget "importButton" (text lbl)) ]
      case res of
        DragEnter _ -> dropDiv true
        DragLeave _ -> dropDiv false
        Drop a -> liftAff $ readFileFromDrop a
        FileContent s -> pure s

simpleTextInputWidgetWithFocus :: String -> Widget HTML String -> String -> String -> Widget HTML String
simpleTextInputWidgetWithFocus id lbl placeholder s = do
  res <- div' [
      label [Props.htmlFor id] [lbl]
    , input [
        Props._type "text"
      , Props._id id
      , Props.placeholder placeholder
      , Props.value s
      , Props.disabled false
      , Props.unsafeTargetValue <$> Props.onChange
      , Props.unsafeTargetValue <$> Props.onFocus
      ]
  ]
  pure res

simpleTextInputWidget :: String -> Widget HTML String -> String -> String -> Widget HTML String
simpleTextInputWidget id lbl placeholder s = simpleInputWidget id lbl false placeholder s "text"

disabledSimpleTextInputWidget :: String -> Widget HTML String -> Boolean -> String -> String -> Widget HTML String
disabledSimpleTextInputWidget id lbl disable placeholder s = simpleInputWidget id lbl disable placeholder s "text"

simplePasswordInputWidget :: String -> Widget HTML String -> String -> Widget HTML String
simplePasswordInputWidget id lbl s = simpleInputWidget id lbl false "password" s "password"

simpleNumberInputWidget :: String -> Widget HTML String -> String -> String -> Widget HTML String
simpleNumberInputWidget id lbl placeholder s = simpleInputWidget id lbl false placeholder s "number"

simpleCheckboxWidget :: String -> Widget HTML Boolean -> Boolean -> Boolean -> Widget HTML Boolean 
simpleCheckboxWidget id lbl lblOnLeft v = do
  res <- if lblOnLeft then div' [
    (not v) <$ input [
        Props._type "checkbox"
      , Props._id id 
      , Props.checked v
      , Props.onChange
      ]
    , label [Props.htmlFor id] [lbl]
  ]
  
  else div' [
      label [Props.htmlFor id] [lbl]
    , (not v) <$ input [
        Props._type "checkbox"
      , Props._id id 
      , Props.checked v
      , Props.onChange
      ]
  ]
  pure res

simpleButton :: forall a. String -> Boolean -> a -> Widget HTML a
simpleButton label disable value = button [value <$ Props.onClick, Props.disabled disable] [text label]

simpleButtonWithId :: forall a. String -> String -> Boolean -> a -> Widget HTML a
simpleButtonWithId id label disable value = button [Props._id id, value <$ Props.onClick, Props.disabled disable] [text label]

simpleButtonWithClass :: forall a. String -> String -> Boolean -> a -> Widget HTML a
simpleButtonWithClass label classes disable value = button [value <$ Props.onClick, Props.disabled disable, Props.className classes] [text label]

simpleTextAreaSignal :: String -> Widget HTML String -> String -> String -> Signal HTML String
simpleTextAreaSignal id label placeholder content = loopW content (simpleTextAreaWidget id label placeholder)

simpleUserSignal :: String -> Signal HTML String
simpleUserSignal u = loopW u (simpleTextInputWidget "username" (text "Username") "username")

simplePasswordSignal :: String -> Signal HTML String
simplePasswordSignal p = loopW p (simplePasswordInputWidget "password" (text "Password"))

simpleCheckboxSignal :: String -> Widget HTML Boolean -> Boolean -> Boolean -> Signal HTML Boolean
simpleCheckboxSignal id lbl lblOnLeft v = loopW v (simpleCheckboxWidget id lbl lblOnLeft)

type PasswordForm = { password       :: String
                    , verifyPassword :: String
                    }
simpleVerifiedPasswordSignal :: PasswordStrengthFunction -> Either PasswordForm String -> Signal HTML (Either PasswordForm String)
simpleVerifiedPasswordSignal psf f = loopS f $ \ef ->
  case ef of
    Left { password, verifyPassword } -> go password verifyPassword
    Right p -> go p p
    where go p vp = do
                      pswd <- loopW p (simplePasswordInputWidget "password" (text "Password"))
                      display $ passwordStrengthShow $ psf pswd
                      pswd2 <- loopW vp (simplePasswordInputWidget "verify_password" (text "Verify password"))
                      display $ text $ if pswd == pswd2 then "The passwords are the same" else "The passwords are not the same"
                      if pswd == pswd2 then
                        pure $ Right pswd
                      else 
                        pure $ Left { password: pswd, verifyPassword: pswd2 }

checkboxesSignal :: Array (Tuple String Boolean) ->  Map String (Widget HTML Boolean) -> Signal HTML (Array (Tuple String Boolean))
checkboxesSignal ts lablesMap = loopS ts \m -> do
  let checkboxes = ((\(Tuple id value) -> Tuple id (simpleCheckboxSignal id (fromMaybe (text "Label not found") (lookup id lablesMap)) false value)) <$> m) :: Array (Tuple String (Signal HTML Boolean))
  let checkboxes2 = ((\(Tuple id s) -> do
                            res <- s
                            pure $ Tuple id res) <$> checkboxes) :: Array (Signal HTML (Tuple String Boolean))
  sequence checkboxes2

clickableListItemWidget :: forall a. Boolean -> Widget HTML a -> Array String -> a -> Widget HTML a
clickableListItemWidget disable item classes reference = 
  let classProps = Props.className (joinWith " " classes)
  in li (classProps : [if disable then Props.emptyProp else reference <$ Props.onClick]) [item]

passwordStrengthShow :: forall a. PasswordStrength -> Widget HTML a
passwordStrengthShow = text <<< show

loadingDiv :: forall a. Widget HTML a
loadingDiv = div [ (Props.className "loading") ] [
  div [Props.className "lds-spinner"] [
    div [] []
  , div [] []
  , div [] []
  , div [] []
  , div [] []
  , div [] []
  , div [] []
  , div [] []
  , div [] []
  , div [] []
  , div [] []
  , div [] []
  ]
]  

disableOverlay :: forall a. Widget HTML a
disableOverlay = div [(Props.className "disableOverlay")] []

confirmationWidget :: String -> Widget HTML Boolean
confirmationWidget message = div [(Props.className "disableOverlay")] [
  text message
, simpleButton "Yes" false true
, simpleButton "No" false false
]

data MenuStatus = MenuOpen | MenuClosed
data SubMenuAction  a = OpenCloseMenu | ClickOnVoice a
data SubMenuAction' a = OpenSubMenu'  | CloseSubMenu' | ClickOnVoice' a

type SubmenuVoice' a = Tuple MenuStatus (MenuStatus -> Widget HTML a)

submenu' :: forall a b. MenuStatus -> Widget HTML b -> Array (Widget HTML a) -> Widget HTML a
submenu' MenuClosed b1 bs = do
  res <- OpenSubMenu' <$ div [] [b1]
  case res of
    OpenSubMenu'  -> submenu' MenuOpen   b1 bs
    CloseSubMenu' -> submenu' MenuClosed b1 bs
    ClickOnVoice' a -> pure a
submenu' MenuOpen b1 bs = do
  res <- div [] $ [CloseSubMenu' <$ b1] <> (((<$>) ClickOnVoice') <$> bs)
  case res of
    OpenSubMenu'  -> submenu' MenuOpen   b1 bs
    CloseSubMenu' -> submenu' MenuClosed b1 bs
    ClickOnVoice' a -> pure a

complexMenu' :: forall a. Array (SubmenuVoice' a) -> Array (Widget HTML (Tuple (Array MenuStatus) a))
complexMenu' arr = 
  let indexes   = range 0 ((length arr) - 1)                              :: Array Int
      newArr    = zipWith (\i -> \t -> { index: i, tuple: t}) indexes arr :: Array {index::Int, tuple::SubmenuVoice' a}
      menuStati = fst <$> arr                                             :: Array MenuStatus
      mapFunc = \{index, tuple: (Tuple open f)} -> (\v -> Tuple (fromMaybe menuStati (updateAt index MenuOpen menuStati)) v) <$> f open
  in mapFunc <$> newArr


submenu :: forall a b. Boolean -> Widget HTML b -> Array (Widget HTML a) -> Widget HTML (Either Boolean a)
submenu showing b1 bs = do
  -- res <- div [] $ [OpenCloseMenu <$ b1] <> (if showing then (((<$>) ClickOnVoice) <$> bs) else [])
  let showingProp = if showing then [Props.className "userSidebarSubitems"] else [Props.className "userSidebarSubitems hidden"]
  res <- li [] $ [OpenCloseMenu <$ b1] <> [(ul showingProp (((<$>) ClickOnVoice) <$> bs))]
  case res of
    OpenCloseMenu -> pure $ Left (not showing)
    ClickOnVoice a -> pure $ Right a

--  Each `SubmenuVoice` can be thought as a menu section, with an open/close status
type SubmenuVoice a = Tuple Boolean (Boolean -> Widget HTML (Either Boolean a))

type IntermediateWidget a = Widget HTML (Either (Array Boolean) a)

--  Given a series of sections (arr :: Array (SumbenuVoice a)) they are bound to an index, so that a sorted list of sections is available (newArr)
complexMenu :: forall a. Maybe String -> Maybe String -> Array (SubmenuVoice a) -> Widget HTML (Tuple (Array (SubmenuVoice a)) a)
complexMenu mId mClass arr = do
  let pid    = fromMaybe [] ((\id -> [Props._id id])      <$> mId)
  let pclass = fromMaybe [] (( \c -> [Props.className c]) <$> mClass) 
  inter <- ul (pid <> pclass) (fromVoiceToWidget <$> newArr)
  case inter of
    Right a   -> pure $ Tuple arr a   --  If a subitem is selected, it is returned with the whole status (open/close) of all other sections.
    Left  bs  -> complexMenu mId mClass $ redraw bs

  where
    indexes :: Array Int
    indexes = range 0 ((length arr) - 1)

    newArr :: Array {index :: Int , tuple :: SubmenuVoice a }
    newArr = zipWith (\i -> \t -> { index: i, tuple: t}) indexes arr

    booleans :: Array Boolean
    booleans = fst <$> arr
    
    --  new stati are bound to function for drawing widgets
    redraw :: Array Boolean -> Array (SubmenuVoice a)
    redraw newBoolean = zipWith (\b -> \(Tuple _ f) -> Tuple b f) newBoolean arr

    --  Ogni sezione della serie ordinata è quindi descritta da:
    --  Each section in the sorted serires is thus defined with:
    --  - position on the menu 
    --  - whether it is open or close
    --  - a function that can draw the matching widget, given its status (open/close)
    --  The menu item widget may return either a change in status of the widget itself (open/close) or whether a subitems has been clicked.
    fromVoiceToWidget :: { index :: Int, tuple :: SubmenuVoice a } -> Widget HTML (Either (Array Boolean) a)
    fromVoiceToWidget { index, tuple: (Tuple b f) } = do
      res <- f b
      case res of 
        Left bool -> pure $ Left  $ fromMaybe booleans (updateAt index bool booleans) --  If a section is opened/closed the array with all the stati of the sections,
                                                                                      --  and the item at the matching position is updated
        Right a   -> pure $ Right a   --  If a subitem is selected, it is returned with the whole status (open/close) of all other sections.

------------------------------------

data OnDraggableEvents a b = StartDrag a | EndDrag a | Dragging a | Value b
instance showOnDraggableEvents :: Show (OnDraggableEvents a b) where
  show (StartDrag a)  = "StartDrag"
  show (EndDrag   a)  = "EndDrag"
  show (Dragging  a)  = "Dragging"
  show (Value     b)  = "Value"

data OnDropAreaEvents a = EvDrop a | EvDragEnter a | EvDragLeave a | EvDragOver a
instance showOnDropAreaEvents :: Show (OnDropAreaEvents a) where
  show (EvDrop      a)  = "EvDrop"
  show (EvDragEnter a)  = "EvDragEnter"
  show (EvDragLeave a)  = "EvDragLeave"
  show (EvDragOver  a)  = "EvDragOver"    --  menu is redrawn [366?]

newtype DraggableWidgetResult a = DraggableWidgetResult { isDragging :: Boolean, exitState :: a }
instance showDraggableWidgetResult :: Show (DraggableWidgetResult a) where
  show (DraggableWidgetResult {isDragging}) = "DWR " <> (show isDragging)

draggableWidget :: forall a. Boolean -> a -> (a -> Widget HTML a) -> Widget HTML (DraggableWidgetResult a)
draggableWidget isDragging initialState widgetFunc = do
  res <- div ([ Props.draggable true
              , Props.classList [Just "draggableElem", (if isDragging then Just "draggingElem" else Nothing)]
              ]
              <> if isDragging then [EndDrag <$> Props.onDragEnd] else [StartDrag <$> Props.onDragStart]
              ) 
              [Value <$> (widgetFunc initialState), text (show isDragging)]
  case res of
    -- do not prevent defaults, otherwise strange behaviours occur
    StartDrag ev -> pure $ DraggableWidgetResult { isDragging: true, exitState: initialState }
    EndDrag   ev -> pure $ DraggableWidgetResult { isDragging: false, exitState: initialState }
    Dragging  ev -> pure $ DraggableWidgetResult { isDragging: true, exitState: initialState }
    Value     a  -> pure $ DraggableWidgetResult { isDragging, exitState: a }

data RemovableDraggableWidgetResult a = Remove | Result (DraggableWidgetResult a)

removableDraggableWidget :: forall a. Boolean -> a -> (a -> Widget HTML a) -> Widget HTML (RemovableDraggableWidgetResult a)
removableDraggableWidget isDragging initialState widgetFunc = do
  div ([ Props.draggable true
       , Props.classList [Just "draggableElem", (if isDragging then Just "draggingElem" else Nothing)]]
       <> if isDragging then 
           [(Result (DraggableWidgetResult { isDragging: false, exitState: initialState })) <$ Props.onDragEnd] 
         else 
           [(Result (DraggableWidgetResult { isDragging: true, exitState: initialState })) <$ Props.onDragStart]
       ) 
      [ simpleButton "x" false Remove
      , (\a -> Result (DraggableWidgetResult { isDragging, exitState: a })) <$> (widgetFunc initialState)
      ]

type DroppableAreaResult = { isSelected :: Boolean, result :: (OnDropAreaEvents SyntheticMouseEvent) }
type IndexedResult a = {index :: Int, result :: a}

droppableArea :: Boolean -> Widget HTML DroppableAreaResult
droppableArea isSelected = do
  result <- div [ Props.classList [Just "dropzone", (if isSelected then Just "selected" else Nothing)]
                , EvDrop <$> Props.onDrop
                , EvDragLeave <$> Props.onDragLeave
                , EvDragEnter <$> Props.onDragEnter
                -- , EvDragOver <$> Props.onDragOver -- managed by js for performance
                ] []
  case result of
    EvDragOver ev -> do
      liftEffect $ preventDefault ev
      droppableArea isSelected
    EvDragEnter ev -> do
      pure { isSelected: true, result }
    EvDragLeave ev -> do
      pure { isSelected: false, result }
    EvDrop ev -> do
      liftEffect $ preventDefault ev
      pure { isSelected: false, result }

type DraggableWidget                a = Widget HTML (DraggableWidgetResult a)
type DraggableWidgetType            a = { widgetFunc :: a -> Widget HTML a, widget :: DraggableWidget a } 
type RemovableDraggableWidget       a = Widget HTML (RemovableDraggableWidgetResult a)
type RemovableDraggableWidgetType   a = { widgetFunc :: a -> Widget HTML a, widget :: RemovableDraggableWidget a } 
type DroppableWidget                  = Widget HTML DroppableAreaResult
type DraggableSupportType           a = { widgetFunc :: a -> Widget HTML a, result :: DraggableWidgetResult a }
type RemovableDraggableSupportType  a = { widgetFunc :: a -> Widget HTML a, result :: RemovableDraggableWidgetResult a }
type SelectedDraggableInfo          a = { index :: Int, state :: a, widgetFunc :: (a -> Widget HTML a) }

-- | TODO: THIS IS BROKEN
dragAndDropList :: forall a. Array (Tuple a (a -> Widget HTML a)) -> Widget HTML a
dragAndDropList widgets = do
  let widgetRange = filter odd $ range 1 ((length widgets) * 2)
  let draggableWidgets = zipWith (\index -> \widget -> {index, widget}) widgetRange ((Right <<< (mapWidget false)) <$> widgets) :: Array { index :: Int, widget :: Either DroppableWidget (DraggableWidgetType a) }
  let dropAreaRange = filter even $ range 0 ((length widgets) * 2)
  let droppableAreas = zipWith (\index -> \widget -> {index, widget}) dropAreaRange ((Left <<< (\_ -> droppableArea false)) <$> dropAreaRange) :: Array { index :: Int, widget :: Either DroppableWidget (DraggableWidgetType a) }
  let sortedWidgets = sortWith (_.index) $ (draggableWidgets <> droppableAreas)
  let widgetList = (_.widget) <$> sortedWidgets
  let elements' = ((rmap mapDraggableWidget) <$> widgetList) :: Array (Either DroppableWidget (Widget HTML (DraggableSupportType a)))
  let widgets = (includeEither <$> elements') :: Array (Widget HTML (Either DroppableAreaResult (DraggableSupportType a)))
  go widgets Nothing

  where
    mapWidget :: Boolean -> Tuple a (a -> Widget HTML a) -> (DraggableWidgetType a)
    mapWidget isDragging (Tuple is widgetFunc) = 
      let widget = draggableWidget isDragging is widgetFunc
      in { widgetFunc, widget }

    manageDroppableAreaResult :: DroppableAreaResult  -> Tuple (OnDropAreaEvents SyntheticMouseEvent) DroppableWidget
    manageDroppableAreaResult { isSelected, result } = 
      let droppableWidget = droppableArea isSelected
      in Tuple result droppableWidget

    manageDraggableWidgetResult :: DraggableWidgetResult a -> DraggableWidgetType a -> Tuple a (DraggableWidgetType a)
    manageDraggableWidgetResult (DraggableWidgetResult { isDragging, exitState }) { widgetFunc, widget } =
      let newWidget = mapWidget isDragging (Tuple exitState widgetFunc)
      in Tuple exitState newWidget

    includeEither :: forall m b c. Functor m => Either (m b) (m c) -> m (Either b c)
    includeEither (Left w) = Left <$> w
    includeEither (Right w) = Right <$> w

    mapDraggableWidget :: DraggableWidgetType a -> Widget HTML (DraggableSupportType a)
    mapDraggableWidget { widgetFunc, widget } = (\result -> { widgetFunc, result }) <$> widget

    prepareNewElements :: Array (Widget HTML (Either DroppableAreaResult (DraggableSupportType a))) 
                       -> SelectedDraggableInfo a
                       -> Int
                       -> Array (Widget HTML (Either DroppableAreaResult (DraggableSupportType a)))
    prepareNewElements elements { index: dragIndex, state, widgetFunc } dropIndex 
      | dragIndex > dropIndex =
          let untouchedFirstElems = take (dropIndex + 1) elements
              untouchedLastElems = drop (dragIndex + 1) elements
              elemsToBeShifted = takeEnd (dragIndex - dropIndex - 1) $ take (dragIndex - 1) elements
          in case elements !! dragIndex of
              Nothing -> elements
              Just elem -> 
                let newElem = Right <$> (mapDraggableWidget $ mapWidget false (Tuple state widgetFunc))
                in untouchedFirstElems
                  <> [newElem]
                  <> elemsToBeShifted
                  <> untouchedLastElems
      | otherwise = 
          let untouchedFirstElems = take (dragIndex - 1) elements
              untouchedLastElems = drop dropIndex elements
              elemsToBeShifted = takeEnd (dropIndex - dragIndex - 1) $ take dropIndex elements
          in case elements !! dragIndex of
              Nothing -> elements
              Just elem -> 
                let newElem = Right <$> (mapDraggableWidget $ mapWidget false (Tuple state widgetFunc))
                in untouchedFirstElems
                  <> elemsToBeShifted
                  <> [includeEither $ Left $ droppableArea false]
                  <> [newElem]
                  <> untouchedLastElems

    go :: Array (Widget HTML (Either DroppableAreaResult (DraggableSupportType a))) -> Maybe (SelectedDraggableInfo a) -> Widget HTML a
    go widgets selectedIndex = do
      let zipped = zipWith (\i -> \w -> { index: i, wi: w}) (range 0 (length widgets)) widgets
      let newWidgets = (\{index, wi} -> (\r -> {index, res: r}) <$> wi) <$> zipped
      {index, res} <- div [Props.className "dragAndDropList"] newWidgets
      case res of 
        Left { isSelected, result } -> do
          case result of
            EvDrop a -> do
              case selectedIndex of
                Just selectedInfo -> do
                  let newElem = includeEither $ Left $ droppableArea false
                  let newElements = updateAt index newElem widgets
                  case newElements of
                    Nothing -> do
                      log "error 0.5"
                      go widgets selectedIndex
                    Just elements -> do
                      let newElements' = prepareNewElements elements selectedInfo index -- TODO
                      go newElements' Nothing
                Nothing -> do
                  log "error1"
                  go widgets selectedIndex
            EvDragEnter a -> do
              let newElem = includeEither $ Left $ droppableArea true
              let newElements = updateAt index newElem widgets
              case newElements of
                Nothing -> do
                  log "error2"
                  go widgets selectedIndex
                Just elements' -> go elements' selectedIndex
            EvDragLeave a -> do
              let newElem = includeEither $ Left $ droppableArea false
              let newElements = updateAt index newElem widgets
              case newElements of
                Nothing -> do
                  log "error3"
                  go widgets selectedIndex
                Just elements' -> go elements' selectedIndex
            EvDragOver a -> do
              let newElem = includeEither $ Left $ droppableArea true
              let newElements = updateAt index newElem widgets
              case newElements of
                Nothing -> do
                  log "error4"
                  go widgets selectedIndex
                Just elements' -> go elements' selectedIndex  
        Right { widgetFunc, result: DraggableWidgetResult { isDragging, exitState } } -> do
          let newElem = (mapDraggableWidget (mapWidget isDragging (Tuple exitState widgetFunc))) :: Widget HTML (DraggableSupportType a)
          let newElem' = includeEither $ Right newElem
          let newElements = updateAt index newElem' widgets
          case newElements of
            Nothing -> do
              log "error5"
              go widgets selectedIndex
            Just elements' -> go elements' (if isDragging then (Just {index, state: exitState, widgetFunc}) else selectedIndex)

type LoopableWidget a = Tuple a (a -> Widget HTML a)

dragAndDropListSignal :: forall a. Array (LoopableWidget a) -> Signal HTML (Array (LoopableWidget a))
dragAndDropListSignal widgets = loopW widgets dragAndDropList'

dragAndDropList' :: forall a. Array (LoopableWidget a) -> Widget HTML (Array (LoopableWidget a))
dragAndDropList' widgets = do
  let widgetsInfo = [Left false] <> (intersperse (Left false) $ Right <$> widgets) <> [Left false]
  go widgetsInfo Nothing

  where
    mapWidget :: Boolean -> Tuple a (a -> Widget HTML a) -> (DraggableWidgetType a)
    mapWidget isDragging (Tuple is widgetFunc) = 
      let widget = draggableWidget isDragging is widgetFunc
      in { widgetFunc, widget }

    manageDroppableAreaResult :: DroppableAreaResult  -> Tuple (OnDropAreaEvents SyntheticMouseEvent) DroppableWidget
    manageDroppableAreaResult { isSelected, result } = 
      let droppableWidget = droppableArea isSelected
      in Tuple result droppableWidget

    manageDraggableWidgetResult :: DraggableWidgetResult a -> DraggableWidgetType a -> Tuple a (DraggableWidgetType a)
    manageDraggableWidgetResult (DraggableWidgetResult { isDragging, exitState }) { widgetFunc, widget } =
      let newWidget = mapWidget isDragging (Tuple exitState widgetFunc)
      in Tuple exitState newWidget

    includeEither :: forall m b c. Functor m => Either (m b) (m c) -> m (Either b c)
    includeEither (Left w) = Left <$> w
    includeEither (Right w) = Right <$> w

    mapDraggableWidget :: DraggableWidgetType a -> Widget HTML (DraggableSupportType a)
    mapDraggableWidget { widgetFunc, widget } = (\result -> { widgetFunc, result }) <$> widget

    prepareNewElements :: Array (Widget HTML (Either DroppableAreaResult (DraggableSupportType a))) 
                       -> SelectedDraggableInfo a
                       -> Int
                       -> Array (Widget HTML (Either DroppableAreaResult (DraggableSupportType a)))
    prepareNewElements elements { index: dragIndex, state, widgetFunc } dropIndex 
      | dragIndex > dropIndex =
          let untouchedFirstElems = take (dropIndex + 1) elements
              untouchedLastElems = drop (dragIndex + 1) elements
              elemsToBeShifted = takeEnd (dragIndex - dropIndex - 1) $ take (dragIndex - 1) elements
          in case elements !! dragIndex of
              Nothing -> elements
              Just elem -> 
                let newElem = Right <$> (mapDraggableWidget $ mapWidget false (Tuple state widgetFunc))
                in untouchedFirstElems
                  <> [newElem]
                  <> elemsToBeShifted
                  <> untouchedLastElems
      | otherwise = 
          let untouchedFirstElems = take (dragIndex - 1) elements
              untouchedLastElems = drop dropIndex elements
              elemsToBeShifted = takeEnd (dropIndex - dragIndex - 1) $ take dropIndex elements
          in case elements !! dragIndex of
              Nothing -> elements
              Just elem -> 
                let newElem = Right <$> (mapDraggableWidget $ mapWidget false (Tuple state widgetFunc))
                in untouchedFirstElems
                  <> elemsToBeShifted
                  <> [includeEither $ Left $ droppableArea false]
                  <> [newElem]
                  <> untouchedLastElems

    convertToLoopableWidget :: Either DroppableAreaResult (DraggableSupportType a) -> Maybe (LoopableWidget a)
    convertToLoopableWidget (Left _) = Nothing
    convertToLoopableWidget (Right { widgetFunc, result: (DraggableWidgetResult { isDragging, exitState })}) = Just (Tuple exitState widgetFunc)

    convertToResult :: Array (Widget HTML (Either DroppableAreaResult (DraggableSupportType a))) -> Widget HTML (Array (LoopableWidget a))
    convertToResult arr =
      let sequencedWidgets = sequence arr
      in (\eithers -> catMaybes (convertToLoopableWidget <$> eithers)) <$> sequencedWidgets

    go :: Array (Either Boolean (LoopableWidget a)) -> Maybe (SelectedDraggableInfo a) -> Widget HTML (Array (LoopableWidget a))
    go widgetsInfo selectedIndex = do
      let widgetsInfo' = ((lmap droppableArea) <$> widgetsInfo) :: Array (Either DroppableWidget (LoopableWidget a))
      let widgetsInfo'' = ((rmap (mapWidget false)) <$> widgetsInfo') :: Array (Either DroppableWidget (DraggableWidgetType a))
      let widgets' = ((rmap mapDraggableWidget) <$> widgetsInfo'') :: Array (Either DroppableWidget (Widget HTML (DraggableSupportType a)))
      let widgets'' = (includeEither <$> widgets') :: Array (Widget HTML (Either DroppableAreaResult (DraggableSupportType a)))
      let zipped = zipWith (\i -> \w -> { index: i, wi: w}) (range 0 (length widgets'')) widgets''
      let newWidgets = (\{index, wi} -> (\r -> {index, res: r}) <$> wi) <$> zipped
      {index, res} <- div [Props.className "dragAndDropList"] newWidgets
      -- pure $ catMaybes $ hush <$> widgetsInfo
      case res of 
        Left { isSelected, result } -> do
          case result of
            EvDrop a -> do
              case selectedIndex of
                Just { state, index: ix, widgetFunc } -> do
                  let withoutElem = (deleteAt ix widgetsInfo)
                  let withElem    = (withoutElem >>= (insertAt index (Right (Tuple state widgetFunc))))
                  case withElem of
                    Nothing -> do
                      log "error 0.5"
                      go widgetsInfo selectedIndex
                    Just elements -> pure $ catMaybes $ hush <$> elements
                Nothing -> do
                  log "error1"
                  go widgetsInfo selectedIndex
            EvDragEnter a -> do
              let newElements = updateAt index (Left true) widgetsInfo
              case newElements of
                Nothing -> do
                  log "error2"
                  go widgetsInfo selectedIndex
                Just elements' -> go elements' selectedIndex
            EvDragLeave a -> do
              let newElements = updateAt index (Left false) widgetsInfo
              case newElements of
                Nothing -> do
                  log "error2"
                  go widgetsInfo selectedIndex
                Just elements' -> go elements' selectedIndex
            EvDragOver a -> do
              let newElements = updateAt index (Left true) widgetsInfo
              case newElements of
                Nothing -> do
                  log "error2"
                  go widgetsInfo selectedIndex
                Just elements' -> go elements' selectedIndex
        Right { widgetFunc, result: DraggableWidgetResult { isDragging, exitState } } -> do
          if isDragging then
            -- dragging
            go widgetsInfo $ Just { index, widgetFunc, state: exitState }
            -- pure $ catMaybes $ hush <$> widgetsInfo
          else do
            -- change of value
            let newElements = updateAt index (Right (Tuple exitState widgetFunc)) widgetsInfo
            case newElements of
              Nothing -> go widgetsInfo selectedIndex
              Just es -> pure $ catMaybes $ hush <$> es 

dragAndDropAndRemoveList :: forall a. Array (LoopableWidget a) -> Widget HTML (Array (LoopableWidget a))
dragAndDropAndRemoveList widgets = do
  let widgetsInfo = [Left false] <> (intersperse (Left false) $ Right <$> widgets) <> [Left false]
  go widgetsInfo Nothing

  where
    mapWidget :: Boolean -> Tuple a (a -> Widget HTML a) -> (RemovableDraggableWidgetType a)
    mapWidget isDragging (Tuple is widgetFunc) = 
      let widget = removableDraggableWidget isDragging is widgetFunc
      in { widgetFunc, widget }

    manageDroppableAreaResult :: DroppableAreaResult  -> Tuple (OnDropAreaEvents SyntheticMouseEvent) DroppableWidget
    manageDroppableAreaResult { isSelected, result } = 
      let droppableWidget = droppableArea isSelected
      in Tuple result droppableWidget

    manageDraggableWidgetResult :: RemovableDraggableWidgetResult a -> RemovableDraggableWidgetType a -> Maybe (Tuple a (RemovableDraggableWidgetType a))
    manageDraggableWidgetResult Remove _ = Nothing
    manageDraggableWidgetResult (Result (DraggableWidgetResult { isDragging, exitState })) { widgetFunc, widget } =
      let newWidget = mapWidget isDragging (Tuple exitState widgetFunc)
      in Just $ Tuple exitState newWidget

    includeEither :: forall m b c. Functor m => Either (m b) (m c) -> m (Either b c)
    includeEither (Left w) = Left <$> w
    includeEither (Right w) = Right <$> w

    mapDraggableWidget :: RemovableDraggableWidgetType a -> Widget HTML (RemovableDraggableSupportType a)
    mapDraggableWidget { widgetFunc, widget } = (\result -> { widgetFunc, result }) <$> widget

    prepareNewElements :: Array (Widget HTML (Either DroppableAreaResult (RemovableDraggableSupportType a))) 
                       -> SelectedDraggableInfo a
                       -> Int
                       -> Array (Widget HTML (Either DroppableAreaResult (RemovableDraggableSupportType a)))
    prepareNewElements elements { index: dragIndex, state, widgetFunc } dropIndex 
      | dragIndex > dropIndex =
          let untouchedFirstElems = take (dropIndex + 1) elements
              untouchedLastElems = drop (dragIndex + 1) elements
              elemsToBeShifted = takeEnd (dragIndex - dropIndex - 1) $ take (dragIndex - 1) elements
          in case elements !! dragIndex of
              Nothing -> elements
              Just elem -> 
                let newElem = Right <$> (mapDraggableWidget $ mapWidget false (Tuple state widgetFunc))
                in untouchedFirstElems
                  <> [newElem]
                  <> elemsToBeShifted
                  <> untouchedLastElems
      | otherwise = 
          let untouchedFirstElems = take (dragIndex - 1) elements
              untouchedLastElems = drop dropIndex elements
              elemsToBeShifted = takeEnd (dropIndex - dragIndex - 1) $ take dropIndex elements
          in case elements !! dragIndex of
              Nothing -> elements
              Just elem -> 
                let newElem = Right <$> (mapDraggableWidget $ mapWidget false (Tuple state widgetFunc))
                in untouchedFirstElems
                  <> elemsToBeShifted
                  <> [includeEither $ Left $ droppableArea false]
                  <> [newElem]
                  <> untouchedLastElems

    convertToLoopableWidget :: Either DroppableAreaResult (RemovableDraggableSupportType a) -> Maybe (LoopableWidget a)
    convertToLoopableWidget (Left _) = Nothing
    convertToLoopableWidget (Right { widgetFunc: _, result: Remove}) = Nothing
    convertToLoopableWidget (Right { widgetFunc, result: (Result (DraggableWidgetResult { isDragging, exitState }))}) = Just (Tuple exitState widgetFunc)

    convertToResult :: Array (Widget HTML (Either DroppableAreaResult (RemovableDraggableSupportType a))) -> Widget HTML (Array (LoopableWidget a))
    convertToResult arr =
      let sequencedWidgets = sequence arr
      in (\eithers -> catMaybes (convertToLoopableWidget <$> eithers)) <$> sequencedWidgets

    go :: Array (Either Boolean (LoopableWidget a)) -> Maybe (SelectedDraggableInfo a) -> Widget HTML (Array (LoopableWidget a))
    go widgetsInfo selectedIndex = do
      let widgetsInfo' = ((lmap droppableArea) <$> widgetsInfo) :: Array (Either DroppableWidget (LoopableWidget a))
      let widgetsInfo'' = ((rmap (mapWidget false)) <$> widgetsInfo') :: Array (Either DroppableWidget (RemovableDraggableWidgetType a))
      let widgets' = ((rmap mapDraggableWidget) <$> widgetsInfo'') :: Array (Either DroppableWidget (Widget HTML (RemovableDraggableSupportType a)))
      let widgets'' = (includeEither <$> widgets') :: Array (Widget HTML (Either DroppableAreaResult (RemovableDraggableSupportType a)))
      let zipped = zipWith (\i -> \w -> { index: i, wi: w}) (range 0 (length widgets'')) widgets''
      let newWidgets = (\{index, wi} -> (\r -> {index, res: r}) <$> wi) <$> zipped
      {index, res} <- div [Props.className "dragAndDropList"] newWidgets
      -- pure $ catMaybes $ hush <$> widgetsInfo
      case res of 
        Left { isSelected, result } -> do
          case result of
            EvDrop a -> do
              case selectedIndex of
                Just { state, index: ix, widgetFunc } -> do
                  let withoutElem = (deleteAt ix widgetsInfo)
                  let withElem    = (withoutElem >>= (insertAt index (Right (Tuple state widgetFunc))))
                  case withElem of
                    Nothing -> do
                      log "error 0.5"
                      go widgetsInfo selectedIndex
                    Just elements -> do
                      let info = catMaybes $ hush <$> elements
                      pure info
                Nothing -> do
                  log "error1"
                  go widgetsInfo selectedIndex
            EvDragEnter a -> do
              let newElements = updateAt index (Left true) widgetsInfo
              case newElements of
                Nothing -> do
                  log "error2"
                  go widgetsInfo selectedIndex
                Just elements' -> go elements' selectedIndex
            EvDragLeave a -> do
              let newElements = updateAt index (Left false) widgetsInfo
              case newElements of
                Nothing -> do
                  log "error2"
                  go widgetsInfo selectedIndex
                Just elements' -> go elements' selectedIndex
            EvDragOver a -> do
              let newElements = updateAt index (Left true) widgetsInfo
              case newElements of
                Nothing -> do
                  log "error2"
                  go widgetsInfo selectedIndex
                Just elements' -> go elements' selectedIndex
        Right { widgetFunc, result: Result (DraggableWidgetResult { isDragging, exitState }) } -> do
          if isDragging then do
            -- dragging
            go widgetsInfo $ Just { index, widgetFunc, state: exitState }
          else do
            -- change of value
            let newElements = updateAt index (Right (Tuple exitState widgetFunc)) widgetsInfo
            case newElements of
              Nothing -> go widgetsInfo selectedIndex
              Just es -> pure $ catMaybes $ hush <$> es 
        Right { widgetFunc: _, result: Remove } -> do
          let newElements = deleteAt index widgetsInfo
          case newElements of
            Nothing -> do
              log "error last"
              go widgetsInfo selectedIndex
            Just es -> pure $ catMaybes $ hush <$> es

  
  
