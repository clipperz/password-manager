module Views.SimpleWebComponents
  ( DraggableWidgetResult(..)
  , LoopableWidget
  , MenuStatus(..)
  , PasswordForm
  , RemovableDraggableWidgetResult(..)
  , SubMenuAction(..)
  , SubmenuVoice
  , checkboxesSignal
  , clickableListItemWidget
  , complexMenu
  , complexMenu'
  , confirmationWidget
  , disabledSimpleTextInputWidget
  , dragAndDropAndRemoveList
  , loadingBar
  , loadingDiv
  , passwordStrengthShow
  , simpleButton
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
  , simpleUserSignal
  , simpleVerifiedPasswordSignal
  , submenu
  , submenu'
  )
  where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, display, loopS, loopW)
import Concur.Core.Props (handleProp)
import Concur.React (HTML)
import Concur.React.DOM (button, div, input, label, li, span, text, textarea, ul)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind, discard, (=<<), (>>=))
import Control.Semigroupoid ((<<<))
import Data.Array (catMaybes, deleteAt, insertAt, intersperse, length, mapWithIndex, range, updateAt, zipWith, (:))
import Data.Bifunctor (lmap, rmap)
import Data.Either (Either(..), hush)
import Data.Eq ((==))
import Data.EuclideanRing ((/))
import Data.Function (($))
import Data.Functor ((<$), (<$>), class Functor)
import Data.HeytingAlgebra (not, (||))
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (power)
import Data.Ring ((+), (-))
import Data.Semigroup ((<>))
import Data.Semiring ((*))
import Data.Show (show, class Show)
import Data.String (joinWith)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Events (readFile)
import Functions.Password (PasswordStrengthFunction, PasswordStrength, passwordStrengthClass)
import React.SyntheticEvent (currentTarget, preventDefault, SyntheticEvent_, NativeEventTarget, NativeEvent, SyntheticMouseEvent)
import Unsafe.Coerce (unsafeCoerce)
import Views.Components (Enabled(..), entropyMeter)


foreign import handleDragStartEvent_ :: String -> Int -> Int -> NativeEvent -> Effect Unit

loadingBar :: Int -> Int -> Int -> String
loadingBar current total width = 
  let fullCh = (width * current) / total
      full = power "█" fullCh
      empty = power "_" (width - fullCh)
  in full <> empty <> "% (" <> (show current) <> " of " <> (show total) <> ")"

simpleTextAreaWidget :: String -> Widget HTML String -> String -> String -> Widget HTML String
simpleTextAreaWidget className lbl placeholder content = do
  label [Props.className className] [
    span [Props.className "label"] [lbl]
  , Props.unsafeTargetValue <$> textarea [
      Props.value content
    , Props.onChange
    , Props.placeholder placeholder
    ] []
  ]


simpleInputWidget :: String -> Widget HTML String -> Boolean -> String -> String -> String -> Widget HTML String
simpleInputWidget className lbl disable placeholder value t =
  label [Props.className className] [
    span [Props.className "label"] [lbl]
  , (Props.unsafeTargetValue) <$> input [
      Props._type t
    , Props.placeholder placeholder
    , Props.value value
    , Props.disabled disable
    , Props.onChange
    ]
  ]

simpleFileInputWidget :: String -> Widget HTML String -> Widget HTML String
simpleFileInputWidget className lbl = do
  label [Props.className className] [
    span [Props.className "label"] [lbl]
  , fromSyntheticEvent =<< input [
      Props._type "file"
    , Props.onChange
    ]
  ]

  where 
    fromSyntheticEvent :: forall r. SyntheticEvent_ (currentTarget :: NativeEventTarget | r) -> Widget HTML String
    fromSyntheticEvent se = do
      nve <- liftEffect $ currentTarget se
      liftAff $ readFile nve

simpleTextInputWidget :: String -> Widget HTML String -> String -> String -> Widget HTML String
simpleTextInputWidget className lbl placeholder s = simpleInputWidget className lbl false placeholder s "text"

disabledSimpleTextInputWidget :: String -> Widget HTML String -> Boolean -> String -> String -> Widget HTML String
disabledSimpleTextInputWidget className lbl disable placeholder s = simpleInputWidget className lbl disable placeholder s "text"

simplePasswordInputWidget :: String -> Widget HTML String -> String -> Widget HTML String
simplePasswordInputWidget className lbl s = simpleInputWidget className lbl false "password" s "password"

simpleNumberInputWidget :: String -> Widget HTML String -> String -> String -> Widget HTML String
simpleNumberInputWidget className lbl placeholder s = simpleInputWidget className lbl false placeholder s "number"

simpleCheckboxWidget :: String -> Widget HTML Boolean -> Boolean -> Widget HTML Boolean 
simpleCheckboxWidget className lbl v =  label [Props.className className] [
                                span [Props.className "label"] [lbl]
                              , (not v) <$  input [
                                  Props._type "checkbox"
                                , Props.checked v
                                , Props.onChange
                                ]
                              ]

simpleButton :: forall a. String -> String -> Boolean -> a -> Widget HTML a
simpleButton className label disable value = button [value <$ Props.onClick, Props.disabled disable, Props.className className ] [span [] [text label]]

simpleTextAreaSignal :: String -> Widget HTML String -> String -> String -> Signal HTML String
simpleTextAreaSignal className label placeholder content = loopW content (simpleTextAreaWidget className label placeholder)

simpleUserSignal :: String -> String -> Signal HTML String
simpleUserSignal className u = loopW u (simpleTextInputWidget className (text "Username") "username")

simplePasswordSignal :: String -> String -> Signal HTML String
simplePasswordSignal className p = loopW p (simplePasswordInputWidget className (text "Password"))

simpleCheckboxSignal :: String -> Widget HTML Boolean -> Boolean -> Signal HTML Boolean
simpleCheckboxSignal className lbl v = loopW v (simpleCheckboxWidget className lbl)

type PasswordForm = { password       :: String
                    , verifyPassword :: String
                    }
simpleVerifiedPasswordSignal :: PasswordStrengthFunction -> Either PasswordForm String -> Signal HTML (Either PasswordForm String)
simpleVerifiedPasswordSignal psf f = loopS f $ \ef ->
  case ef of
    Left { password, verifyPassword } -> go password verifyPassword
    Right p -> go p p
    where go p vp = do
                      pswd <- loopW p (simplePasswordInputWidget ("password" <> " " <> (passwordStrengthClass $ psf p)) (text "Password"))
                      _ <- loopW p entropyMeter
                      pswd2 <- loopW vp (simplePasswordInputWidget ("verify_password" <> (if pswd == vp then "" else " different")) (text "Verify password"))
                      display $ text $ if pswd == pswd2 then "The passwords are the same" else "The passwords are not the same"
                      if pswd == pswd2 then
                        pure $ Right pswd
                      else 
                        pure $ Left { password: pswd, verifyPassword: pswd2 }

checkboxesSignal :: Array (Tuple String Boolean) ->  Map String (Widget HTML Boolean) -> Signal HTML (Array (Tuple String Boolean))
checkboxesSignal ts lablesMap = loopS ts \m -> do
  let checkboxes = ((\(Tuple id value) -> Tuple id (simpleCheckboxSignal id (fromMaybe (text "Label not found") (lookup id lablesMap)) value)) <$> m) :: Array (Tuple String (Signal HTML Boolean))
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

confirmationWidget :: String -> Widget HTML Boolean
confirmationWidget message = div [(Props.className "disableOverlay")] [
  div [Props.className "mask", false <$ Props.onClick] []
, div [Props.className "dialog"] [
    div [Props.className "message"] [text message]
  , div [Props.className "answers"] [
      simpleButton "confirm" "Yes" false true
    , simpleButton "cancel"  "No"  false false
    ]
  ]
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
  let showingProp = if showing then [Props.className "userSidebarSubitems"] else [Props.className "userSidebarSubitems hidden"]
  res <- li [] $ [OpenCloseMenu <$ b1] <> [(ul showingProp (((<$>) ClickOnVoice) <$> bs))]
  case res of
    OpenCloseMenu -> pure $ Left (not showing)
    ClickOnVoice a -> pure $ Right a

--  Each `SubmenuVoice` can be thought as a menu section, with an open/close status
type SubmenuVoice a = Tuple Boolean (Boolean -> a -> Widget HTML (Either Boolean a))

type IntermediateWidget a = Widget HTML (Either (Array Boolean) a)

--  Given a series of sections (arr :: Array (SumbenuVoice a)) they are bound to an index, so that a sorted list of sections is available (newArr)
complexMenu :: forall a. Maybe String -> Maybe String -> Array (SubmenuVoice a) -> a -> Widget HTML (Tuple (Array (SubmenuVoice a)) a)
complexMenu mId mClass arr currentItemSelected = do
  let pid    = fromMaybe [] ((\id -> [Props._id id])      <$> mId)
  let pclass = fromMaybe [] (( \c -> [Props.className c]) <$> mClass) 
  inter <- ul (pid <> pclass) (fromVoiceToWidget <$> newArr)
  case inter of
    Right a   -> pure $ Tuple arr a   --  If a subitem is selected, it is returned with the whole status (open/close) of all other sections.
    Left  bs  -> complexMenu mId mClass (redraw bs) currentItemSelected 

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

    --  Each section in the sorted serires is thus defined with:
    --  - position on the menu 
    --  - whether it is open or close
    --  - a function that can draw the matching widget, given its status (open/close)
    --  The menu item widget may return either a change in status of the widget itself (open/close) or whether a subitems has been clicked.
    fromVoiceToWidget :: { index :: Int, tuple :: SubmenuVoice a } -> Widget HTML (Either (Array Boolean) a)
    fromVoiceToWidget { index, tuple: (Tuple b f) } = do
      res <- f b currentItemSelected
      case res of 
        Left bool -> pure $ Left  $ fromMaybe booleans (updateAt index bool booleans) --  If a section is opened/closed the array with all the stati of the sections,
                                                                                      --  and the item at the matching position is updated
        Right a   -> pure $ Right a   --  If a subitem is selected, it is returned with the whole status (open/close) of all other sections.

------------------------------------

data OnDraggableEvents a b = StartDrag a | EndDrag a | Dragging a | Value b
instance showOnDraggableEvents :: Show (OnDraggableEvents a b) where
  show (StartDrag _)  = "StartDrag"
  show (EndDrag   _)  = "EndDrag"
  show (Dragging  _)  = "Dragging"
  show (Value     _)  = "Value"

data OnDropAreaEvents a = EvDrop a | EvDragEnter a | EvDragLeave a | EvDragOver a
instance showOnDropAreaEvents :: Show (OnDropAreaEvents a) where
  show (EvDrop      _)  = "EvDrop"
  show (EvDragEnter _)  = "EvDragEnter"
  show (EvDragLeave _)  = "EvDragLeave"
  show (EvDragOver  _)  = "EvDragOver"    --  menu is redrawn [366?]

newtype DraggableWidgetResult a = DraggableWidgetResult { isDragging :: Boolean, exitState :: a }
instance showDraggableWidgetResult :: Show (DraggableWidgetResult a) where
  show (DraggableWidgetResult {isDragging}) = "DWR " <> (show isDragging)

data RemovableDraggableWidgetResult a = Remove | Result (DraggableWidgetResult a)

handleDragStartEvent :: SyntheticMouseEvent -> Effect Unit 
handleDragStartEvent e = handleDragStartEvent_ "draggableElem" 20 50 (unsafeCoerce e).nativeEvent

removableDraggableWidget :: forall a. Boolean -> a -> (a -> Widget HTML a) -> Widget HTML (RemovableDraggableWidgetResult a)
removableDraggableWidget isDragging initialState widgetFunc = do
  res <- div [
    Props.classList [Just "draggableElem", (if isDragging then Just "draggingElem" else Nothing)]
  , (Result (DraggableWidgetResult { isDragging: true, exitState: initialState })) <$ Props.onDragStart
  , (Result (DraggableWidgetResult { isDragging: false, exitState: initialState })) <$ Props.onDragEnd
  ] [
    div [Props.className "editActions"] [
      div [Props.className "remove"] [simpleButton "remove" "remove field" false Remove]
    , div [
        Props.className "dragHandler"
      , Props.draggable true
      , handleProp handleDragStartEvent Props.onDragStart
      ] [span [] []]
    ]
  , (\a -> Result (DraggableWidgetResult { isDragging, exitState: a })) <$> (widgetFunc initialState)
  ]
  pure res

type DroppableAreaResult = { isSelected :: Boolean, result :: (OnDropAreaEvents SyntheticMouseEvent) }
type IndexedResult a = {index :: Int, result :: a}

droppableArea :: Int -> Enabled -> Boolean -> Widget HTML DroppableAreaResult
droppableArea index (Enabled enabled) isSelected = do
  result <- div [ Props.classList [Just ("dropzone " <> show index), (if isSelected then Just "selected" else Nothing), (if enabled then Nothing else Just "disabled")]] [
                  span ([
                    Props.className "activationArea"
                  ] <> (if enabled then [ 
                    EvDrop  <$> Props.onDrop
                  , EvDragLeave <$> Props.onDragLeave
                  , EvDragEnter <$> Props.onDragEnter
                  ] else [])) []
                , span [Props.className "dropArea"] []
                ]
  case result of
    EvDragOver ev -> do
      liftEffect $ preventDefault ev
      droppableArea index (Enabled enabled) isSelected
    EvDragEnter _ -> do
      pure { isSelected: true, result }
    EvDragLeave _ -> do
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

type LoopableWidget a = Tuple a (a -> Widget HTML a)


dragAndDropAndRemoveList :: forall a. Array (LoopableWidget a) -> Widget HTML (Array (LoopableWidget a))
dragAndDropAndRemoveList widgets = do
  let widgetsInfo = [Left false] <> (intersperse (Left false) $ Right <$> widgets) <> [Left false]
  go widgetsInfo Nothing

  where
    mapWidget :: Boolean -> Tuple a (a -> Widget HTML a) -> (RemovableDraggableWidgetType a)
    mapWidget isDragging (Tuple is widgetFunc) = 
      let widget = removableDraggableWidget isDragging is widgetFunc
      in { widgetFunc, widget }

    includeEither :: forall m b c. Functor m => Either (m b) (m c) -> m (Either b c)
    includeEither (Left w) = Left <$> w
    includeEither (Right w) = Right <$> w

    mapDraggableWidget :: RemovableDraggableWidgetType a -> Widget HTML (RemovableDraggableSupportType a)
    mapDraggableWidget { widgetFunc, widget } = (\result -> { widgetFunc, result }) <$> widget

    isDroppableAreaEnabled :: Int -> Maybe (SelectedDraggableInfo a) -> Enabled
    isDroppableAreaEnabled index (Just {index: selectedIndex}) = Enabled $ not $ ((index == selectedIndex - 1) || (index == (selectedIndex + 1)))
    isDroppableAreaEnabled _     Nothing = Enabled false

    go :: Array (Either Boolean (LoopableWidget a)) -> Maybe (SelectedDraggableInfo a) -> Widget HTML (Array (LoopableWidget a))
    go widgetsInfo selectedIndex = do
      let widgetsInfo'  = (mapWithIndex (\index selected -> lmap (droppableArea index (isDroppableAreaEnabled index selectedIndex)) selected) widgetsInfo)        :: Array (Either DroppableWidget (LoopableWidget a))
      let widgetsInfo'' = ((rmap (mapWidget false)) <$> widgetsInfo')   :: Array (Either DroppableWidget (RemovableDraggableWidgetType a))
      let widgets'      = ((rmap mapDraggableWidget) <$> widgetsInfo'') :: Array (Either DroppableWidget (Widget HTML (RemovableDraggableSupportType a)))
      let widgets''     = (includeEither <$> widgets')                  :: Array (Widget HTML (Either DroppableAreaResult (RemovableDraggableSupportType a)))
      let zipped        = zipWith (\i -> \w -> { index: i, wi: w}) (range 0 (length widgets'')) widgets''
      let newWidgets    = (\{index, wi} -> (\r -> {index, res: r}) <$> wi) <$> zipped
      {index, res} <- div [Props.classList [Just "dragAndDropList", (\_ -> "dragging") <$> selectedIndex]] newWidgets
      case res of 
        Left { result } -> do
          case result of
            EvDrop _ -> do
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
            EvDragEnter _ -> do
              let newElements = updateAt index (Left true) widgetsInfo
              case newElements of
                Nothing -> do
                  log "error2"
                  go widgetsInfo selectedIndex
                Just elements' -> go elements' selectedIndex
            EvDragLeave _ -> do
              let newElements = updateAt index (Left false) widgetsInfo
              case newElements of
                Nothing -> do
                  log "error2"
                  go widgetsInfo selectedIndex
                Just elements' -> go elements' selectedIndex
            EvDragOver _ -> do
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

  
  
