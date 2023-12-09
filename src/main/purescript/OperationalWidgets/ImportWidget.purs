module OperationalWidgets.ImportWidget where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, demand, fireOnce, loopS, loopW)
import Concur.React (HTML)
import Concur.React.DOM (text, div, h1, form, h3, h5, ul, li', p, span, br', a', a, div_, label, input, ul_, li_, dl, dt, dd)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Except.Trans (runExceptT, ExceptT(..), except)
import Control.Semigroupoid ((<<<))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (filter, length, cons, concat)
import Data.Either (Either(..), isLeft)
import Data.EuclideanRing ((/))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra (not)
import Data.List as List
import Data.List (List(..), fromFoldable, snoc, zipWith, (..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Operation (OperationStep(..), runOperation)
import Data.Semigroup ((<>))
import Data.Semiring ((+), (*))
import Data.Show (show)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import DataModel.AppState (AppError(..))
import DataModel.Card (Card(..), CardValues(..), CardField(..))
import DataModel.Index (Index(..), CardEntry(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.Cards (postCardWithState, deleteCardWithState)
import Functions.Communication.Users (getIndexWithState)
import Functions.Events (readFileFromDrop)
import Functions.Import (decodeImport, parseHTMLImport, decodeHTML)
import Functions.Time (getCurrentDateTime, formatDateTimeToDate)
import Views.SimpleWebComponents (loadingDiv, simpleButton, simpleCheckboxSignal, simpleFileInputWidget, simpleTextAreaSignal, simpleTextInputWidget)

data QuickSelection = All | None | Archived | NonArchived

data SelectionAction = Cards (Array (Tuple Boolean Card)) | NewQuickSelection QuickSelection

type CardSelectionInfo = { tag :: Maybe String, selectedCards :: (Array (Tuple Boolean Card)) }

data ImportStep = UploadContent String | ChooseCards (Array (Tuple Boolean Card)) | Confirm CardSelectionInfo
  
data DragFileEvents a = DragEnter a | DragLeave a | Drop a | FileContent String

dragAndDropFileInputWidget :: Widget HTML String
dragAndDropFileInputWidget = do
  dropDiv false

  where 
    dropDiv highlight = do
      res <- div  [ Props.classList (Just <$> (["dropArea"] <> if highlight then ["highlight"] else []))
                  , Props._id "import"
                  , DragEnter <$> Props.onDragEnter
                  , DragLeave <$> Props.onDragLeave
                  , Drop <$> Props.onDropCapture
                  ]
                  [ span [] [text "Drag your CSV or Clipperz export file here"]
                  , br'
                  , span [] [text "or"]
                  , br'
                  , FileContent <$> (simpleFileInputWidget "importButton" (a' [text "select it manually"]))
                  ]
      case res of
        DragEnter _ -> dropDiv true
        DragLeave _ -> dropDiv false
        Drop a -> liftAff $ readFileFromDrop a
        FileContent s -> pure s

importWidget :: Widget HTML (Either AppError Index)
importWidget = do
  newIndex <- ((Right (Index Nil)) <$ (importView "" Nothing)) <|> (liftAff $ runExceptT $ getIndexWithState)
  case newIndex of
    Right index -> div [Props._id "importPage"] [importPage index Nothing (UploadContent "")]
    Left err -> pure $ Left err

  where 
    saveImport :: List Card -> Index -> Widget HTML (Either AppError Index)
    saveImport cards (Index entries) = do
      let saveCardFunc = \c -> 
                         \(prevRes :: (Either (Tuple AppError (List CardEntry)) (List CardEntry))) -> do
                            case prevRes of
                              Left  _  -> pure prevRes
                              Right es -> do
                                res :: Either AppError (List CardEntry) <- liftAff $ runExceptT $ do
                                  newEntry <- postCardWithState c
                                  pure $ Cons newEntry es
                                case res of
                                  Right newEs -> pure $ Right newEs
                                  Left err -> pure $ Left (Tuple err es)
      let lastStep = LastStep (\prevRes -> do
                                            case prevRes of
                                              Left (Tuple err es) -> do
                                                _ <- liftAff $ runExceptT $ sequence $ (\(CardEntry r) -> deleteCardWithState r.cardReference) <$> es
                                                pure $ Left err
                                              Right es -> liftAff $ runExceptT $ do
                                                let newIndex = Index (entries <> es)
                                                -- _ <- updateIndexWithState newIndex
                                                pure newIndex
                              ) (form [] [h1 [] [text "Import"], text "Saving index"])
      let funcs = saveCardFunc <$> cards
      let total = List.length cards
      let mkPlaceholder = \{index, card: (Card {content: (CardValues r)})} -> form [Props.className "savingImport"] [
        h1 [] [text "Import"]
      , div [Props.className "loadingBarContainer"] [
          div[Props.className "loadingBar", Props.style {width: "" <> show ((index + 1) * 100 / total) <> "%"}] []  
        ]
      , p [] [text ("Saving " <> show (index + 1) <> " of " <> show total <> " (" <> r.title <> ")")]
      ]
      let pls = mkPlaceholder <$> (zipWith (\i -> \c -> {index: i, card: c}) (0 .. total) cards)
      let zipped = zipWith (\func -> \pl -> {func, pl}) funcs pls
      let steps = (snoc ((\{func, pl} -> IntermediateStep func pl) <$> zipped) lastStep) :: List (OperationStep (Either (Tuple AppError (List CardEntry)) (List CardEntry)) (Either AppError Index) (Widget HTML))
      extractEithers $ runOperation (Right Nil) steps

    extractEithers :: Widget HTML (Either (Either AppError Index) (Either (Tuple AppError (List CardEntry)) (List CardEntry))) -> Widget HTML (Either AppError Index)
    extractEithers m = do
      res <- m
      case res of
        Left i -> pure i
        Right (Right _) -> pure $ Left $ ImportError "Didn't save all cards"
        Right (Left (Tuple i _)) -> pure $ Left i

    importPage :: Index -> Maybe String -> ImportStep -> Widget HTML (Either AppError Index)
    importPage index error (UploadContent pl) = do
      res <- importView pl error
      case res of
        Left html -> do
          eitherCards <- runExceptT $ do
            codedCardData   <- except $ parseHTMLImport html
            decodedCardData <- pure   $ decodeHTML codedCardData
            ExceptT $ liftEffect $ decodeImport decodedCardData
          case eitherCards of
            Left  err   -> importPage index (Just $ show err) (UploadContent "")
            Right cards -> importPage index Nothing $ ChooseCards $ (\c@(Card r) -> Tuple (not r.archived) c) <$> cards
        Right jsonText ->
          let eitherJson = jsonParser jsonText
          in case eitherJson of
              Left  err  -> importPage index (Just $ show err) (UploadContent jsonText)
              Right json ->
                let eitherCards = decodeJson json
                in case eitherCards of
                    Left  err    -> importPage index (Just $ show err) (UploadContent (stringify json))
                    Right cards -> importPage index Nothing $ ChooseCards $ (\c@(Card r) -> Tuple (not r.archived) c) <$> cards
    importPage index _ (ChooseCards cards) = do
      (cardSelectionWidget (UploadContent (stringify $ encodeJson (snd <$> cards))) cards) >>= (importPage index Nothing)
    importPage index _ (Confirm { tag, selectedCards }) = 
      let cards = case tag of
                    Just t  -> prepareSelectedCards t selectedCards 
                    Nothing -> selectedCards
          total = length cards
          toImportCards = snd <$> (filter (\(Tuple b _) -> b) cards)
          toImport = length toImportCards
      in do
          res <- form [Props.className "importPage"] [
            h1 [] [text "Import"]
          , h5 [] [text $ "Import " <> (show toImport) <> " cards (of " <> (show total) <> ")?"]
          , simpleButton "import" "Import" false true
          , div [Props.className "importButtons"] [
              simpleButton "back" "previous" false false
            , simpleButton "next hide" "next"     true false
            ]
          ]
          if res then loadingDiv <|> (div [Props.className "importList"] [div [] [saveImport (fromFoldable toImportCards) index]])
          else importPage index Nothing (ChooseCards cards) 

    importView :: String -> Maybe String -> Widget HTML (Either String String)
    importView pl error = form [Props.className "importPage"] [
        h1 [] [text "Import"]
      , text (fromMaybe "" error)
      , p [Props.className "description"] [text "Import data from another Clipperz account using a JSON/HTML export file created by Clipperz."]
      , div [Props.className "importInput"] [
          Left <$> dragAndDropFileInputWidget
        , p [Props.className "description"] [text "Alternatively you may type or paste any properly formatted JSON data."]
        , Right <$> (demand $ do
                              textContent <- simpleTextAreaSignal "importText" (text "Import") "Type or copy your data here" pl
                              fireOnce (div [Props.className "importButtons"] [
                                simpleButton "back" "previous" true ""
                              , simpleButton "next" "next" (isLeft (jsonParser textContent)) textContent
                              ]))
        ]
      ]

    cardSelectionWidget :: ImportStep -> Array (Tuple Boolean Card) -> Widget HTML ImportStep --(Array (Tuple Boolean Card))
    cardSelectionWidget goBackValue cards = do
      newTagWithDate <- (((<>) "Import_") <<< formatDateTimeToDate) <$> (liftEffect getCurrentDateTime)
      form [Props.classList (Just <$> ["importPage", "scrollable"])] [ 
        h1 [] [text "Import"]
      , (demand $ do
          newTag <- loopS { sel: Nothing, tag: newTagWithDate, cb: true } $ \v -> do
                                                          newSel    <- loopW v.sel (\_ -> Just <$> selectWidget)
                                                          div_ [Props.className "tagButtons"] do
                                                            newTagCB  <-  loopW v.cb (\v_ -> label [Props.className "apply_tag"] [
                                                                            (not v_) <$  input [
                                                                              Props._type "checkbox"
                                                                            , Props.checked v_
                                                                            , Props.onChange
                                                                            ]
                                                                          , span [Props.className "label"] [text "Apply the following tag to imported cards:"]
                                                                          ])
                                                            newTag    <-  loopW v.tag (simpleTextInputWidget "tag" (text "Tag") "Tag")
                                                            pure $ { sel: newSel, tag: newTag, cb: newTagCB }
          selectedCards <- ul_ [] $ case newTag.sel of
            Nothing -> sequence $ (importCardProposalWidget if newTag.cb then Just newTag.tag else Nothing) <$> cards
            Just f  -> sequence $ (importCardProposalWidget if newTag.cb then Just newTag.tag else Nothing) <$> (filterCards f cards)
          let importInfo = { tag: (if newTag.cb then Just newTag.tag else Nothing), selectedCards }
          fireOnce  (div [Props.className "importButtons"] [
                      simpleButton "back" "previous" false goBackValue
                    , simpleButton "next" "next"     false (Confirm importInfo)
                    ]))
      ]

    filterCards :: QuickSelection -> Array (Tuple Boolean Card) -> Array (Tuple Boolean Card)
    filterCards All arr = (\(Tuple _ c) -> Tuple true c) <$> arr
    filterCards None arr = (\(Tuple _ c) -> Tuple false c) <$> arr
    filterCards Archived arr = (\(Tuple _ c@(Card r)) -> Tuple r.archived c) <$> arr
    filterCards NonArchived arr = (\(Tuple _ c@(Card r)) -> Tuple (not r.archived) c) <$> arr

    selectWidget :: Widget HTML QuickSelection
    selectWidget = div [Props.className "selectButtons"] [
      span [] [text "Select:"]
    , a [Props.className "all",          All         <$ Props.onClick] [text "All"]
    , a [Props.className "none",         None        <$ Props.onClick] [text "None"]
    , a [Props.className "archived",     Archived    <$ Props.onClick] [text "Archived"]
    , a [Props.className "not_archived", NonArchived <$ Props.onClick] [text "Not Archived"]
    ]

    importCardProposalWidget :: Maybe String -> Tuple Boolean Card -> Signal HTML (Tuple Boolean Card)
    importCardProposalWidget newTag (Tuple b c@(Card { content: cv })) = li_ [] do
      cb <- simpleCheckboxSignal "select_card" (cardContent cv newTag) b
      pure $ Tuple cb c

    cardContent :: forall a. CardValues -> Maybe String -> Widget HTML a
    cardContent (CardValues {title: t, tags: ts, fields: fs, notes: n}) newTag = div [Props.className "cardContent"] [
      h3 [Props.className "card_title"]    [text t]
    , ul [Props.className "card_tags"]   $ (\s -> li' [text s]) <$> (maybe ts (\nt -> ts <> [nt]) newTag)
    , dl [Props.className "card_fields"] $  concat $ (\(CardField {name, value, locked}) -> [
          dt [] [text name]
        , dd [Props.classList [if locked then (Just "password") else Nothing]] [
            text value
          ]
      ]) <$> fs
    , p [Props.className "card_notes"] [text n]
    ]

    prepareSelectedCards :: String -> Array (Tuple Boolean Card) -> Array (Tuple Boolean Card)
    prepareSelectedCards newTag =
      (<$>) (\(Tuple b (Card { content: CardValues r, secrets, archived, timestamp})) -> Tuple b (Card { archived, secrets, timestamp, content: (CardValues $ r { tags = (cons newTag r.tags) })}))
