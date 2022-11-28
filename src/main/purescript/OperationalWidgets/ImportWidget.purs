module OperationalWidgets.ImportWidget where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, fireOnce, demand, loopW, loopS, step, dyn, hold)
import Concur.React (HTML)
import Concur.React.DOM (text, div, h1, form, h3, ul, li', p)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except.Trans (runExceptT, ExceptT(..), except)
import Control.Semigroupoid ((<<<))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (filter, length, cons)
import Data.Bifunctor (lmap)
import Data.Bitraversable (bisequence)
import Data.Either (Either(..), isLeft)
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra (not)
import Data.List as List
import Data.List (List(..), (:), concat, fromFoldable, snoc, zipWith, (..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Operation (OperationStep(..), extractResult, runOperation)
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Show (show)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Data.Unit (unit)
import DataModel.AppState (AppError(..))
import DataModel.Card (Card(..), CardValues(..))
import DataModel.Index (Index(..), CardEntry(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.Cards (postCard, deleteCard)
import Functions.Communication.Users (updateIndex, getIndex)
import Functions.Import (decodeImport, parseHTMLImport, decodeHTML)
import Functions.Time (getCurrentDateTime, formatDateTimeToDate)
import Views.CardViews (cardField)
import Views.SimpleWebComponents (loadingDiv, simpleFileInputWidget, dragAndDropFileInputWidget, simpleTextInputWidget, simpleTextAreaSignal, simpleCheckboxSignal, simpleButton)

data QuickSelection = All | None | Archived | NonArchived

data SelectionAction = Cards (Array (Tuple Boolean Card)) | NewQuickSelection QuickSelection

type CardSelectionInfo = { tag :: Maybe String, selectedCards :: (Array (Tuple Boolean Card)) }

data ImportStep = UploadContent String | ChooseCards (Array (Tuple Boolean Card)) | Confirm CardSelectionInfo

importWidget :: Widget HTML (Either AppError Index)
importWidget = do
  newIndex <- ((Right (Index Nil)) <$ (importView "" Nothing)) <|> (liftAff $ runExceptT $ getIndex)
  case newIndex of
    Right index@(Index es) -> div [Props._id "importPage"] [h1 [] [text "Import"], importPage index Nothing (UploadContent "")]
    Left err -> pure $ Left err

  where 
    saveImport' :: List Card -> Index -> Widget HTML (Either AppError Index)
    saveImport' Nil ix = pure $ Right ix
    saveImport' (Cons c@(Card {content: (CardValues r)}) cs) ix@(Index es) = (do
        nix <- liftAff $ runExceptT $ do
          newEntry <- postCard c
          let newIndex = Index (Cons newEntry es)
          _ <- updateIndex newIndex
          pure newIndex
        case nix of
          Right ix' -> saveImport cs ix'
          Left err -> pure nix
      ) <|> p [] [text ("Saving " <> r.title)]

    saveImport :: List Card -> Index -> Widget HTML (Either AppError Index)
    saveImport cards index@(Index entries) = do
      let saveCardFunc = \c@(Card {content: (CardValues r)}) -> 
                         \(prevRes :: (Either (Tuple AppError (List CardEntry)) (List CardEntry))) -> do
                            case prevRes of
                              Left err -> pure prevRes
                              Right es -> do
                                res :: Either AppError (List CardEntry) <- liftAff $ runExceptT $ do
                                  newEntry <- postCard c
                                  pure $ Cons newEntry es
                                case res of
                                  Right newEs -> pure $ Right newEs
                                  Left err -> pure $ Left (Tuple err es)
      let lastStep = LastStep (\prevRes -> do
                                            case prevRes of
                                              Left (Tuple err es) -> do
                                                _ <- liftAff $ runExceptT $ sequence $ (\(CardEntry r) -> deleteCard r.cardReference) <$> es
                                                pure $ Left err
                                              Right es -> liftAff $ runExceptT $ do
                                                let newIndex = Index (entries <> es)
                                                _ <- updateIndex newIndex
                                                pure newIndex
                              ) (text "Saving index")
      let funcs = saveCardFunc <$> cards
      let total = List.length cards
      let mkPlaceholder = \{index, card: (Card {content: (CardValues r)})} -> p [] [text ("Saving " <> r.title <> ", card " <> (show (index + 1)) <> " of " <> (show total) )]
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
            codedCardData <- except $ parseHTMLImport html
            decodedCardData <- except $ Right $ decodeHTML codedCardData
            ExceptT $ liftEffect $ decodeImport decodedCardData
          case eitherCards of
            Left err -> importPage index (Just $ show err) (UploadContent "")
            Right cards -> importPage index Nothing $ ChooseCards $ (\c@(Card r) -> Tuple (not r.archived) c) <$> cards
        Right jsonText ->
          let eitherJson = jsonParser jsonText
          in case eitherJson of
              Left err -> importPage index (Just $ show err) (UploadContent jsonText)
              Right json ->
                let eitherCards = decodeJson json
                in case eitherCards of
                    Left err -> importPage index (Just $ show err) (UploadContent (stringify json))
                    Right cards -> importPage index Nothing $ ChooseCards $ (\c@(Card r) -> Tuple (not r.archived) c) <$> cards
    importPage index _ (ChooseCards cards) = do
      (cardSelectionWidget (UploadContent (stringify $ encodeJson (snd <$> cards))) cards) >>= (importPage index Nothing)
    importPage index _ (Confirm { tag, selectedCards }) = 
      let cards = case tag of
                    Just t -> prepareSelectedCards t selectedCards 
                    Nothing -> selectedCards
          total = length cards
          toImportCards = snd <$> (filter (\(Tuple b _) -> b) cards)
          toImport = length toImportCards
      in do
          res <- form [Props.className "importPage"] [
            text $ "Import " <> (show toImport) <> " cards (of " <> (show total) <> ")?"
          , ((simpleButton "<<" false false) <|> (simpleButton "Import" false true))
          ]
          if res then loadingDiv <|> (div [Props.className "importList"] [div [] [saveImport (fromFoldable toImportCards) index]])
          else importPage index Nothing (ChooseCards cards) 

    importView :: String -> Maybe String -> Widget HTML (Either String String)
    importView pl error = form [Props.className "importPage"] [
        text (fromMaybe "" error)
      , p [] [text "Import data from another Clipperz account using a JSON/HTML export file created by Clipperz."]
      , div [Props.className "importInput"] [
          Left <$> (dragAndDropFileInputWidget "import" "Import")
        , p [] [text "Alternatively you may type or paste any properly formatted JSON data."]
        , Right <$> (demand $ do
                              textContent <- simpleTextAreaSignal "importText" (text "Import") "Type or copy your data here" pl
                              fireOnce (simpleButton "Import" (isLeft (jsonParser textContent)) textContent))
        ]
      ]

    cardSelectionWidget :: ImportStep -> Array (Tuple Boolean Card) -> Widget HTML ImportStep --(Array (Tuple Boolean Card))
    cardSelectionWidget goBackValue cards = do
      newTagWithDate <- (((<>) "Import_") <<< formatDateTimeToDate) <$> (liftEffect getCurrentDateTime)
      form [Props.classList (Just <$> ["importPage", "scrollable"])] [ 
          (demand $ do
            newTag <- loopS { sel: Nothing, tag: newTagWithDate, cb: true } $ \v -> do
                                                            newSel <- loopW v.sel (\_ -> Just <$> selectWidget)
                                                            newTagCB <- simpleCheckboxSignal "addTag" (text "Apply the following tag to imported cards:") true v.cb
                                                            newTag <- loopW v.tag (simpleTextInputWidget "newTag" (text "Tag") "Tag")
                                                            pure $ { sel: newSel, tag: newTag, cb: newTagCB }
            selectedCards <- case newTag.sel of
              Nothing -> sequence $ importCardProposalWidget <$> cards
              Just f -> sequence $ importCardProposalWidget <$> (filterCards f cards)
            let importInfo = { tag: (if newTag.cb then Just newTag.tag else Nothing), selectedCards }
            fireOnce (div [Props.className "fixedFoot"] [(simpleButton "<<" false goBackValue) <|> (simpleButton "Import" false (Confirm importInfo))]))
        ]

    filterCards :: QuickSelection -> Array (Tuple Boolean Card) -> Array (Tuple Boolean Card)
    filterCards All arr = (\(Tuple _ c) -> Tuple true c) <$> arr
    filterCards None arr = (\(Tuple _ c) -> Tuple false c) <$> arr
    filterCards Archived arr = (\(Tuple _ c@(Card r)) -> Tuple r.archived c) <$> arr
    filterCards NonArchived arr = (\(Tuple _ c@(Card r)) -> Tuple (not r.archived) c) <$> arr

    selectWidget :: Widget HTML QuickSelection
    selectWidget = div [] [
      p [] [text "Select:"]
    , simpleButton "All" false All
    , simpleButton "None" false None
    , simpleButton "Archived" false Archived
    , simpleButton "Not archived" false NonArchived
    ]

    importCardProposalWidget :: Tuple Boolean Card -> Signal HTML (Tuple Boolean Card)
    importCardProposalWidget (Tuple b c@(Card { content: cv@(CardValues content), archived, timestamp})) = do
      cb <- simpleCheckboxSignal ("importCheckbox_" <> content.title) (cardContent cv) true b
      pure $ Tuple cb c

    cardContent :: forall a. CardValues -> Widget HTML a
    cardContent (CardValues {title: t, tags: ts, fields: fs, notes: n}) = div [Props.className "cardContent"] [
      h3  [Props.className "card_title"]  [text t]
    , ul  [Props.className "card_tags"]   $ (\s -> li' [text s]) <$> ts
    , div [Props.className "card_fields"] $ cardField <$> fs
    , div [Props.className "card_notes"]  [text n]
    ]

    prepareSelectedCards :: String -> Array (Tuple Boolean Card) -> Array (Tuple Boolean Card)
    prepareSelectedCards newTag =
      (<$>) (\(Tuple b (Card { content: CardValues r, archived, timestamp})) -> Tuple b (Card { archived, timestamp, content: (CardValues $ r { tags = (cons newTag r.tags) })}))
