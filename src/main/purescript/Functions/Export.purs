module Functions.Export where

import Affjax.ResponseFormat as RF
import Control.Alt (class Alt)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (runExceptT, ExceptT(..), mapExceptT, except)
import Control.Semigroupoid ((<<<))
import Data.Argonaut.Core as AC
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array (toUnfoldable, fromFoldable)
import Data.Bifunctor (lmap)
import Data.Either (either, Either(..), note)
import Data.Eq (class Eq)
import Data.Foldable (fold)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, fromArrayBuffer)
import Data.HTTP.Method (Method(..))
import Data.List (List(..), (:), sort, snoc, length, zipWith, (..))
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.Operation (OperationStep(..))
import Data.Ord (class Ord)
import Data.Semigroup ((<>))
import Data.Show (show, class Show)
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Communication.FromString as FS
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (Index(..), CardEntry(..), CardReference(..))
import DataModel.Card (Card(..), CardValues(..), CardField(..))
import DataModel.User (IndexReference(..), UserCard(..), UserInfoReferences(..), UserPreferencesReference(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (Aff, makeAff)
import Effect.Aff.Class (liftAff, class MonadAff)
import Effect.Exception (error)
import Foreign (readString)
import Functions.Communication.BackendCommunication (manageGenericRequest, isStatusCodeOk)
import Functions.Communication.Blobs (getBlob)
import Functions.Communication.Cards (getCard)
import Functions.Communication.Users (getUserCard)
import Functions.Events (renderElement)
import Functions.JSState (getAppState)
import Functions.State (offlineDataId)
import Functions.Time (getCurrentDateTime, formatDateTimeToDate, formatDateTimeToTime)
import Web.DOM.Document (Document, documentElement, toNode, createElement)
import Web.DOM.Element as EL
import Web.DOM.Node (lastChild, firstChild, insertBefore, setTextContent)
import Web.File.Blob (fromString, Blob)
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.File.FileReader (fileReader, readAsText, toEventTarget, result)
import Web.File.Url (createObjectURL)
import Web.HTML.Event.EventTypes as EventTypes

unencryptedExportStyle :: String
unencryptedExportStyle = "body {font-family: 'DejaVu Sans Mono', monospace;margin: 0px;}header {padding: 10px;border-bottom: 2px solid black;}header p span {font-weight: bold;}h1 {margin: 0px;}h2 {margin: 0px;padding-top: 10px;}h3 {margin: 0px;}h5 {margin: 0px;color: gray;}ul {margin: 0px;padding: 0px;}div > ul > li {border-bottom: 1px solid black;padding: 10px;}div > ul > li.archived {background-color: #ddd;}ul > li > ul > li {font-size: 9pt;display: inline-block;}ul > li > ul > li:after {content: \",\";padding-right: 5px;}ul > li > ul > li:last-child:after {content: \"\";padding-right: 0px;}dl {}dt {color: gray;font-size: 9pt;}dd {margin: 0px;margin-bottom: 5px;padding-left: 10px;font-size: 13pt;}div > div {background-color: black;color: white;padding: 10px;}li p, dd.hidden {white-space: pre-wrap;word-wrap: break-word;font-family: monospace;}textarea {display: none}a {color: white;}@media print {div > div, header > div {display: none !important;}div > ul > li.archived {color: #ddd;}ul > li {page-break-inside: avoid;} }"

prepareOfflineCopy :: Index -> Aff (Either String String)
prepareOfflineCopy index = runExceptT $ do
  blobList <- mapExceptT (\m -> (lmap show) <$> m) $ prepareBlobList index
  userCard <- mapExceptT (\m -> (lmap show) <$> m) $ getUserCard
  doc <- mapExceptT (\m -> (lmap show) <$> m) getBasicHTML
  preparedDoc <- appendCardsDataInPlace doc blobList userCard
  blob <- ExceptT $ Right <$> (liftEffect $ prepareHTMLBlob preparedDoc)
  _ <- ExceptT $ Right <$> readFile blob
  ExceptT $ Right <$> (liftEffect $ createObjectURL blob)

data OfflineCopyStep = PrepareBlobList | GetUserCard | GetFileStructure | PrepareDocument | PrepareDowload
derive instance ordOfflineCopyStep :: Ord OfflineCopyStep
derive instance eqOfflineCopyStep :: Eq OfflineCopyStep
instance showOfflineCopyStep :: Show OfflineCopyStep where
  show PrepareBlobList = "PrepareBlobList" 
  show GetUserCard = "GetUserCard" 
  show GetFileStructure = "GetFileStructure" 
  show PrepareDocument = "PrepareDocument" 
  show PrepareDowload = "Preparing download url..."

type BlobsList = List (Tuple HexString HexString)

data OfflineCopyStepResult = Start
                           | BlobList BlobsList
                           | UCard { userCard :: UserCard, blobList :: BlobsList }
                           | BasicDoc { userCard :: UserCard, blobList :: BlobsList, doc :: Document }
                           | PreparedDoc Document
                           | DownloadUrl String
instance showOfflineCopyStepResult :: Show OfflineCopyStepResult where
  show Start = "Start"
  show (BlobList _) = "Prepare list of blobs"
  show (UCard _) = "Get user card"
  show (BasicDoc _) = "Get template for offline copy"
  show (PreparedDoc _) = "Prepare offline copy"
  show (DownloadUrl _) = "Download url ready"

prepareOfflineCopySteps :: forall m. MonadAff m => MonadEffect m => Alt m => Map OfflineCopyStep (m (Either String OfflineCopyStepResult)) -> (Int -> m (Either String OfflineCopyStepResult)) -> Index -> Aff (List (OperationStep (Either String OfflineCopyStepResult) (Either String String) m))
prepareOfflineCopySteps placeholders mkPlaceholderGetBlob index = do
  steps <- runExceptT $ prepareBlobListSteps mkPlaceholderGetBlob index
  case steps of
    Left  _ -> pure Nil
    Right s -> pure $ s <> (toUnfoldable  
      [ IntermediateStep (\ocsr -> 
                            case ocsr of
                              Left err -> pure $ Left err
                              Right (BlobList blobList) -> ((<$>) (\userCard -> UCard {userCard, blobList})) <$> (liftAff $ runExceptT $ mapExceptT (\m -> (lmap show) <$> m) $ getUserCard)
                              Right _ -> pure $ Left "Wrong step before getting user card"
                        ) (fromMaybe (pure (Left "Please wait...")) (lookup GetUserCard placeholders))
      , IntermediateStep (\ocsr -> 
                            case ocsr of
                              Left err -> pure $ Left err
                              Right (UCard {userCard, blobList}) -> ((<$>) (\doc -> BasicDoc {userCard, doc, blobList})) <$> (liftAff $ runExceptT $ mapExceptT (\m -> (lmap show) <$> m) getBasicHTML)
                              Right _ -> pure $ Left "Wrong step before getting file template"
                        ) (fromMaybe (pure (Left "Please wait...")) (lookup GetFileStructure placeholders))
      , IntermediateStep (\ocsr -> 
                            case ocsr of
                              Left err -> pure $ Left err
                              Right (BasicDoc {userCard, doc, blobList}) -> ((<$>) PreparedDoc) <$> (liftAff $ runExceptT $ appendCardsDataInPlace doc blobList userCard)
                              Right _ -> pure $ Left "Wrong step before preparing offline copy"
                        ) (fromMaybe(pure (Left "Please wait...")) (lookup PrepareDocument placeholders))
      , LastStep (\ocsr -> 
                    case ocsr of
                      Left err -> pure $ Left err
                      Right (PreparedDoc doc) -> do
                        blob <- liftEffect $ prepareHTMLBlob doc
                        _ <- liftAff $ readFile blob
                        url <- liftEffect $ createObjectURL blob
                        pure $ Right $ url
                      Right _ -> pure $ Left "Wrong step before preparing download url"
                ) (((<$>) show) <$> (fromMaybe (pure (Left "Please wait...")) (lookup PrepareDowload placeholders)))
      ])

--------------------------------------------------

prepareUnencryptedCopy :: Index -> Aff (Either String String)
prepareUnencryptedCopy index = runExceptT $ do
  dt <- ExceptT $ Right <$> (liftEffect $ getCurrentDateTime)
  let date = formatDateTimeToDate dt
  let time = formatDateTimeToTime dt
  cardList <- mapExceptT (\m -> (lmap show) <$> m) $ prepareCardList index
  let styleString = "<style type=\"text/css\">" <> unencryptedExportStyle <> "</style>"
  let htmlDocString1 = "<div><header><h1>Your data on Clipperz</h1><h5>Export generated on " <> date <> " at " <> time <> "</h5></header>"
  let htmlDocString2 = "</div>"
  let htmlDocContent = prepareUnencryptedContent cardList
  let htmlDocString = styleString <> htmlDocString1 <> htmlDocContent <> htmlDocString2
  doc :: Document <- ExceptT $ Right <$> (liftEffect $ FS.fromString htmlDocString)
  blob <- ExceptT $ Right <$> (liftEffect $ prepareHTMLBlob doc)
  _ <- ExceptT $ Right <$> readFile blob
  ExceptT $ Right <$> (liftEffect $ createObjectURL blob)

data UnencryptedCopyStep = PrepareCardList | PrepareContent | PrepareDoc | PrepareDowloadUrl
derive instance ordUnencryptedCopyStep :: Ord UnencryptedCopyStep
derive instance eqUnencryptedCopyStep :: Eq UnencryptedCopyStep
instance showUnencryptedCopyStep :: Show UnencryptedCopyStep where
  show PrepareCardList = "PrepareCardList" 
  show PrepareContent = "PrepareContent" 
  show PrepareDoc = "PrepareDocument" 
  show PrepareDowloadUrl = "Preparing download url..."

data UnencryptedCopyStepResult = StartStep
                               | CardList (List Card)
                               | DocumentContent String 
                               | PreparedUnencryptedDoc Document
                               | Url String
instance showUnencryptedCopyStepResult :: Show UnencryptedCopyStepResult where
  show StartStep = "Start"
  show (CardList _) = "Prepare list of cards"
  show (DocumentContent _) = "Prepare content of document"
  show (PreparedUnencryptedDoc _) = "Prepare offline copy"
  show (Url _) = "Download url ready"

prepareUnencryptedCopySteps :: forall m. MonadAff m 
                            => MonadEffect m 
                            => Alt m 
                            => Map UnencryptedCopyStep (m (Either String UnencryptedCopyStepResult)) 
                            -> ({index :: Int, card :: CardEntry} -> m (Either String UnencryptedCopyStepResult))
                            -> Index 
                            -> List (OperationStep (Either String UnencryptedCopyStepResult) (Either String String) m)
prepareUnencryptedCopySteps placeholders mkPlaceholderGetCard index = toUnfoldable
  ( (fromFoldable $ prepareCardListSteps mkPlaceholderGetCard index) <>
    [ IntermediateStep (\ocsr -> 
                          case ocsr of
                            Left err -> pure $ Left err
                            Right (CardList cardList) -> (Right <<< DocumentContent) <$> do
                                                          dt <- liftEffect $ getCurrentDateTime
                                                          let date = formatDateTimeToDate dt
                                                          let time = formatDateTimeToTime dt
                                                          let styleString = "<style type=\"text/css\">" <> unencryptedExportStyle <> "</style>"
                                                          let htmlDocString1 = "<div><header><h1>Your data on Clipperz</h1><h5>Export generated on " <> date <> " at " <> time <> "</h5></header>"
                                                          let htmlDocString2 = "</div>"
                                                          let htmlDocContent = prepareUnencryptedContent cardList
                                                          pure $ styleString <> htmlDocString1 <> htmlDocContent <> htmlDocString2
                            Right _ -> pure $ Left "Wrong step before defining document content"
                      ) (fromMaybe (pure (Left "Please wait...")) (lookup PrepareContent placeholders))
    , IntermediateStep (\ocsr -> 
                          case ocsr of
                            Left err -> pure $ Left err
                            Right (DocumentContent content) -> (Right <<< PreparedUnencryptedDoc) <$> (liftEffect $ FS.fromString content)
                            Right _ -> pure $ Left "Wrong step before preparing document"
                      ) (fromMaybe (pure (Left "Please wait...")) (lookup PrepareDoc placeholders))
    , LastStep (\ocsr -> 
                case ocsr of
                  Left err -> pure $ Left err
                  Right (PreparedUnencryptedDoc doc) -> do
                      blob <- liftEffect $ prepareHTMLBlob doc
                      _ <- liftAff $ readFile blob
                      url <- liftEffect $ createObjectURL blob
                      pure $ Right $ url
                  Right _ -> pure $ Left "Wrong step before preparing download url"
              ) (((<$>) show) <$> (fromMaybe (pure (Left "Please wait...")) (lookup PrepareDowloadUrl placeholders)))
    ]
  )

------------------------------------------------

prepareBlobListSteps :: forall m. MonadAff m => MonadEffect m => (Int -> m (Either String OfflineCopyStepResult)) -> Index -> ExceptT AppError Aff (List (OperationStep (Either String OfflineCopyStepResult) (Either String String) m))
prepareBlobListSteps placeholderFunc (Index entries) = do
  { userInfoReferences } <- ExceptT $ liftEffect $ getAppState
  (UserInfoReferences { indexReference: IndexReference { reference: indexRef }
                      , preferencesReference: UserPreferencesReference { reference: prefRef}
                      }) <- except $ note (InvalidStateError $ MissingValue $ "userInfoReferences is Nothing") userInfoReferences
  let references = prefRef : indexRef : (extractRefFromEntry <$> entries)
  let getBlobFunc = \hexref ->
                    \prevRes -> case prevRes of
                      Left err -> pure $ Left err
                      Right (BlobList blobs) -> do
                        newBlob <- liftAff $ runExceptT $ getBlob hexref
                        case newBlob of
                          Left err -> pure $ Left $ show err
                          Right b -> pure $ Right $ BlobList $ snoc blobs (Tuple hexref (fromArrayBuffer b))
                      Right Start -> do
                        newBlob <- liftAff $ runExceptT $ getBlob hexref
                        case newBlob of
                          Left err -> pure $ Left $ show err
                          Right b -> pure $ Right $ BlobList $ Cons (Tuple hexref (fromArrayBuffer b)) Nil
                      Right _ -> pure $ Left "illegal result of previous step"
  let funcs = getBlobFunc <$> references
  let total = length references
  let pls = placeholderFunc <$> (0 .. total) 
  let zipped = zipWith (\func -> \pl -> {func, pl}) funcs pls
  except $ Right ((\{func, pl} -> IntermediateStep func pl) <$> zipped)

  where 
    extractRefFromEntry (CardEntry r) = 
      case r.cardReference of
        (CardReference { reference }) -> reference

------------------------------------------------

prepareCardListSteps :: forall m. MonadAff m => MonadEffect m => ({index :: Int, card :: CardEntry} -> m (Either String UnencryptedCopyStepResult)) -> Index -> List (OperationStep (Either String UnencryptedCopyStepResult) (Either String String) m)
prepareCardListSteps placeholderFunc (Index entries) =
  let getCardFunc = \(CardEntry { cardReference }) ->
                    \prevRes -> case prevRes of
                      Left err -> pure $ Left err
                      Right (CardList cards) -> do
                        newCard <- liftAff $ runExceptT $ getCard cardReference
                        case newCard of
                          Left err -> pure $ Left $ show err
                          Right c -> pure $ Right $ CardList $ snoc cards c
                      Right StartStep -> do
                        newCard <- liftAff $ runExceptT $ getCard cardReference
                        case newCard of
                          Left err -> pure $ Left $ show err
                          Right c -> pure $ Right $ CardList $ Cons c Nil
                      Right _ -> pure $ Left "illegal result of previous step"
      funcs = getCardFunc <$> entries
      total = length entries
      pls = placeholderFunc <$> (zipWith (\i -> \c -> {index: i, card: c}) (0 .. total) entries)
      zipped = zipWith (\func -> \pl -> {func, pl}) funcs pls
  in ((\{func, pl} -> IntermediateStep func pl) <$> zipped)

------------------------------------------------

formatText :: String -> String
formatText = (replaceAll (Pattern "<") (Replacement "&lt;")) <<< (replaceAll (Pattern "&") (Replacement "&amp;"))

prepareCardList :: Index -> ExceptT AppError Aff (List Card)
prepareCardList (Index l) = do
  let refs = (\(CardEntry cr) -> cr.cardReference) <$> (sort l)
  sequence $ getCard <$> refs

prepareUnencryptedContent :: List Card -> String
prepareUnencryptedContent l = 
  let list = fold $ cardToLi <$> l
      textareaContent = formatText $ AC.stringify $ encodeJson l
  in "<ul>" <> list <> "</ul><div><textarea>" <> textareaContent <> "</textarea></div>"

  where
    cardToLi (Card {content: (CardValues {title, tags, fields, notes}), archived, timestamp: _}) =
      let archivedTxt = if archived then "archived" else ""
          tagsLis = fold $ (\t -> "<li>" <> (formatText t) <> "</li>") <$> tags
          fieldsDts = fold $  (\(CardField {name, value, locked}) -> "<dt>" <> (formatText name) <> "</dt><dd class=\"" <> (if locked then "hidden" else "") <> "\">" <> (formatText value) <> "</dd>") <$> fields
          liContent = "<h2>" <> (formatText title) <> "</h2><ul> " <> tagsLis <> "</ul><div><dl>" <> fieldsDts <> "</dl></div><p>" <> (formatText notes) <> "</p>"
      in "<li class=\"" <> archivedTxt <> "\">" <> liContent <> "</li>"

getBasicHTML :: ExceptT AppError Aff Document
getBasicHTML = do
  let url = "static/index.html"
  res <- manageGenericRequest url GET Nothing RF.document
  if isStatusCodeOk res.status then except $ Right res.body
  else except $ Left $ ProtocolError $ ResponseError $ unwrap res.status 

appendCardsDataInPlace :: Document -> List (Tuple HexString HexString) -> UserCard -> ExceptT String Aff Document
appendCardsDataInPlace doc blobList (UserCard r) = do
  let blobsContent = "const blobs = { " <> (fold $ (\(Tuple k v) -> "\"" <> show k <> "\": \"" <> show v <> "\", " ) <$> blobList) <> "}"
  let userCardContent = "const userCard = " <> (AC.stringify $ encodeJson r)
  let prepareContent = "window.blobs = blobs; window.userCard = userCard;"
  let nodeContent = userCardContent <> ";\n" <> blobsContent <> ";\n" <> prepareContent

  let asNode = toNode doc
  html <- ExceptT $ (note "") <$> (liftEffect $ lastChild asNode)
  body <- ExceptT $ (note "") <$> (liftEffect $ lastChild html)
  fst <- ExceptT $ (note "") <$> (liftEffect $ firstChild body)

  scriptElement <- ExceptT $ Right <$> (liftEffect $ createElement "script" doc)
  ExceptT $ Right <$> (liftEffect $ EL.setId offlineDataId scriptElement)
  let newNode = EL.toNode scriptElement
  ExceptT $ Right <$> (liftEffect $ setTextContent nodeContent newNode)
  ExceptT $ Right <$> (liftEffect $ insertBefore newNode fst body)

  except $ Right doc

prepareBlobList :: Index -> ExceptT AppError Aff (List (Tuple HexString HexString))
prepareBlobList (Index list) = do
  { userInfoReferences } <- ExceptT $ liftEffect $ getAppState
  (UserInfoReferences { indexReference: IndexReference { reference: indexRef }
                      , preferencesReference: UserPreferencesReference { reference: prefRef}
                      }) <- except $ note (InvalidStateError $ MissingValue $ "userInfoReferences is Nothing") userInfoReferences
  let allRefs = prefRef : indexRef : (extractRefFromEntry <$> list)
  sequence $ prepareTuple <$> allRefs

  where 
    extractRefFromEntry (CardEntry r) = 
      case r.cardReference of
        (CardReference { reference }) -> reference
    
    prepareTuple :: HexString -> ExceptT AppError Aff (Tuple HexString HexString)
    prepareTuple ref = do
      ((Tuple ref) <<< fromArrayBuffer) <$> getBlob ref

prepareHTMLBlob :: Document -> Effect Blob
prepareHTMLBlob doc = do
  html <- ((<$>) (renderElement)) <$> documentElement doc
  pure $ fromString (fromMaybe "<html></html>" html) (MediaType "text/html")

readFile :: Blob -> Aff String
readFile blob = makeAff \fun -> do
  let err = fun <<< Left
      succ = fun <<< Right
  fr <- fileReader
  let et = toEventTarget fr

  errorListener <- eventListener \_ -> err (error "error")
  loadListener <- eventListener \_ -> do
    res <- result fr
    either (\errs -> err $ error $ show errs) succ $ runExcept $ readString res

  addEventListener EventTypes.error errorListener false et
  addEventListener EventTypes.load loadListener false et
  readAsText blob fr
  pure mempty
