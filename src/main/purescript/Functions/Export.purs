module Functions.Export where

import Affjax.ResponseFormat as RF
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (runExceptT, ExceptT(..), mapExceptT, except)
import Control.Semigroupoid ((<<<))
import Data.Argonaut.Core as AC
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (hush, either, Either(..), note)
import Data.Foldable (fold)
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HexString (HexString, fromArrayBuffer)
import Data.HTTP.Method (Method(..))
import Data.List (List(..), (:), toUnfoldable, sort)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Communication.FromString as FS
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (Index(..), CardEntry(..), CardReference(..))
import DataModel.Card (Card(..), CardValues(..), CardField(..))
import DataModel.User (IndexReference(..), UserCard(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Foreign (readString)
import Functions.Communication.BackendCommunication (manageGenericRequest, isStatusCodeOk)
import Functions.Communication.Blobs (getBlob)
import Functions.Communication.Cards (getCard)
import Functions.Communication.Users (getUserCard)
import Functions.Events (renderElement)
import Functions.JSState (getAppState)
import Functions.State (offlineDataId)
import Web.DOM.Document (Document, documentElement, toNode, createElement)
import Web.DOM.Element as EL
import Web.DOM.Node (childNodes, nodeName, lastChild, firstChild, insertBefore, setTextContent)
import Web.DOM.NodeList (toArray)
import Web.File.Blob (fromString, Blob)
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.File.File (File, name)
import Web.File.FileReader (fileReader, readAsText, toEventTarget, result)
import Web.File.Url (createObjectURL, revokeObjectURL)
import Web.HTML (window)
import Web.HTML.Event.EventTypes as EventTypes
import Web.HTML.HTMLHtmlElement (toHTMLElement)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

prepareOfflineCopy :: Index -> Aff (Either String String)
prepareOfflineCopy index = runExceptT $ do
  blobList <- mapExceptT (\m -> (lmap show) <$> m) $ prepareBlobList index
  userCard <- mapExceptT (\m -> (lmap show) <$> m) $ getUserCard
  doc <- mapExceptT (\m -> (lmap show) <$> m) getBasicHTML
  preparedDoc <- appendCardsDataInPlace doc blobList userCard
  blob <- ExceptT $ Right <$> (liftEffect $ prepareHTMLBlob preparedDoc)
  s :: String <- ExceptT $ Right <$> readFile blob
  ExceptT $ Right <$> (liftEffect $ createObjectURL blob)

prepareUnencryptedCopy :: Index -> Aff (Either String String)
prepareUnencryptedCopy index = runExceptT $ do
  cardList <- mapExceptT (\m -> (lmap show) <$> m) $ prepareCardList index
  let htmlDocString1 = "<div><header>This data is unencrypted</header>"
  let htmlDocString2 = "</div>" -- "<footer></footer></div>"
  let htmlDocContent = prepareUnencryptedContent cardList
  let htmlDocString = htmlDocString1 <> htmlDocContent <> htmlDocString2
  ExceptT $ Right <$> (liftEffect $ log htmlDocString)
  doc :: Document <- ExceptT $ Right <$> (liftEffect $ FS.fromString htmlDocString)
  blob <- ExceptT $ Right <$> (liftEffect $ prepareHTMLBlob doc)
  s :: String <- ExceptT $ Right <$> readFile blob
  ExceptT $ Right <$> (liftEffect $ createObjectURL blob)

formatText :: String -> String
formatText = (replaceAll (Pattern "<") (Replacement "&lt;")) <<< (replaceAll (Pattern "&") (Replacement "&amp;"))

prepareCardList :: Index -> ExceptT AppError Aff (List Card)
prepareCardList index@(Index l) = do
  let refs = (\(CardEntry cr) -> cr.cardReference) <$> (sort l)
  sequence $ getCard <$> refs

prepareUnencryptedContent :: List Card -> String
prepareUnencryptedContent l = 
  let list = fold $ cardToLi <$> l
      textareaContent = formatText $ AC.stringify $ encodeJson l
  in "<ul>" <> list <> "</ul><div><textarea>" <> textareaContent <> "</textarea></div>"

  where
    cardToLi (Card {content: (CardValues {title, tags, fields, notes}), archived, timestamp}) =
      let archivedTxt = if archived then "archived" else ""
          tagsLis = fold $ (\t -> "<li>" <> (formatText t) <> ":" <> ":after </li>") <$> tags
          fieldsDts = fold $  (\(CardField {name, value, locked}) -> "<dt>" <> (formatText name) <> "</dt><dd class=\"" <> (if locked then "hidden" else "") <> "\">" <> (formatText value) <> "</dd>") <$> fields
          liContent = "::marker <h2>" <> (formatText title) <> "</h2><ul> " <> tagsLis <> "</ul><div><dl>" <> fieldsDts <> "</dl></div><p>" <> (formatText notes) <> "</p>"
      in "<li class=\"" <> archivedTxt <> "\">" <> liContent <> "</li>"

getBasicHTML :: ExceptT AppError Aff Document
getBasicHTML = do
  let url = "index.html"
  res <- manageGenericRequest url GET Nothing RF.document
  if isStatusCodeOk res.status then except $ Right res.body
  else except $ Left $ ProtocolError $ ResponseError $ unwrap res.status 

appendCardsDataInPlace :: Document -> List (Tuple HexString HexString) -> UserCard -> ExceptT String Aff Document
appendCardsDataInPlace doc blobList uc@(UserCard r) = do
  let blobsContent = "const blobs = { " <> (fold $ (\(Tuple k v) -> "\"" <> show k <> "\": \"" <> show v <> "\", " ) <$> blobList) <> "}"
  let userCardContent = "const userCard = { " 
                          <> "\"c\": \"" <> (show r.c) <> "\""
                          <> ", \"v\": \"" <> (show r.v) <> "\""
                          <> ", \"s\": \"" <> (show r.s) <> "\""
                          <> ", \"srpVersion\": " <> (show r.srpVersion)
                          <> ", \"masterKeyEncodingVersion\": " <> (show r.masterKeyEncodingVersion)
                          <> ", \"masterKeyContent\": \"" <> (show r.masterKeyContent) <> "\""
                          <> "}"
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
prepareBlobList index@(Index list) = do
  { indexReference } <- ExceptT $ liftEffect $ getAppState
  (IndexReference { reference: indexRef } ) <- except $ note (InvalidStateError $ MissingValue $ "indexReference is Nothing") indexReference
  let allRefs = indexRef : (extractRefFromEntry <$> list)
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
