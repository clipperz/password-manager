module OperationalWidgets.ExportWidget where

import Affjax.ResponseFormat as RF
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, a, text, button)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (runExceptT, ExceptT(..), mapExceptT, except)
import Control.Semigroupoid ((<<<))
import Data.Argonaut.Core as AC
import Data.Bifunctor (lmap)
import Data.Either (hush, either, Either(..), note)
import Data.Foldable (fold)
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HexString (HexString, fromArrayBuffer)
import Data.HTTP.Method (Method(..))
import Data.List (List(..), (:), toUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (Index(..), CardEntry(..), CardReference(..))
import DataModel.User (IndexReference(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Foreign (readString)
import Functions.Communication.BackendCommunication (manageGenericRequest, isStatusCodeOk)
import Functions.Communication.Blobs (getBlob)
import Functions.Events (renderElement)
import Functions.JSState (getAppState)
import Web.DOM.Document (Document, documentElement, toNode, createElement)
import Web.DOM.Element as EL
import Web.DOM.Node (childNodes, nodeName, lastChild, appendChild, setTextContent)
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

exportWidget :: Index -> Widget HTML Unit
exportWidget index = do
  _ <- button [Props.onClick, Props.disabled false] [text "Prepare offline copy"]
  downloadOfflineCopy index
  pure unit

downloadOfflineCopy :: Index -> Widget HTML Unit
downloadOfflineCopy index = do
  url <- liftAff $ runExceptT $ do
    blobList <- mapExceptT (\m -> (lmap show) <$> m) $ prepareBlobList index
    doc <- mapExceptT (\m -> (lmap show) <$> m) getBasicHTML
    preparedDoc <- appendCardsDataInPlace doc blobList
    blob <- ExceptT $ Right <$> (liftEffect $ prepareHTMLBlob preparedDoc)
    s :: String <- ExceptT $ Right <$> readFile blob
    ExceptT $ Right <$> (liftEffect $ createObjectURL blob)
  case url of
    Right url -> a [Props.download "offline_copy", Props.href url, (\_ -> unit) <$> Props.onClick] [button [Props.disabled false] [text "Download"]] -- TODO: add date
    Left err -> text $ "Could not prepare offline copy: " <> err

getBasicHTML :: ExceptT AppError Aff Document
getBasicHTML = do
  let url = "index.html"
  res <- manageGenericRequest url GET Nothing RF.document
  if isStatusCodeOk res.status then except $ Right res.body
  else except $ Left $ ProtocolError $ ResponseError $ unwrap res.status 

appendCardsDataInPlace :: Document -> List (Tuple HexString HexString) -> ExceptT String Aff Document
appendCardsDataInPlace doc blobList = do
  let nodeContent = "const blobs = { " <> (fold $ (\(Tuple k v) -> "\"" <> show k <> "\": \"" <> show v <> "\", " ) <$> blobList) <> "}"

  let asNode = toNode doc
  html <- ExceptT $ (note "") <$> (liftEffect $ lastChild asNode)
  body <- ExceptT $ (note "") <$> (liftEffect $ lastChild html)

  newNode <- ExceptT $ Right <$> (liftEffect $ EL.toNode <$> (createElement "script" doc))
  ExceptT $ Right <$> (liftEffect $ setTextContent nodeContent newNode)
  ExceptT $ Right <$> (liftEffect $ appendChild newNode body)

  except $ Right doc

prepareBlobList :: Index -> ExceptT AppError Aff (List (Tuple HexString HexString))
prepareBlobList index@(Index list) = do
  { indexReference } <- ExceptT $ liftEffect $ getAppState
  (IndexReference { reference: indexRef } ) <- except $ note (InvalidStateError $ MissingValue $ "indexReference is Nothing") indexReference
  let allRefs = indexRef : (extractRefFromEntry <$> list)
  prepareTuples allRefs
  -- sequence $ prepareTuple <$> allRefs

  where 
    extractRefFromEntry (CardEntry r) = 
      case r.cardReference of
        (CardReference { reference }) -> reference

    prepareTuples :: List HexString -> ExceptT AppError Aff (List (Tuple HexString HexString))
    prepareTuples Nil = except $ Right Nil
    prepareTuples (Cons r l) = do
      blobAb <- getBlob r
      let blob = ((Tuple r) <<< fromArrayBuffer) blobAb
      otherBlobs <- prepareTuples l
      except $ Right $ blob : otherBlobs
    
    -- prepareTuple :: HexString -> ExceptT AppError Aff (Tuple HexString HexString)
    -- prepareTuple ref = do
    --   ((Tuple ref) <<< fromArrayBuffer) <$> getBlob ref

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
