module Functions.Export where

import Affjax.ResponseFormat as RF
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Category (identity)
import Control.Monad.Except (throwError)
import Control.Monad.Except.Trans (ExceptT(..), except)
import Control.Semigroupoid ((<<<))
import Data.Argonaut.Core as AC
import Data.Array as Array
import Data.Codec.Argonaut (encode)
import Data.Codec.Argonaut.Common as CAC
import Data.Either (Either(..), either, note)
import Data.Foldable (fold)
import Data.Formatter.DateTime (formatDateTime)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (textHTML)
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Set (toUnfoldable)
import Data.Show (show)
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..))
import DataModel.AppError (AppError(..))
import DataModel.AppState (ProxyResponse(..))
import DataModel.CardVersions.Card (Card(..), CardField(..), CardValues(..), cardVersionCodec, fromCard)
import DataModel.CardVersions.CurrentCardVersions (currentCardCodecVersion, currentCardVersion)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.UserVersions.User (RequestUserCard, requestUserCardCodec)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.Communication.Backend (ConnectionState, isStatusCodeOk, genericRequest)
import Functions.Events (renderElement)
import Functions.State (offlineDataId)
import Functions.Time (formatDateTimeToDate, formatDateTimeToTime, getCurrentDateTime)
import Web.DOM.Document (Document, documentElement, toNode, createElement)
import Web.DOM.Element as EL
import Web.DOM.Node (lastChild, firstChild, insertBefore, setTextContent)
import Web.File.Blob (fromString, Blob)

unencryptedExportStyle :: String
unencryptedExportStyle = "body {font-family: 'DejaVu Sans Mono', monospace;margin: 0px;}header {padding: 10px;border-bottom: 2px solid black;}header p span {font-weight: bold;}h1 {margin: 0px;}h2 {margin: 0px;padding-top: 10px;}h3 {margin: 0px;}h5 {margin: 0px;color: gray;}ul {margin: 0px;padding: 0px;}div > ul > li {border-bottom: 1px solid black;padding: 10px;}div > ul > li.archived {background-color: #ddd;}ul > li > ul > li {font-size: 9pt;display: inline-block;}ul > li > ul > li:after {content: \",\";padding-right: 5px;}ul > li > ul > li:last-child:after {content: \"\";padding-right: 0px;}dl {}dt {color: gray;font-size: 9pt;}dd {margin: 0px;margin-bottom: 5px;padding-left: 10px;font-size: 13pt;}div > div {background-color: black;color: white;padding: 10px;}li p, dd.hidden {white-space: pre-wrap;word-wrap: break-word;font-family: monospace;}textarea {display: none}a {color: white;}@media print {div > div, header > div {display: none !important;}div > ul > li.archived {color: #ddd;}ul > li {page-break-inside: avoid;} }"

type BlobsList = List (Tuple HexString HexString)

prepareUnencryptedExport :: List Card -> Effect Blob
prepareUnencryptedExport cardList = do
  dt <- getCurrentDateTime
  let date = formatDateTimeToDate dt
  let time = formatDateTimeToTime dt
  let styleString    = "<style type=\"text/css\">" <> unencryptedExportStyle <> "</style>"
  let htmlDocString1 = "<div><header><h1>Your data on Clipperz</h1><h5>Export generated on " <> date <> " at " <> time <> "</h5></header>"
  let htmlDocString2 = "</div>"
  let htmlDocContent = prepareUnencryptedContent cardList
  pure (fromString (styleString <> htmlDocString1 <> htmlDocContent <> htmlDocString2) textHTML)

formatText :: String -> String
formatText = (replaceAll (Pattern "<") (Replacement "&lt;")) <<< (replaceAll (Pattern "&") (Replacement "&amp;"))

prepareUnencryptedContent :: List Card -> String
prepareUnencryptedContent l = 
  let list = fold $ cardToLi <$> l
      textareaContent = formatText $ AC.stringify $ encode (CAC.list currentCardCodecVersion) $ fromCard <$> l
  in "<ul>" <> list <> "</ul><div><textarea class=\'" <> (AC.stringify $ encode cardVersionCodec currentCardVersion) <> "\'>" <> textareaContent <> "</textarea></div>"

  where
    cardToLi (Card {content: (CardValues {title, tags, fields, notes}), archived, timestamp: _}) =
      let archivedTxt = if archived then "archived" else ""
          tagsLis = Array.fold $ (\t -> "<li>" <> (formatText t) <> "</li>") <$> (toUnfoldable tags)
          fieldsDts = fold $  (\(CardField {name, value, locked}) -> "<dt>" <> (formatText name) <> "</dt><dd class=\"" <> (if locked then "hidden" else "") <> "\">" <> (formatText value) <> "</dd>") <$> fields
          liContent = "<h2>" <> (formatText title) <> "</h2><ul> " <> tagsLis <> "</ul><div><dl>" <> fieldsDts <> "</dl></div><p>" <> (formatText notes) <> "</p>"
      in "<li class=\"" <> archivedTxt <> "\">" <> liContent <> "</li>"

getBasicHTML :: ConnectionState -> ExceptT AppError Aff (ProxyResponse Document)
getBasicHTML connectionState = do
  let url = "static/index.html"
  ProxyResponse proxy res <- genericRequest connectionState url GET Nothing RF.document
  if isStatusCodeOk res.status
  then pure $ ProxyResponse proxy res.body
  else throwError $ ProtocolError (ResponseError $ unwrap res.status)

appendCardsDataInPlace :: Document -> List (Tuple HexString HexString) -> RequestUserCard -> ExceptT AppError Aff Document
appendCardsDataInPlace doc blobList requestUserCard = do
  let blobsContent     = "const blobs = { "          <> (fold $ (\(Tuple k v) -> "\"" <> show k <> "\": \"" <> show v <> "\", " ) <$> blobList) <> "}"
  let userCardContent  = "const userCard = "         <> (AC.stringify $ encode requestUserCardCodec requestUserCard)
  currentDateTime     <- liftEffect getCurrentDateTime
  let offlineTimestamp = "const offlineTimestamp = '" <> ((either identity identity $ formatDateTime "ddd, D MMMM YYYY HH:mm:ss" currentDateTime) <> " UTC'")
  let prepareContent   = "window.blobs = blobs; window.userCard = userCard; window.offlineTimestamp = offlineTimestamp;"
  let nodeContent      = userCardContent <> ";\n" <> blobsContent <> ";\n" <> offlineTimestamp <> ";\n" <> prepareContent

  let asNode = toNode doc
  html <- ExceptT $ (note $ UnhandledCondition "TODO") <$> (liftEffect $ lastChild asNode)
  body <- ExceptT $ (note $ UnhandledCondition "TODO") <$> (liftEffect $ lastChild html)
  fst <-  ExceptT $ (note $ UnhandledCondition "TODO") <$> (liftEffect $ firstChild body)

  scriptElement <- liftEffect $ createElement "script" doc
  liftEffect $ EL.setId offlineDataId scriptElement
  let newNode = EL.toNode scriptElement
  liftEffect $ setTextContent nodeContent newNode
  liftEffect $ insertBefore newNode fst body

  except $ Right doc

prepareHTMLBlob :: Document -> Effect Blob
prepareHTMLBlob doc = do
  html <- ((<$>) (renderElement)) <$> documentElement doc
  pure $ fromString (fromMaybe "<html></html>" html) (MediaType "text/html")
