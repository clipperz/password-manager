module OperationalWidgets.ExportWidget
  ( exportWidget
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, a, text, h1, h3, p, button)
import Concur.React.Props as Props
import Control.Alternative ((<|>))
import Control.Bind (bind)
import Control.Monad.Except.Trans (runExceptT)
import Control.Semigroupoid ((<<<))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), fromRight)
import Data.Function (($))
import Data.Functor ((<$>), (<$), void)
import Data.List (List(..), length)
import Data.Map (fromFoldable)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Operation (runOperation)
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import DataModel.Index (Index(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.Users (getIndexWithState)
import Functions.Export (OfflineCopyStep(..), OfflineCopyStepResult(..), UnencryptedCopyStep(..), UnencryptedCopyStepResult(..), prepareOfflineCopySteps, prepareUnencryptedCopySteps)
import Functions.JSState (getAppState)
import Functions.Time (getCurrentDateTime, formatDateTimeToDate)
import Views.SimpleWebComponents (loadingBar)

exportWidget :: Widget HTML Unit
exportWidget = do
  newIndex <- ((Right (Index Nil)) <$ exportView (Left "Loading index, please wait")) <|> (liftAff $ runExceptT $ getIndexWithState)
  exportView (lmap (\_ -> "Current index could not be loaded, please reload the application.") newIndex)

  where
    exportView :: Either String Index -> Widget HTML Unit
    exportView (Left err) = div [Props._id "exportPage"] [
      h1 [] [text "Export"]
    , p [] [text err]
    ]
    exportView (Right index) = div [Props._id "exportPage"] [
      h1 [] [text "Export"]
    , div [] [ 
        h3 [] [text "Offline copy"]
      , p [] [text "Download a read-only portable version of Clipperz. Very convenient when no Internet connection is available."]
      , p [] [text "An offline copy is just a single HTML file that contains both the whole Clipperz web application and your encrypted data."]
      , p [] [text "It is as secure as the hosted Clipperz service since they both share the same code and security architecture."]
      , offlineCopyWidget index
      ]
    , div [] [ 
        h3 [] [text "HTML + JSON"]
      , p [] [text "Download a printer-friendly HTML file that lists the content of all your cards."]
      , p [] [text "This same file also contains all your data in JSON format. Please note that file attachments are not included."]
      , p [Props.className "important"] [text "Beware: all data are unencrypted! Therefore make sure to properly store and manage this file."]
      , unencryptedCopyWidget index
      ]
    ]

    offlineCopyWidget :: Index -> Widget HTML Unit
    offlineCopyWidget index@(Index entries) = do
      let placeholders = fromFoldable [ (Tuple PrepareBlobList (text "Preparing blobs...")) 
                                      , (Tuple GetUserCard (text "Getting card list...")) 
                                      , (Tuple GetFileStructure (text "Preparing document template...")) 
                                      , (Tuple PrepareDocument (text "Preparing document...")) 
                                      , (Tuple PrepareDowload (text "Preparing file to download..."))
                                      ]
      let total = (length entries) + 2
      let plFunc = \i -> p [] [text $ "Getting blobs: " <> (loadingBar (i + 1) total 30)]
      steps <- liftAff $ prepareOfflineCopySteps placeholders plFunc index
      res <- runOperation (Right Start) steps
      case res of
        Left (Right url) -> do
          dt <- liftEffect $ formatDateTimeToDate <$> getCurrentDateTime
          a [Props.download (dt <> "_Clipperz_Offline"), Props.href url, void Props.onClick] [button [Props.disabled false] [text "Download"]] 
        Left (Left err) -> (text $ "Could not prepare offline copy: " <> err) <|> (a [] [button [Props.disabled true] [text "Download"]]) 
        Right _ -> (text "Preparation not completed, reload the application") <|> (a [] [button [Props.disabled true] [text "Download"]]) 

    unencryptedCopyWidget :: Index -> Widget HTML Unit
    unencryptedCopyWidget index@(Index entries) = do
      let placeholders = fromFoldable [ (Tuple PrepareCardList (text "Preparing cards...")) 
                                      , (Tuple PrepareContent (text "Preparing document content...")) 
                                      , (Tuple PrepareDoc (text "Preparing document...")) 
                                      , (Tuple PrepareDowloadUrl (text "Preparing file to download..."))
                                      ]
      let total = length entries
      let plFunc = \{index: index_} -> p [] [text $ "Getting cards: " <> (loadingBar (index_ + 1) total 30)]
      let steps = prepareUnencryptedCopySteps placeholders plFunc index
      res <- runOperation (Right StartStep) steps
      case res of
        Left (Right url) -> do
          dt <- liftEffect $ formatDateTimeToDate <$> getCurrentDateTime
          username <- liftEffect $ ((fromMaybe "") <<< (fromRight Nothing) <<< ((<$>) (\as -> as.username))) <$> getAppState
          a [Props.download (dt <> "_Clipperz_Export_" <> username), Props.href url, void Props.onClick] [button [Props.disabled false] [text "Download"]]
        Left (Left err) -> (text $ "Could not prepare unencrypted copy: " <> err) <|> (a [] [button [Props.disabled true] [text "Download"]]) 
        Right _ -> (text "Preparation not completed, reload the application") <|> (a [] [button [Props.disabled true] [text "Download"]]) 
          
