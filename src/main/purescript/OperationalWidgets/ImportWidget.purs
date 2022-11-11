module OperationalWidgets.ImportWidget where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, fireOnce, demand, step, dyn, hold)
import Concur.React (HTML)
import Concur.React.DOM (text, div, h1, form, h3, ul, li', p)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except.Trans (runExceptT, ExceptT(..), except)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (filter, length)
import Data.Either (Either(..), isRight)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.List (List(..), (:), concat, fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Data.Unit (unit)
import DataModel.AppState (AppError)
import DataModel.Card (Card(..), CardValues(..))
import DataModel.Index (Index(..), CardEntry)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Communication.Cards (postCard)
import Functions.Communication.Users (updateIndex)
import Functions.Import (decodeImport, parseHTMLImport, decodeHTML)
import Views.CardViews (cardField)
import Views.SimpleWebComponents (loadingDiv, simpleFileInputWidget, simpleTextAreaSignal, simpleCheckboxSignal, simpleButton)

data ImportStep = UploadContent String | ChooseCards String | Confirm (Array (Tuple Boolean Card))

importWidget :: Index -> Widget HTML (Either AppError Index)
importWidget index@(Index entries) = div [Props._id "importPage"] [h1 [] [text "Import"], importPage Nothing (UploadContent "")]

  where 
    saveImport :: Array Card -> Aff (Either AppError Index)
    saveImport cards = runExceptT $ do
      newEntries :: Array CardEntry <- sequence (postCard <$> cards)
      let newIndex = Index (concat $ entries : (fromFoldable newEntries) : Nil)
      _ <- updateIndex newIndex
      ExceptT $ pure $ Right newIndex

    importPage :: Maybe String -> ImportStep -> Widget HTML (Either AppError Index)
    importPage error (UploadContent pl) = (ChooseCards <$> (div [] [
      text (fromMaybe "" error)
    , p [] [text "Import data from another Clipperz account using a JSON/HTML export file created by Clipperz."]
    , form [] [
        simpleFileInputWidget "import" (text "Import")
      , p [] [text "Alternatively you may type or paste any properly formatted JSON data."]
      , demand $ do
                  textContent <- simpleTextAreaSignal "importText" (text "Import") "Type or copy your data here" pl
                  fireOnce (simpleButton "Import" (isRight (jsonParser textContent)) textContent)
      ]
    ])) >>= (importPage Nothing)
    importPage _ (ChooseCards content) = do
      eitherCards <- runExceptT $ do
        codedCardData <- except $ parseHTMLImport content
        decodedCardData <- except $ Right $ decodeHTML codedCardData
        ExceptT $ liftEffect $ decodeImport decodedCardData
      case eitherCards of 
        Left err -> importPage (Just $ show err) (UploadContent content)
        Right cards -> (Confirm <$> do
          let cardOptions = (\c@(Card r) -> Tuple r.archived c) <$> cards
          div [Props.className "scrollable"] [demand $ do
                    selectedCards <- sequence $ importCardProposalWidget <$> cardOptions
                    fireOnce (div [Props.className "fixedFoot"] [simpleButton "Import" false selectedCards])]
        ) >>= (importPage Nothing)
    importPage _ (Confirm cards) = 
      let total = length cards
          toImportCards = snd <$> (filter (\(Tuple b _) -> b) cards)
          toImport = length toImportCards
      in do
          div [] [
            text $ "Import " <> (show toImport) <> " cards (of " <> (show total) <> ")?"
          , simpleButton "Import" false unit
          ]
          loadingDiv <|> (liftAff $ saveImport toImportCards)

    importCardProposalWidget :: Tuple Boolean Card -> Signal HTML (Tuple Boolean Card)
    importCardProposalWidget (Tuple b c@(Card { content: cv@(CardValues content), archived, timestamp})) = do
      cb <- simpleCheckboxSignal ("importCheckbox_" <> content.title) (cardContent cv) true (not archived)
      pure $ Tuple cb c

    cardContent :: forall a. CardValues -> Widget HTML a
    cardContent (CardValues {title: t, tags: ts, fields: fs, notes: n}) = div [Props.className "cardContent"] [
      h3  [Props.className "card_title"]  [text t]
    , ul  [Props.className "card_tags"]   $ (\s -> li' [text s]) <$> ts
    , div [Props.className "card_fields"] $ cardField <$> fs
    , div [Props.className "card_notes"]  [text n]
    ]
