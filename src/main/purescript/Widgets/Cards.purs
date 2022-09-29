module Widgets.Cards where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, h3, li', p, text, ul)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$), (<$>))
import Data.Semigroup ((<>))
import Data.Show (show, class Show)

import DataModel.Card (CardField(..), CardValues(..), Card(..))
import DataModel.Index (CardReference(..))
import Effect.Aff.Class (liftAff)
import Functions.Communication.Cards (getCard)
import Functions.Clipboard (copyToClipboard)
import Widgets.Index (IndexUpdateAction(..))
import Widgets.SimpleWebComponents (simpleButton)

-- -----------------------------------

data CardAction = Edit Card | Clone Card | Archive Card | Delete Card | NoAction
instance showCardAction :: Show CardAction where
  show (Edit _)    = "Edit"
  show (Clone _)   = "Clone"
  show (Archive _) = "Archive"
  show (Delete _)  = "Delete"
  show (NoAction)   = "No action"

-- -----------------------------------

cardWidget :: CardReference -> Widget HTML IndexUpdateAction
cardWidget reference = do
  eitherCard <- liftAff $ runExceptT $ getCard reference
  case eitherCard of
    Right c -> do 
      res <- card c
      case res of
        Edit cc -> pure $ ChangeToReference cc cc
        Clone cc -> pure $ AddReference cc
        Archive cc -> pure $ ChangeToReference cc cc
        Delete cc -> pure $ DeleteReference cc
        NoAction -> cardWidget reference -- TODO: to optimize, use a cardWidget that takes a card
    Left err -> do
      -- TODO: check error to decide what to do
      NoUpdate <$ div [] [text $ show err] 

card :: Card -> Widget HTML CardAction
card c@(Card_v1 r) = div [] [
    cardActions c
  , NoAction <$ cardContent r.content
]

cardActions :: Card -> Widget HTML CardAction
cardActions c = div [] [
    simpleButton (show (Edit c))    false (Edit c)
  , simpleButton (show (Clone c))   false (Clone c)
  , simpleButton (show (Archive c)) false (Archive c)
  , simpleButton (show (Delete c))  false (Delete c)
]

cardContent :: forall a. CardValues -> Widget HTML a
cardContent (CardValues_v1 {title: t, tags: ts, fields: fs, notes: n}) = div [Props.className "card_content"] [
  h3  [Props.className "card_title"]  [text t]
, ul  [Props.className "card_tags"]   $ (\s -> li' [text s]) <$> ts
, div [Props.className "card_fields"] $ cardField <$> fs
, div [Props.className "card_notes"]  [text n]
]

cardField :: forall a. CardField -> Widget HTML a
cardField (CardField_v1 {name, value, locked}) = div [] [
  text name
, p ((if locked then [Props.className "PASSWORD"] else []) <> [(\_ -> copyToClipboard value) <$> Props.onClick]) [text value]
] --TODO add class based on content for urls and emails
