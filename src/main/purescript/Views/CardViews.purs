module Views.CardViews where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, h3, li', p, p', text, ul)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.Semigroup ((<>))
import Data.Show (show, class Show)
import DataModel.Card (CardField(..), CardValues(..), Card(..))
import Functions.Clipboard (copyToClipboard)
import Views.SimpleWebComponents (simpleButton, confirmationWidget)

-- -----------------------------------

data CardAction = Edit Card | Clone Card | Archive Card | Restore Card | Delete Card | Used Card
instance showCardAction :: Show CardAction where
  show (Edit _)    = "Edit"
  show (Used _)      = "Used"
  show (Clone _)   = "Clone"
  show (Archive _) = "Archive"
  show (Restore _) = "Restore"
  show (Delete _)  = "Delete"

-- -----------------------------------

cardView :: Card -> Boolean -> Widget HTML CardAction
cardView c@(Card r) isOffline = do
  res <- div [Props._id "card"] [
      cardActions c isOffline
    , (Used c) <$ cardContent r.content
  ]
  case res of
    Delete _ -> do
      confirmation <- div [] [
        false <$ cardActions c true
      , cardContent r.content
      , confirmationWidget "Are you sure you want to delete this card?"
      ]
      if confirmation then pure res else cardView c isOffline
    _ -> pure res

cardActions :: Card -> Boolean -> Widget HTML CardAction
cardActions c@(Card r) disabled = div [Props.className "cardActions"] [
    simpleButton (show (Edit c))    disabled (Edit c)
  , simpleButton (show (Clone c))   disabled (Clone c)
  , if r.archived then simpleButton (show (Restore c)) disabled (Restore c) else simpleButton (show (Archive c)) disabled (Archive c)
  , simpleButton (show (Delete c))  disabled (Delete c)
]

cardContent :: forall a. CardValues -> Widget HTML a
cardContent (CardValues {title: t, tags: ts, fields: fs, notes: n}) = div [Props._id "cardContent"] [
  h3  [Props.className "card_title"]  [text t]
, ul  [Props.className "card_tags"]   $ (\s -> li' [text s]) <$> ts
, div [Props.className "card_fields"] $ cardField <$> fs
, div [Props.className "card_notes"]  [text n]
]

cardField :: forall a. CardField -> Widget HTML a
cardField f@(CardField {name, value, locked}) = do
  res <- div [Props.className "field"] [
    p' [text name]
  , p ((if locked then [Props.className "PASSWORD"] else []) <> [Props.onClick]) [text value]
  ] --TODO add class based on content for urls and emails
  _ <- pure $ copyToClipboard value
  cardField f
