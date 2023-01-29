module Views.CardViews where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, h3, li', p, p', text, ul, textarea)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.Semigroup ((<>))
import Data.Show (show, class Show)
import DataModel.AppState (ProxyConnectionStatus(..))
import DataModel.Card (CardField(..), CardValues(..), Card(..))
import Functions.Clipboard (copyToClipboard)
import Views.SimpleWebComponents (simpleButton, confirmationWidget)

-- -----------------------------------

data CardAction = Edit Card | Clone Card | Archive Card | Restore Card | Delete Card | Used Card | Exit Card
instance showCardAction :: Show CardAction where
  show (Edit _)    = "edit"
  show (Used _)    = "used"
  show (Clone _)   = "clone"
  show (Archive _) = "archive"
  show (Restore _) = "restore"
  show (Delete _)  = "delete"
  show (Exit _ )   = "exit"

-- -----------------------------------

cardView :: Card -> ProxyConnectionStatus -> Widget HTML CardAction
cardView c@(Card r) proxyConnectionStatus = do
  res <- div [Props._id "cardView"] [
    cardActions c proxyConnectionStatus
  , (Used c) <$ cardContent r.content
  ]
  case res of
    Delete _ -> do
      confirmation <- div [] [
        false <$ cardActions c proxyConnectionStatus
      , cardContent r.content
      , confirmationWidget "Are you sure you want to delete this card?"
      ]
      if confirmation then pure res else cardView c proxyConnectionStatus
    _ -> pure res

cardActions :: Card -> ProxyConnectionStatus -> Widget HTML CardAction
cardActions c@(Card r) proxyConnectionStatus = div [Props.className "cardActions"] [
    simpleButton (show (Exit c)) "exit"   false    (Exit c)
      , simpleButton "edit"       (show (Edit c))    disabled (Edit c)
      , simpleButton "clone"      (show (Clone c))   disabled (Clone c)
      , if r.archived then
          simpleButton "restore"  (show (Restore c)) disabled (Restore c)
        else
          simpleButton "archive"  (show (Archive c)) disabled (Archive c)
      , simpleButton "delete"     (show (Delete c))  disabled (Delete c)
    -- ]
]
  where
    disabled = case proxyConnectionStatus of
      ProxyOnline   -> false
      ProxyOffline  -> true

cardContent :: forall a. CardValues -> Widget HTML a
cardContent (CardValues {title: t, tags: ts, fields: fs, notes: n}) = div [Props._id "cardContent"] [
  h3  [Props.className "card_title"]  [text t]
, div [Props.className "card_tags"]   [ul  []   $ (\s -> li' [text s]) <$> ts]
, div [Props.className "card_fields"] $ cardField <$> fs
, div [Props.className "card_notes"]  [text n]
]

cardField :: forall a. CardField -> Widget HTML a
cardField f@(CardField {name, value, locked}) = do
  res <- div [Props.className "fieldValue"] [
    div [Props.className "fieldLabel"] [text name]
  , textarea ((if locked then [Props.className "PASSWORD"] else []) <> [Props.onClick, Props.disabled true, Props.value value]) []
  ] --TODO add class based on content for urls and emails
  _ <- pure $ copyToClipboard value
  cardField f
