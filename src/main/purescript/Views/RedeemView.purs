module Views.RedeemView where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, fireOnce, loopW)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, form, input, label, span, text, textarea)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Alternative ((*>))
import Control.Applicative (pure)
import Control.Bind (bind, (=<<))
import Control.Category ((>>>))
import Data.Argonaut.Parser (jsonParser)
import Data.Array (mapWithIndex, replicate)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonDecodeError(..), decode)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$), (<$>))
import Data.HeytingAlgebra (not, (&&), (||))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Ord ((<), (>))
import Data.String (length)
import Data.Unit (Unit, unit)
import DataModel.CardVersions.Card (Card(..), toCard)
import DataModel.CardVersions.CurrentCardVersions (currentCardCodecVersion)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Functions.Clipboard (copyToClipboard)
import Functions.EnvironmentalVariables (appURL)
import Functions.Events (cursorToEnd)
import Unsafe.Coerce (unsafeCoerce)
import Views.CardViews (cardContent)
import Views.Components (Enabled(..), dynamicWrapper)
import Views.OverlayView (OverlayColor(..), OverlayStatus(..), overlay)

redeemView :: Enabled -> Widget HTML String
redeemView (Enabled enabled) = do
  form [Props.className "form"] [
    div [Props.className "secret"] []
  , demand $ do
      result <- loopW "" (\v -> do
        div [Props.className "redeemPin"] [
          div [Props.className "pinBackground"] $ mapWithIndex (\index _ -> div [Props.classList [if ((length v) > index) then Just "inserted" else Nothing, if ((length v) == index || ((length v) > index && index == 4)) then Just "current" else Nothing]] []) (replicate 5 unit)
        , label [Props.className "pin"] [
            span [Props.className "label"] [text "Message key"]
          , input [
              Props._type "text"
            , Props.disabled (not enabled)
            , Props.inputMode "numeric"
            , Props.autoFocus true
            , v <$ cursorToEnd >>> unsafePerformEffect <$> Props.onFocus
            , v <$ cursorToEnd >>> unsafePerformEffect <$> Props.onClick
            , v <$ cursorToEnd >>> unsafePerformEffect <$> Props.onKeyUp
            , Props.value v
            , Props.maxLength "5"
            , Props.pattern "^[0-9]+$"
            , (\e -> 
                if (unsafeCoerce e).target.validity.valid
                then (Props.unsafeTargetValue e)
                else v
              ) <$> Props.onChange
            ]
          ]
        ]
      )
      fireOnce $ button [
        Props.disabled (length result < 5)
      , Props.className "redeem"
      , result <$ Props.onClick
      ] [text "redeem"]
  ]

redeemedView :: String -> Widget HTML Unit
redeemedView secret = do
  result <- div [Props.className "redeemedView"] [
    false <$ case toCard <$> (decode currentCardCodecVersion =<< (lmap TypeMismatch $ jsonParser secret)) of
      Right (Card {content}) -> div [Props.className "redeemedCard"] [
                                  text ("Here is your secret card:")
                                , cardContent content
                                ]
                                <> 
                                do
                                  appURL_ <- liftEffect $ appURL
                                  button [Props.className "addCardToAccount"] [
                                    a [Props.href (appURL_ <> "#addCard?" <> secret), Props.target "_blank"] [
                                      text "add to account"
                                    ]
                                  ]
      Left _                 -> label [] [
                                  span [Props.className "label"] [text "Secret"]
                                , dynamicWrapper Nothing secret $ 
                                    textarea [
                                      Props.value secret
                                    , Props.readOnly true
                                    ] []
                                ]
  , true <$ (button [Props.onClick] [text "copy"] *> (liftAff $ copyToClipboard secret))
  ]
  _ <- if result then
    redeemedView secret <|> (liftAff $ delay (Milliseconds 1000.0)) <|> overlay { status: Copy, color: Black, message: "copied" }
  else
    pure unit
  redeemedView secret
  