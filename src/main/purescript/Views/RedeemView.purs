module Views.RedeemView where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, fireOnce, loopW)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, form, input, label, span, text, textarea)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Argonaut.Decode (fromJsonString)
import Data.Array (mapWithIndex, replicate)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$), (<$>))
import Data.HeytingAlgebra (not)
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Ord ((<), (>))
import Data.String (length)
import Data.Unit (Unit, unit)
import DataModel.Card (Card(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Clipboard (copyToClipboard)
import Functions.EnvironmentalVariables (appURL)
import Unsafe.Coerce (unsafeCoerce)
import Views.CardViews (cardContent)
import Views.Components (Enabled(..), dynamicWrapper)
import Views.OverlayView (OverlayStatus(..), overlay)
import Views.SimpleWebComponents (simpleButton)

redeemView :: Enabled -> Widget HTML String
redeemView (Enabled enabled) = do
  form [Props.className "form"] [
    div [Props.className "secret"] []
  , demand $ do
      result <- loopW "" (\v -> do
        div [Props.className "redeemPin"] [
          div [Props.className "pinBackground"] $ mapWithIndex (\index _ -> div [Props.classList [if ((length v) > index) then Just "inserted" else Nothing]] []) (replicate 5 unit)
        , label [Props.className "pin"] [
            span [Props.className "label"] [text "Message key"]
          , input [
              Props._type "text"
            , Props.disabled (not enabled)
            , Props.inputMode "numeric"
            , Props.placeholder "message key"
            , Props.value v
            , Props.maxLength "5"
            , Props.pattern "^[0-9]+$"
            , (\e -> 
                if (unsafeCoerce e).target.validity.valid
                then Props.unsafeTargetValue e
                else v
              ) <$> Props.onChange
            ]
          ]
        ]
      )
      fireOnce (simpleButton "redeem" "redeem" (length result < 5) result)
  ]

redeemedView :: String -> Widget HTML Unit
redeemedView secret = do
  result <- div [Props.className "redeemedView"] [
    false <$ case fromJsonString secret of
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
  , true <$ button [(\_ -> copyToClipboard secret) <$> Props.onClick] [text "copy"]
  ]
  _ <- if result then
    redeemedView secret <|> (liftAff $ delay (Milliseconds 1000.0)) <|> overlay { status: Copy, message: "copied" }
  else
    pure unit
  redeemedView secret
  