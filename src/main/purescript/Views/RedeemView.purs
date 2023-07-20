module Views.RedeemView where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, fireOnce, loopS, loopW)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, form, input, label, span, text, textarea)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Argonaut.Decode (fromJsonString)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$), (<$>))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.String (null)
import Data.Unit (Unit, unit)
import DataModel.Card (Card(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Clipboard (copyToClipboard)
import Functions.EnvironmentalVariables (appURL)
import Views.CardViews (cardContent)
import Views.Components (dynamicWrapper)
import Views.OverlayView (OverlayStatus(..), overlay)
import Views.SimpleWebComponents (simpleButton)

redeemView :: Widget HTML String
redeemView = do
  form [Props.className "form"] [
    div [Props.className "secret"] []
  , demand $ do
      result <- loopS "" (\password_ -> do
        newPassword <- loopW password_ (\v -> div [] [
          label [] [
              span [Props.className "label"] [text "Message key"]
            , (Props.unsafeTargetValue) <$> input [
                Props._type "password"
              , Props.placeholder "message key"
              , Props.value v
              , Props.autoComplete "off", Props.autoCorrect "off", Props.autoCapitalize "off", Props.spellCheck false
              , Props.disabled false
              , Props.onChange
              ]
            ]
        ])
        pure newPassword
      )
      fireOnce (simpleButton "redeem" "redeem" (null result) result)
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
  , true <$ button [(\_ -> copyToClipboard secret) <$> Props.onClick] [text "Copy"]
  ]
  _ <- if result then
    redeemedView secret <|> (liftAff $ delay (Milliseconds 1000.0)) <|> overlay { status: Copy, message: "copied" }
  else
    pure unit
  redeemedView secret
  