module Views.DonationViews where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, div, iframe, span, text)
import Concur.React.Props as Props
import Control.Alt (void, ($>), (<$))
import Control.Alternative (pure)
import Control.Bind (bind, discard)
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Effect.Aff.Class (liftAff)
import Functions.Donations (DonationLevel(..), donationLevelClass)
import Functions.Events (getWindowMessage)


data DonationPageEvent = UpdateDonationLevel | CloseDonationPage

donationIFrame :: String -> Widget HTML DonationPageEvent
donationIFrame destinationPage = do
  res <-  iframe [Props.className "donationIframe", Props.src destinationPage] []
          <>
          (liftAff getWindowMessage)
  pure $ case res of
    "done" -> UpdateDonationLevel
    _      -> CloseDonationPage

donationPage :: DonationLevel -> Widget HTML DonationPageEvent
donationPage DonationWarning =
  div [Props.className "donationPage"] [
    CloseDonationPage <$ div [Props.className "closeButton"] [ button [Props.onClick] [span [] [text "remove field"]] ] 
  , donationIFrame "/donations/app/splash/"
  ]
donationPage _ = text ""

donationReminder :: DonationLevel -> Widget HTML DonationPageEvent
donationReminder DonationOk = text ""
donationReminder donationLevel = do
  div [Props.classList [Just "donationButton", Just $ donationLevelClass donationLevel]] [
    button [void Props.onClick] [span [] [text "Donate"]]
  ]

  event <-  div [Props.classList [Just "donationButton", Just "overlayOpen", Just $ donationLevelClass donationLevel]] [
              div [Props.className "disableOverlay"] [
                div [Props.className "mask", Props.onClick $> CloseDonationPage] []
              , div [Props.className "dialog"] [
                  donationIFrame "/donations/app/"
                ]
              ]
            , button [void Props.onClick] [span [] [text "Donate"]] $> CloseDonationPage
            ]
  case event of
    CloseDonationPage   -> donationReminder donationLevel
    UpdateDonationLevel -> pure event