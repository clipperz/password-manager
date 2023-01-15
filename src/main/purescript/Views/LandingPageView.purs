module Views.LandingPageView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text, ul, li, h1, h3, footer, header, span, a, button)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Functor ((<$))
import Data.Unit (Unit, unit)
-- import DataModel.User (IndexReference)
import DataModel.WidgetState (WidgetState(..))
import Record (merge)
import Views.SimpleWebComponents (simpleButton)
import Views.LoginFormView (LoginDataForm)
import Views.SignupFormView (SignupDataForm, emptyDataForm)
import OperationalWidgets.LoginWidget (loginWidget)
import OperationalWidgets.SignupWidget (signupWidgetWithLogin)

data LandingPageView = SignupView WidgetState SignupDataForm | LoginView WidgetState LoginDataForm

data LandingWidgetAction = LandingPageView LandingPageView | Login

landingPageView :: LandingPageView -> Widget HTML Unit
landingPageView view = do
  result <- div [Props.className pageClassName] [
    div [Props.className "content"] [
      header [] [
        h1 [] [text "clipperz"],
        h3 [] [text "keep it to yourself"]
      ],
      div [Props.className "body"] [
        (case view of
          LoginView state form ->  div [Props.className "bodyContent"] [
                          Login <$ loginWidget state form
                        , (\value -> button [value <$ Props.onClick, Props.disabled false, Props.className "registrationLink"] [text "sign up"]) (LandingPageView (SignupView Default (merge form emptyDataForm)))
                        ]
          SignupView state form -> div [Props.className "bodyContent"] [
                          Login <$ signupWidgetWithLogin state form -- TODO
                        , simpleButton "login" "login" false (LandingPageView (LoginView Default { username: form.username, password: form.password }))
                        ]
        )
      ],
      div [(Props.className "other")] [
        div [(Props.className "links")] [
          ul [] [
            li [] [a [Props.href "https://clipperz.is/about/",          Props.target "_blank"] [text "About"]],
            li [] [a [Props.href "https://clipperz.is/terms_service/",  Props.target "_blank"] [text "Terms of service"]],
            li [] [a [Props.href "https://clipperz.is/privacy_policy/", Props.target "_blank"] [text "Privacy"]]
          ]
        ]
      ],
      footer [] [
        div [Props.className "footerContent"] [
          div [Props.className "applicationVersion"] [
            span [] [text "application version"],
            a [Props.href "https://github.com/clipperz/password-manager/tree/develop"] [text "epsilon-develop"]
          ]
        ]
      ]
    ]
  ]

  case result of
    LandingPageView v -> landingPageView v
    Login             -> pure unit

  where 
    pageClassName = case view of
      LoginView  _ _ -> "page login"
      SignupView _ _ -> "page signup"
