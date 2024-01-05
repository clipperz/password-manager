module Views.ExportView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, div, h1, h3, p, span, text)
import Concur.React.Props as Props
import Control.Alt (($>))

data ExportEvent = OfflineCopy | UnencryptedCopy

exportView :: Widget HTML ExportEvent
exportView = div [Props._id "exportPage"] [
      h1 [] [text "Export"]
    , div [] [
        h3 [] [text "Offline copy"]
      , p [] [text "Download a read-only portable version of Clipperz. Very convenient when no Internet connection is available."]
      , p [] [text "An offline copy is just a single HTML file that contains both the whole Clipperz web application and your encrypted data."]
      , p [] [text "It is as secure as the hosted Clipperz service since they both share the same code and security architecture."]
      , button [Props.onClick] [span [] [text "download offline copy"]] $> OfflineCopy
      ]
    , div [] [ 
        h3 [] [text "HTML + JSON"]
      , p [] [text "Download a printer-friendly HTML file that lists the content of all your cards."]
      , p [] [text "This same file also contains all your data in JSON format. Please note that file attachments are not included."]
      , p [Props.className "important"] [text "Beware: all data are unencrypted! Therefore make sure to properly store and manage this file."]
      , button [Props.onClick] [span [] [text "download HTML+JSON"]] $> UnencryptedCopy
      ]
    ]