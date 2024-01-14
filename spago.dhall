{ name = "password-generator"
, sources = [ "src/**/*.purs" ]
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arraybuffer"
  , "arraybuffer-builder"
  , "arraybuffer-types"
  , "arrays"
  , "b64"
  , "bifunctors"
  , "bigints"
  , "codec-argonaut"
  , "concur-core"
  , "concur-react"
  , "console"
  , "control"
  , "datetime"
  , "decimals"
  , "effect"
  , "either"
  , "encoding"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "formatters"
  , "fortuna"
  , "functions"
  , "http-methods"
  , "identity"
  , "integers"
  , "js-promise-aff"
  , "js-uri"
  , "lcg"
  , "lists"
  , "maybe"
  , "markdown-it"
  , "media-types"
  , "newtype"
  , "now"
  , "numbers"
  , "ordered-collections"
  , "prelude"
  , "profunctor"
  , "quickcheck"
  , "react"
  , "record"
  , "spec"
  , "strings"
  , "subtlecrypto"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "unsafe-coerce"
  , "variant"
  , "web-clipboard"
  , "web-dom"
  , "web-file"
  , "web-html"
  , "web-storage"
  ,	"web-xhr"
  ]
, packages = ./packages.dhall
}
