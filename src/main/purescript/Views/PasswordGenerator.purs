module Views.PasswordGenerator where
  
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, div, h4, header, input, label, span, text, textarea)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Semigroupoid ((<<<))
import Data.Array (all, difference, elem, nub, sort)
import Data.Either (Either(..), either)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra (not)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.CodePoints (fromCodePointArray, toCodePointArray)
import Data.Tuple (Tuple(..))
import DataModel.AsyncValue (AsyncValue(..))
import DataModel.Password (PasswordGeneratorSettings, CharacterSet(..), defaultCharacterSets)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Functions.Clipboard (copyToClipboard)
import Functions.Password (randomPassword)
import Views.Components (dynamicWrapper, entropyMeter)
import Views.OverlayView (overlay)
import Views.OverlayView as OverlayStatus

extractValue :: forall a. Monoid a => AsyncValue a -> a
extractValue v = 
  case v of
    Done     a        -> a
    Loading (Just a)  -> a
    Loading (Nothing) -> mempty

---------------------------

passwordGenerator :: PasswordGeneratorSettings -> AsyncValue String -> Widget HTML (Tuple String (Maybe PasswordGeneratorSettings))
passwordGenerator initialSettings initialPassword = do
  result@(Tuple newPassword newSettings) <- composedWidget initialSettings false initialPassword --check if settings have changed 
  pure $ case newSettings of
    Just s -> if s == initialSettings then Tuple newPassword Nothing else result 
    Nothing -> result

data ComposedWidgetAction = ModifiedSettingsAction PasswordGeneratorSettings 
                          | RequestedNewSuggestion
                          | ApprovedSuggestion String
                          | ObtainedNewSuggestion String
                          | TogglePasswordSettings Boolean
composedWidget :: PasswordGeneratorSettings -> Boolean -> AsyncValue String -> Widget HTML (Tuple String (Maybe PasswordGeneratorSettings))
composedWidget settings isOpen av = do
  res <- case av of
    Done    _ -> widget settings isOpen av
    Loading _ -> widget settings isOpen av <|> (ObtainedNewSuggestion <$> (computePassword settings))
  case res of
    ModifiedSettingsAction s'   -> composedWidget s' isOpen (Loading (Just (extractValue av))) -- need to regenerate password
    RequestedNewSuggestion      -> composedWidget settings isOpen (Loading (Just (extractValue av)))
    TogglePasswordSettings bool -> composedWidget settings bool   av
    ObtainedNewSuggestion  pswd -> composedWidget settings isOpen (Done pswd)
    ApprovedSuggestion     pwsd -> pure (Tuple pwsd (Just settings))
  where 
    computePassword :: PasswordGeneratorSettings -> Widget HTML String
    computePassword s' = liftAff $ randomPassword s'.length s'.characters

    widget :: PasswordGeneratorSettings -> Boolean -> AsyncValue String -> Widget HTML ComposedWidgetAction
    widget s _ v = div [Props.className "passwordGeneratorForm"] [
      div [Props.classList [Just "optionWrapper", Just (if isOpen then "open" else "close")]] [
        header [] [button [(TogglePasswordSettings $ not isOpen) <$ Props.onClick] [text "options"]]
      , ModifiedSettingsAction <$> settingsWidget s
      ]
    , div [Props.className "passwordGenerator"] [
        h4 [Props.className "label"] [text "Generated value"]
      , div [Props.className "valueGeneration"] [
          RequestedNewSuggestion <$ button [Props.className "generatePassword", Props.title "regenerate", Props.onClick] [span [] [text "generate password"]]
        , (either ObtainedNewSuggestion ApprovedSuggestion) <$> suggestionWidget v
        ]
      ]
      <> 
      case v of
        Done p    -> (ApprovedSuggestion <$> button [Props.className "sharePassword", p <$ Props.onClick] [span [] [text "share"]])
        Loading _ -> button [Props.className "sharePassword", Props.disabled true] [span [] [text "share"]]
    ]

data PasswordWidgetAction = PasswordChange String | UpdatePassword | InsertPassword String | CopyPassword String

suggestionWidget :: AsyncValue String -> Widget HTML (Either String String)
suggestionWidget av =
  case av of  
    Done p  -> go false p
    Loading mp -> go true (fromMaybe "" mp)
  where
    go :: Boolean -> String -> Widget HTML (Either String String)
    go b s = do
      res <-
        div [Props.className "generatedValue"] [
           PasswordChange <$> label [] [
              span [Props.className "label"] [text "generatedValue"]
            , dynamicWrapper Nothing s $ textarea [Props.rows 1, Props.spellCheck false, Props.disabled b, Props.value s, Props.unsafeTargetValue <$> Props.onChange] []
            ]
          , entropyMeter s
        ]
        <>
        (CopyPassword s <$  button [Props.className "copy", Props.title "copy", (\_ -> copyToClipboard s) <$> Props.onClick] [span [] [text "copy"]])
        <>
        (InsertPassword <$> button [Props.className "setPassword", Props.title "insert", Props.disabled b, s <$ Props.onClick] [span [] [text "set password"]])
      
      case res of
        PasswordChange p       -> suggestionWidget $ Done p
        CopyPassword p         -> (suggestionWidget $ Done p) <|> (liftAff $ (Left p) <$ delay (Milliseconds 1000.0)) <|> overlay { status: OverlayStatus.Copy, message: "copied" }
        UpdatePassword         -> suggestionWidget $ Done ""
        InsertPassword newPswd -> pure $ Right newPswd 

charsetSelector :: String -> (Tuple String CharacterSet) -> Widget HTML String
charsetSelector currentSelection (Tuple charsetName (CharacterSet charsetString)) = do
  let currentSelectionChars = toCodePointArray currentSelection
  let charsetChars          = toCodePointArray charsetString
  let isChecked = all (\c -> elem c currentSelectionChars) charsetChars
  checked <-  label [Props.classList [Just charsetName, Just "charsetSelector"]] [
                span [Props.className "label"] [text charsetName]
              , (not isChecked) <$ input [Props._type "checkbox", Props.checked isChecked, Props.onChange]
              ]
  pure $ fromCodePointArray $ sort $ case checked of
    true  -> nub $ (currentSelectionChars <> charsetChars)
    false -> difference currentSelectionChars charsetChars

data SettingsWidgetAction = LengthChange Int | Chars String
settingsWidget :: PasswordGeneratorSettings -> Widget HTML PasswordGeneratorSettings
settingsWidget s = do
  let length = s.length
  let characters = s.characters
  res <- div [Props.className "passwordSettings"] [
    (LengthChange <<< (fromMaybe 0) <<< fromString <<< Props.unsafeTargetValue) <$> label [Props.className "passwordLength"] [
      span  [Props.className "label"] [text "length"]
    , input [Props._type "number", Props.value $ show length, Props.onChange, Props.min "1"]
    , span  [Props.className "unit"] [text "characters"]
    ]
  , Chars <$> div [Props.className "charList"] [
      h4  [Props.className "label"] [text "character sets"]
    , div [Props.className "charset"] [
        div [Props.className "charsetSets"] ((charsetSelector characters) <$> defaultCharacterSets)
      , label [Props.className "charListLabel"] [
          span [Props.className "label"] [text "charList"]
        , dynamicWrapper Nothing characters $ textarea [Props.rows 1, Props.spellCheck false, Props.value characters, Props.unsafeTargetValue <$> Props.onChange] [] 
        ]
    ]
  ]
  ]

  case res of
    LengthChange  n   -> pure $ s { length = n }
    Chars         str -> pure $ s { characters = str }
