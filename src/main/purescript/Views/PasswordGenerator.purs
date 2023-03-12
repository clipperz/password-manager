module Views.PasswordGenerator where
  
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, div', text, label, span, input, textarea, header, button)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Semigroupoid ((<<<))
import Data.Array (concat, nub, difference, all, elem, sort)
import Data.Either (Either(..), either)
import Data.Eq ((==), (/=))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra (not, (&&))
import Data.Int (fromString)
import Data.Map (update, fromFoldable, toUnfoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe, fromJust, isJust)
import Data.Monoid (class Monoid, mempty)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String (contains)
-- import Data.String.CodeUnits (fromCodePointArray, toCodePointArray)
import Data.String.CodePoints (length, take, drop, fromCodePointArray, toCodePointArray, CodePoint)
import Data.String.Pattern (Pattern(..))
import Data.Tuple(Tuple(..))
import DataModel.AsyncValue (AsyncValue(..))
import DataModel.Password (PasswordGeneratorSettings, CharacterSet(..), defaultCharacterSets)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Password (randomPassword, standardPasswordStrengthFunction, passwordStrengthClass)
import Views.Components (dynamicWrapper)
import Views.SimpleWebComponents (simpleButton, simpleNumberInputWidget, disabledSimpleTextInputWidget, simpleTextInputWidget, simpleCheckboxWidget)


extractValue :: forall a. Monoid a => AsyncValue a -> a
extractValue v = 
  case v of
    Done     a        -> a
    Loading (Just a)  -> a
    Loading (Nothing) -> mempty

---------------------------

passwordGenerator :: PasswordGeneratorSettings -> Widget HTML (Tuple String (Maybe PasswordGeneratorSettings))
-- passwordGenerator initialSettings = composedWidget initialSettings false (Loading Nothing)
passwordGenerator initialSettings = do
  result@(Tuple newPassword newSettings) <- composedWidget initialSettings false (Loading Nothing) --check if settings have changed 
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
    widget s isOpen v = div [Props.className "passwordGeneratorForm"] [
      div [Props.classList [Just "optionWrapper", Just (if isOpen then "open" else "close")]] [
        header [] [button [(TogglePasswordSettings $ not isOpen) <$ Props.onClick] [text "options"]]
      , ModifiedSettingsAction <$> settingsWidget s
      ]
    , div [Props.className "passwordGenerator"] [
        simpleButton "generatePassword" "generate password" false RequestedNewSuggestion
      , (either ObtainedNewSuggestion ApprovedSuggestion) <$> suggestionWidget v
      ]
    ]

suggestionWidget :: AsyncValue String -> Widget HTML (Either String String)
suggestionWidget av =
  case av of  
    Done p  -> go false p
    Loading mp -> go true (fromMaybe "" mp)
  where
    go :: Boolean -> String -> Widget HTML (Either String String)
    go b s = do
      res <-
        -- PasswordChange <$> disabledSimpleTextInputWidget "passwordSuggestion" (text "Generated password") b "" s
        (PasswordChange <$> (div [Props.className "generatedValue"] [
          label [] [
            span [Props.className "label"] [text "Generated value"]
          , dynamicWrapper Nothing s $ textarea [Props.rows 1, Props.spellCheck false, Props.disabled b, Props.value s, Props.unsafeTargetValue <$> Props.onChange] [] 
          ]
        , div [Props.classList [Just "entropyWrapper", Just $ passwordStrengthClass (standardPasswordStrengthFunction s){--, Just $ show $ computePasswordEntropy s--}]] [] --TODO
        ]))
        <>
        (InsertPassword <$> simpleButton "setPassword" "set password" b s)
      
      case res of
        PasswordChange p       -> suggestionWidget $ Done p
        UpdatePassword         -> suggestionWidget $ Done ""
        InsertPassword newPswd -> pure $ Right newPswd 

data PasswordWidgetAction = PasswordChange String | UpdatePassword | InsertPassword String
passwordWidget :: PasswordGeneratorSettings -> Maybe String -> Widget HTML String
passwordWidget settings str =
  case str of  
    Just p  -> go p
    Nothing -> do
      pswd <- liftAff $ randomPassword settings.length settings.characters
      go pswd
  where
    go :: String -> Widget HTML String
    go s = do
      liftEffect $ log s
      res <- div' [
        PasswordChange <$> simpleTextInputWidget "password" (text "GeneratedPassword") "" s
      , simpleButton "regenerate" "Regenerate" false UpdatePassword
      , InsertPassword <$> simpleButton "insert" "Insert" false s
      ]
      case res of
        PasswordChange p       -> passwordWidget settings (Just p)
        UpdatePassword         -> do
          newPassword <- liftAff $ randomPassword settings.length settings.characters
          passwordWidget settings (Just newPassword)
        InsertPassword newPswd -> pure newPswd 

{-
data SettingsWidgetAction = LengthChange Int | CharSetToggle String | Chars String
settingsWidget :: PasswordGeneratorSettings -> Widget HTML PasswordGeneratorSettings
settingsWidget s = do
  let lengthWidget = (LengthChange <<< (fromMaybe 0) <<< fromString) <$> simpleNumberInputWidget "password_length" (text "Password Length") "" (show s.length)
  let setsWidgets = (\(Tuple id v) -> (CharSetToggle id) <$ simpleCheckboxWidget ("char_set_" <> id) (text id) false v) <$> s.characterSets
  let charsWidget = Chars <$> simpleTextInputWidget "password_characters" (text "Possible characters") "" s.characters
  res <- div [Props.className "passwordGeneratorSettings"] $ concat [[lengthWidget], setsWidgets, [charsWidget]]
  case res of
    LengthChange  n   -> pure $ s { length = n }
    CharSetToggle key -> do
      let newArray = (toUnfoldable <<< update (\v -> Just (not v)) key <<< fromFoldable) s.characterSets
      let (CharacterSet chars) = charactersFromSets newArray
      pure $ s { characterSets = newArray, characters = chars }
    Chars         str -> pure $ s { characterSets = (checkCharSetsToggle s.characterSets str), characters = str }
-}

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
    , input [Props._type "number", Props.value $ show length, Props.onChange]
    , span  [Props.className "unit"] [text "characters"]
    ]
  , Chars <$> label [Props.className "charList"] [
      span  [Props.className "label"] [text "characters"]
    , div   [Props.className "charset"] [
        div [Props.className "charsetSets"] ((charsetSelector characters) <$> defaultCharacterSets)
      , dynamicWrapper Nothing characters $ textarea [Props.rows 1, Props.spellCheck false, Props.value characters, Props.unsafeTargetValue <$> Props.onChange] [] 
      -- , textarea [Props.value characters, Props.unsafeTargetValue <$> Props.onChange, Props.value characters] []
    ]
  ]
  ]
{-
  let lengthWidget = (LengthChange <<< (fromMaybe 0) <<< fromString) <$> simpleNumberInputWidget "password_length" (text "Password Length") "" (show s.length)
  let setsWidgets = (\(Tuple id v) -> (CharSetToggle id) <$ simpleCheckboxWidget ("char_set_" <> id) (text id) false v) <$> s.characterSets
  let charsWidget = Chars <$> simpleTextInputWidget "password_characters" (text "Possible characters") "" s.characters
  res <- div [Props.className "passwordGeneratorSettings"] $ concat [[lengthWidget], setsWidgets, [charsWidget]]
-}
  case res of
    LengthChange  n   -> pure $ s { length = n }
    -- CharSetToggle key -> do
    --   let newArray = (toUnfoldable <<< update (\v -> Just (not v)) key <<< fromFoldable) s.characterSets
    --   let (CharacterSet chars) = charactersFromSets newArray
    --   pure $ s { characterSets = newArray, characters = chars }
    Chars         str -> pure $ s { characters = str }


-- checkCharSetsToggle :: Array (Tuple String Boolean) -> String -> Array (Tuple String Boolean)
-- checkCharSetsToggle charSets chars = do
--   (\(Tuple set _) -> (Tuple set (contains (Pattern (show $ fromMaybe (CharacterSet "") $ lookup set defaultCharacterSets)) chars))) <$> charSets
