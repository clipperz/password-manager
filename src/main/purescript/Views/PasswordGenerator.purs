module Views.PasswordGenerator where
  
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, div', text)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Semigroupoid ((<<<))
import Data.Array (concat)
import Data.Either (Either(..), either)
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra (not)
import Data.Int (fromString)
import Data.Map (update, fromFoldable, toUnfoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Data.Tuple(Tuple(..))
import DataModel.AsyncValue (AsyncValue(..))
import DataModel.Password (PasswordGeneratorSettings, CharacterSet(..), characterSets, charactersFromSets)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Password (randomPassword)
import Views.SimpleWebComponents (simpleButton, simpleNumberInputWidget, disabledSimpleTextInputWidget, simpleTextInputWidget, simpleCheckboxWidget)

extractValue :: forall a. Monoid a => AsyncValue a -> a
extractValue v = 
  case v of
    Done     a        -> a
    Loading (Just a)  -> a
    Loading (Nothing) -> mempty

---------------------------

passwordGenerator :: PasswordGeneratorSettings -> Widget HTML String
passwordGenerator initialSettings = composedWidget initialSettings (Loading Nothing)

data ComposedWidgetAction = ModifiedSettingsAction PasswordGeneratorSettings 
                          | RequestedNewSuggestion
                          | ApprovedSuggestion String
                          | ObtainedNewSuggestion String
composedWidget :: PasswordGeneratorSettings -> AsyncValue String -> Widget HTML String
composedWidget settings av = do
  res <- case av of
    Done    _ -> widget settings av
    Loading _ -> widget settings av <|> (ObtainedNewSuggestion <$> (computePassword settings))
  case res of
    ModifiedSettingsAction s'   -> composedWidget s' (Loading (Just (extractValue av))) -- need to regenerate password
    RequestedNewSuggestion      -> composedWidget settings (Loading (Just (extractValue av)))
    ObtainedNewSuggestion  pswd -> composedWidget settings (Done pswd)
    ApprovedSuggestion     pwsd -> pure pwsd
  where 
    computePassword :: PasswordGeneratorSettings -> Widget HTML String
    computePassword s' = liftAff $ randomPassword s'.length s'.characters

    widget :: PasswordGeneratorSettings -> AsyncValue String -> Widget HTML ComposedWidgetAction
    widget s v = div' [
      ModifiedSettingsAction <$> settingsWidget s
    , simpleButton "Regenerate" false RequestedNewSuggestion
    , (either ObtainedNewSuggestion ApprovedSuggestion) <$> suggestionWidget v
    ]

suggestionWidget :: AsyncValue String -> Widget HTML (Either String String)
suggestionWidget av =
  case av of  
    Done p  -> go false p
    Loading mp -> go true (fromMaybe "" mp)
  where
    go :: Boolean -> String -> Widget HTML (Either String String)
    go b s = do
      res <- div' [
        PasswordChange <$> disabledSimpleTextInputWidget "generated_password" (text "Generated password") b "" s
      , InsertPassword <$> simpleButton "Insert" b s
      ]
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
        PasswordChange <$> simpleTextInputWidget "generated_password" (text "GeneratedPassword") "" s
      , simpleButton "Regenerate" false UpdatePassword
      , InsertPassword <$> simpleButton "Insert" false s
      ]
      case res of
        PasswordChange p       -> passwordWidget settings (Just p)
        UpdatePassword         -> do
          newPassword <- liftAff $ randomPassword settings.length settings.characters
          passwordWidget settings (Just newPassword)
        InsertPassword newPswd -> pure newPswd 

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

checkCharSetsToggle :: Array (Tuple String Boolean) -> String -> Array (Tuple String Boolean)
checkCharSetsToggle charSets chars = do
  (\(Tuple set _) -> (Tuple set (contains (Pattern (show $ fromMaybe (CharacterSet "") $ lookup set characterSets)) chars))) <$> charSets
