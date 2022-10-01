module Views.PasswordGenerator where
  
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div', text)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Semigroupoid ((<<<))
import Data.Array (fold, filter, mapMaybe, concat)
import Data.Either (Either(..), either)
import Data.Eq((/=))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra (not)
import Data.Int (fromString)
import Data.Map (Map, update, fromFoldable, toUnfoldable, keys, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Semigroup ((<>), class Semigroup)
import Data.Set as Set
import Data.Show (show, class Show)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Data.Tuple(Tuple(..), fst)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

import Functions.Password (randomPassword)
import Views.SimpleWebComponents (simpleButton, simpleNumberInputWidget, disabledSimpleTextInputWidget, simpleTextInputWidget, simpleCheckboxWidget)

data AsyncValue a = Loading (Maybe a) | Done a
instance showAsyncValue :: (Show a) => Show (AsyncValue a) where
    show (Loading p) = "loading[" <> show p <> "]"
    show (Done p) = "done[" <> show p <> "]"

extractValue :: forall a. Monoid a => AsyncValue a -> a
extractValue v = 
  case v of
    Done a -> a
    Loading (Just a) -> a
    Loading (Nothing) -> mempty

type Settings = {
    length              :: Int,
    characterSets       :: Array (Tuple String Boolean),
    characters          :: String
}

newtype CharacterSet = CharacterSet String
instance characterSetShow :: Show CharacterSet where
  show (CharacterSet s) = s
derive newtype instance characterSetSemigroup :: Semigroup CharacterSet
derive newtype instance characterSetMonoid :: Monoid CharacterSet

characterSets :: Map String CharacterSet
characterSets = fromFoldable [
    Tuple "A-Z" (CharacterSet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
    Tuple "a-z" (CharacterSet "abcdefghijklmnopqrstuvwxyz"),
    Tuple "0-9" (CharacterSet "0123456789"),
    Tuple "space" (CharacterSet " "),
    Tuple "!#?" (CharacterSet """~`!@#$%^&*()-_=+,.<>/?[]{}\|";':""")
]

defaultCharacterSets :: Array (Tuple String Boolean)
defaultCharacterSets = (\k -> Tuple k (k /= "space")) <$> (Set.toUnfoldable (keys characterSets))

charactersFromSets :: Array (Tuple String Boolean) -> CharacterSet
charactersFromSets ar =
  let chosenKeys = (fst <$> (filter (\(Tuple _ checked) -> checked) ar)) :: Array String
      cSets = (mapMaybe (\a -> a) $ (\k -> lookup k characterSets) <$> chosenKeys) :: Array CharacterSet
  in fold cSets

defaultCharacters :: String
defaultCharacters = 
  let (CharacterSet chars) = charactersFromSets defaultCharacterSets
  in chars

initialSettings :: Settings
initialSettings = { length: 16, characterSets: defaultCharacterSets, characters: defaultCharacters}

---------------------------

passwordGenerator :: Widget HTML String
passwordGenerator = composedWidget initialSettings (Loading Nothing)

data ComposedWidgetAction = ModifiedSettingsAction Settings 
                          | RequestedNewSuggestion
                          | ApprovedSuggestion String
                          | ObtainedNewSuggestion String
composedWidget :: Settings -> AsyncValue String -> Widget HTML String
composedWidget settings av = do
  res <- case av of
    -- Just: just show the widget and wait for the user to do something
    Done _    -> widget settings av
    -- Nothing: show the widget while waiting for the computation of a password
    Loading _ -> widget settings av <|> (ObtainedNewSuggestion <$> (computePassword settings))
  case res of
    ModifiedSettingsAction s'   -> composedWidget s' (Loading (Just (extractValue av))) -- need to regenerate password
    RequestedNewSuggestion      -> composedWidget settings (Loading (Just (extractValue av)))
    ObtainedNewSuggestion  pswd -> composedWidget settings (Done pswd)
    ApprovedSuggestion     pwsd -> pure pwsd
  where 
    computePassword :: Settings -> Widget HTML String
    computePassword s' = liftAff $ randomPassword s'.length s'.characters

    widget :: Settings -> AsyncValue String -> Widget HTML ComposedWidgetAction
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
        PasswordChange <$> disabledSimpleTextInputWidget "generated_password" (text "Generated password") b s
      , InsertPassword <$> simpleButton "Insert" b s
      ]
      case res of
        PasswordChange p       -> suggestionWidget $ Done p
        UpdatePassword         -> suggestionWidget $ Done ""
        InsertPassword newPswd -> pure $ Right newPswd 

data PasswordWidgetAction = PasswordChange String | UpdatePassword | InsertPassword String
passwordWidget :: Settings -> Maybe String -> Widget HTML String
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
        PasswordChange <$> simpleTextInputWidget "generated_password" (text "GeneratedPassword") s
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
settingsWidget :: Settings -> Widget HTML Settings
settingsWidget s = do
  let lengthWidget = (LengthChange <<< (fromMaybe s.length) <<< fromString) <$> simpleNumberInputWidget "password_length" (text "Password Length") (show s.length)
  let charsWidget = Chars <$> simpleTextInputWidget "password_characters" (text "Possible characters") s.characters
  let setsWidgets = (\(Tuple id v) -> (CharSetToggle id) <$ simpleCheckboxWidget ("char_set_" <> id) (text id) v) <$> s.characterSets
  res <- div' $ concat [[lengthWidget], setsWidgets, [charsWidget]]
  case res of
    LengthChange n -> pure $ s { length = n }
    CharSetToggle key -> do
      let newArray = (toUnfoldable <<< update (\v -> Just (not v)) key <<< fromFoldable) s.characterSets
      let (CharacterSet chars) = charactersFromSets newArray
      pure $ s { characterSets = newArray, characters = chars }
    Chars str -> pure $ s { characterSets = (checkCharSetsToggle s.characterSets str), characters = str }

checkCharSetsToggle :: Array (Tuple String Boolean) -> String -> Array (Tuple String Boolean)
checkCharSetsToggle charSets chars = do
  (\(Tuple set _) -> (Tuple set (contains (Pattern (show $ fromMaybe (CharacterSet "") $ lookup set characterSets)) chars))) <$> charSets