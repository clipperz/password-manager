module DataModel.Password where

import Data.Array (mapMaybe, filter, sort)
import Data.Eq ((/=))
import Data.Foldable (fold)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Monoid (class Monoid)
import Data.Map (Map, fromFoldable, keys, lookup)
import Data.Newtype (class Newtype, unwrap)
import Data.Show (class Show)
import Data.Semigroup (class Semigroup, (<>))
import Data.Set as Set
import Data.String.Common (joinWith)
import Data.String.CodePoints (length, take, drop, fromCodePointArray, toCodePointArray, CodePoint)
import Data.Tuple (Tuple(..), fst)

type PasswordGeneratorSettings = {
    length              :: Int,
--  characterSets       :: Array (Tuple String Boolean),
    characters          :: String
}

newtype CharacterSet = CharacterSet String
instance characterSetShow :: Show CharacterSet where
  show (CharacterSet s) = s
derive newtype instance characterSetSemigroup :: Semigroup CharacterSet
derive newtype instance characterSetMonoid :: Monoid CharacterSet
derive instance newtypeCharacterSet :: Newtype CharacterSet _

elemInCharacterSet :: CodePoint -> CharacterSet -> Boolean
elemInCharacterSet c set = false


capitalLetters    = (CharacterSet "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
lowercaseLetters  = (CharacterSet "abcdefghijklmnopqrstuvwxyz")
numbersChars      = (CharacterSet "0123456789")

defaultCharacterSets :: Array (Tuple String CharacterSet)
defaultCharacterSets = [
  Tuple "A-Z"   capitalLetters    --  (CharacterSet "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
, Tuple "a-z"   lowercaseLetters  --  (CharacterSet "abcdefghijklmnopqrstuvwxyz")
, Tuple "0-9"   numbersChars      --  (CharacterSet "0123456789"),
, Tuple "space" (CharacterSet " ")
, Tuple "!#?"   (CharacterSet """~`!@#$%^&*()-_=+,.<>/?[]{}\|";':""")
, Tuple "😀"    (CharacterSet "💪😀😢🤷🥶")
]

{-
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
-}

standardPasswordGeneratorSettings :: PasswordGeneratorSettings
-- standardPasswordGeneratorSettings = { length: 16, characterSets: defaultCharacterSets, characters: defaultCharacters}
-- standardPasswordGeneratorSettings = { length: 16, characters: "ABCDEFGHIJKLMNOPQRSTUVWXYZ" <> "abcdefghijklmnopqrstuvwxyz" <> "0123456789"}
standardPasswordGeneratorSettings = { length: 16, characters: fromCodePointArray $ sort $ toCodePointArray $ joinWith "" (unwrap <$> [capitalLetters, lowercaseLetters, numbersChars])}
