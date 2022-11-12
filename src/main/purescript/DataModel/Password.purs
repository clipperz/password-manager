module DataModel.Password where

import Data.Array (mapMaybe, filter)
import Data.Eq ((/=))
import Data.Foldable (fold)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Monoid (class Monoid)
import Data.Map (Map, fromFoldable, keys, lookup)
import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Data.Semigroup (class Semigroup)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst)

type PasswordGeneratorSettings = {
    length              :: Int,
    characterSets       :: Array (Tuple String Boolean),
    characters          :: String
}

newtype CharacterSet = CharacterSet String
instance characterSetShow :: Show CharacterSet where
  show (CharacterSet s) = s
derive newtype instance characterSetSemigroup :: Semigroup CharacterSet
derive newtype instance characterSetMonoid :: Monoid CharacterSet
derive instance newtypeCharacterSet :: Newtype CharacterSet _

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

standardPasswordGeneratorSettings :: PasswordGeneratorSettings
standardPasswordGeneratorSettings = { length: 16, characterSets: defaultCharacterSets, characters: defaultCharacters}
