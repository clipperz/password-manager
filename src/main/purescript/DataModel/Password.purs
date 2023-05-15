module DataModel.Password where

import Data.Array (elem, sort)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap)
import Data.Show (class Show)
import Data.Semigroup (class Semigroup)
import Data.String.Common (joinWith)
import Data.String.CodePoints (CodePoint, fromCodePointArray, toCodePointArray)
import Data.Tuple (Tuple(..))

type PasswordGeneratorSettings = {
    length              :: Int,
    characters          :: String
}

newtype CharacterSet = CharacterSet String
instance characterSetShow :: Show CharacterSet where
  show (CharacterSet s) = s
derive newtype instance characterSetSemigroup :: Semigroup CharacterSet
derive newtype instance characterSetMonoid :: Monoid CharacterSet
derive instance newtypeCharacterSet :: Newtype CharacterSet _

elemInCharacterSet :: CodePoint -> CharacterSet -> Boolean
elemInCharacterSet c (CharacterSet set) = elem c (toCodePointArray set)

capitalLetters :: CharacterSet
capitalLetters    = (CharacterSet "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

lowercaseLetters :: CharacterSet
lowercaseLetters  = (CharacterSet "abcdefghijklmnopqrstuvwxyz")

numbersChars :: CharacterSet
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

standardPasswordGeneratorSettings :: PasswordGeneratorSettings
standardPasswordGeneratorSettings = { length: 16, characters: fromCodePointArray $ sort $ toCodePointArray $ joinWith "" (unwrap <$> [capitalLetters, lowercaseLetters, numbersChars])}
