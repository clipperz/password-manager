module Functions.Password where

import Bytes (foldMapBytesToString)
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Semigroupoid ((<<<))
import Data.Array (elem, nub, toUnfoldable)
import Data.Array as Array
import Data.Boolean (otherwise)
import Data.EuclideanRing ((/))
import Data.Foldable (fold)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (toNumber)
import Data.List (filter, List)
import Data.List as List
import Data.Map (Map, values, fromFoldable)
import Data.Monoid (class Monoid)
import Data.Newtype (unwrap, wrap, class Newtype)
import Data.Number (log, isNaN)
import Data.Ord ((<=), (<))
import Data.Ring ((-))
import Data.Semigroup (class Semigroup, (<>))
import Data.Semiring ((+), (*))
import Data.Show (class Show, show)
import Data.String.CodePoints (length, take, drop)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Fortuna (randomBytes)

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

getSets :: Char -> List CharacterSet
getSets c = 
  let
    allSets = values characterSets :: List CharacterSet
    allStrings = (unwrap <$> allSets) :: List String
    allArrays = toCharArray <$> allStrings
    relevantArrays = filter (elem c) allArrays
  in (CharacterSet <<< fromCharArray) <$> relevantArrays

compactSets :: List CharacterSet -> CharacterSet
compactSets = wrap <<< fromCharArray <<< nub <<< toCharArray <<< unwrap <<< fold

data PasswordStrength = VeryWeak | Weak | Acceptable | Strong | VeryStrong
instance showPasswordStrengh :: Show PasswordStrength where
  show VeryWeak   = "very weak"
  show Weak       = "weak"
  show Acceptable = "acceptable"
  show Strong     = "strong"
  show VeryStrong = "very strong"

type PasswordStrengthFunction = String -> PasswordStrength

standardPasswordStrengthFunction :: PasswordStrengthFunction
standardPasswordStrengthFunction = formatPasswordEntropy <<< computePasswordEntropy

computePasswordEntropy :: String -> Number
computePasswordEntropy s =
  let 
    setsPerChar = (getSets <$> (toCharArray s)) :: Array (List CharacterSet)
    sets = (Array.fold setsPerChar) :: List CharacterSet
    adjustmentFactor = 1.0 -- 0.15 * (toNumber (List.length (List.nub (unwrap <$> sets)))) // factor to adjust for the presence of different types of characters
    poolSize = toNumber $ length $ unwrap $ compactSets sets
    pLength = toNumber $ length s
  in adjustmentFactor * pLength * (log poolSize) / (log 2.0)

formatPasswordEntropy :: Number -> PasswordStrength
formatPasswordEntropy n
  | isNaN n   = VeryWeak
  | n < 15.0  = VeryWeak
  | n < 30.0  = Weak
  | n < 65.0  = Acceptable
  | n < 80.0  = Strong
  | otherwise = VeryStrong

randomPassword :: Int -> String -> Aff String
randomPassword l characters = appendRandomChars (repeatStringUpToSize 256 characters) l ""
  where
    appendRandomChars :: String -> Int -> String -> Aff String
    appendRandomChars _     n p | n <= length p = pure (take n p)
    appendRandomChars chars n p = do
      bytes <- randomBytes (n - (length p))
      appendRandomChars chars n (p <> (foldMapBytesToString (characterAtIndex chars) bytes))

    characterAtIndex :: String -> Int -> String
    characterAtIndex s 0 = take 1 s
    characterAtIndex s i | i < length s = take 1 (drop i s)
    characterAtIndex _ _ = ""

    repeatStringUpToSize :: Int -> String -> String
    repeatStringUpToSize n s = repeatStringUpToSize' n s s
      where
        repeatStringUpToSize' :: Int -> String -> String -> String
        repeatStringUpToSize' _  ""  _ = ""
        repeatStringUpToSize' n' s'  a | (length a) + (length s') <= n' = repeatStringUpToSize' n' s' (a <> s')
        repeatStringUpToSize' _  _   a = a
