module Functions.Password
  ( PasswordStrength
  , PasswordStrengthFunction
  , passwordStrengthClass
  , randomPassword
  , standardPasswordStrengthFunction
  )
  where

import Bytes (foldMapBytesToString)
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Semigroupoid ((<<<))
import Data.Array (elem, nub, filter)
import Data.Array as Array
import Data.Boolean (otherwise)
import Data.EuclideanRing ((/))
import Data.Foldable (fold)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (toNumber)
import Data.List (List)
import Data.Map (values)
import Data.Newtype (unwrap, wrap)
import Data.Number (log, isNaN)
import Data.Ord ((<=), (<), (>=))
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Semiring ((+), (*))
import Data.Show (class Show)
import Data.String.CodePoints (length, take, drop, fromCodePointArray, toCodePointArray, CodePoint)
-- import Data.String.CodeUnits ()
import Data.Tuple (snd)
import DataModel.Password (CharacterSet(..), defaultCharacterSets, elemInCharacterSet)
import Effect.Aff (Aff)
import Effect.Fortuna (randomBytes)


-- filter (elemInCharacterSet c) (snd <$> defaultCharacterSets)
{-
getSets :: Char -> List CharacterSet
getSets c = 
  let
    allSets = values defaultCharacterSets   :: Array CharacterSet
    allStrings = (unwrap <$> allSets)       :: Array String
    allArrays = toCodePointArray <$> allStrings
    relevantArrays = filter (elem c) (snd <$> defaultCharacterSets)
  in (CharacterSet <<< fromCodePointArray) <$> relevantArrays

compactSets :: List CharacterSet -> CharacterSet
compactSets = wrap <<< fromCodePointArray <<< nub <<< toCodePointArray <<< unwrap <<< fold
-}

data PasswordStrength = VeryWeak | Weak | Acceptable | Strong | VeryStrong
instance showPasswordStrengh :: Show PasswordStrength where
  show VeryWeak   = "very weak"
  show Weak       = "weak"
  show Acceptable = "acceptable"
  show Strong     = "strong"
  show VeryStrong = "very strong"

passwordStrengthClass :: PasswordStrength -> String
passwordStrengthClass VeryWeak    = "veryWeak"
passwordStrengthClass Weak        = "weak"
passwordStrengthClass Acceptable  = "acceptable"
passwordStrengthClass Strong      = "strong"
passwordStrengthClass VeryStrong  = "veryStrong"


type PasswordStrengthFunction = String -> PasswordStrength

standardPasswordStrengthFunction :: PasswordStrengthFunction
standardPasswordStrengthFunction = formatPasswordEntropy <<< computePasswordEntropy

matchingCharacterSet :: CodePoint -> Array CharacterSet
matchingCharacterSet c = filter (elemInCharacterSet c) (snd <$> defaultCharacterSets)

computePasswordEntropy :: String -> Number
computePasswordEntropy s =
  let
    
    relevantCharsets = (matchingCharacterSet <$> (toCodePointArray s)) :: Array (Array CharacterSet)
    relevantChars = (fromCodePointArray <<< nub <<< fold) $ (toCodePointArray <<< unwrap) <$> fold relevantCharsets :: String
    adjustmentFactor = 1.0 -- 0.15 * (toNumber (List.length (List.nub (unwrap <$> sets)))) // factor to adjust for the presence of different types of characters
    -- poolSize = toNumber $ length $ unwrap $ compactSets sets
    poolSize  = toNumber $ length relevantChars
    pLength   = toNumber $ length s
  in adjustmentFactor * pLength * (log poolSize) / (log 2.0)  

formatPasswordEntropy :: Number -> PasswordStrength
formatPasswordEntropy n
  -- | isNaN n   = VeryWeak
  | n >= 80.0 = VeryStrong
  -- | n < 80.0  = Strong
  | n >= 65.0  = Strong
  | n >= 30.0  = Acceptable
  | n >= 15.0  = Weak
  | otherwise = VeryWeak

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
