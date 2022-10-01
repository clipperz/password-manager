module Functions.Password where

import Bytes (foldMapBytesToString)
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Ord ((<=), (<))
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Show (class Show, show)
import Data.String.CodePoints (length, take, drop)
import Effect.Aff (Aff)
import Effect.Fortuna (randomBytes)

data PasswordStrength = VeryWeak | Weak | Acceptable | Strong | VeryStrong
instance showPasswordStrengh :: Show PasswordStrength where
  show VeryWeak   = "very weak"
  show Weak       = "weak"
  show Acceptable = "acceptable"
  show Strong     = "strong"
  show VeryStrong = "very strong"

type PasswordStrengthFunction = String -> PasswordStrength

standardPasswordStrengthFunction :: PasswordStrengthFunction
standardPasswordStrengthFunction s = if (length s) <= 4 then Weak else Strong

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