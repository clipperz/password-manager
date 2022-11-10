module Functions.Import where

import Control.Applicative (pure)
import Control.Bind (bind, (=<<), (>>=))
import Control.Monad.Except (except, runExcept)
import Control.Semigroupoid ((<<<))
import Data.Argonaut.Core (Json, caseJsonArray, caseJsonObject, toBoolean, toObject, toString)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (head, tail, elem, filter)
import Data.Array.NonEmpty as ANE
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.Boolean (otherwise)
import Data.Either (Either(..), note)
import Data.Eq (eq, (==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.Maybe (fromMaybe, Maybe)
import Data.String (split)
import Data.String.Common (replace)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Regex (Regex, regex, match)
import Data.String.Regex.Flags (global)
import Data.Traversable (sequence)
import DataModel.AppState (AppError(..))
import DataModel.Card (Card(..), CardValues(..), CardField(..))
import Effect (Effect)
import Foreign.Object (Object, lookup, values)
import Functions.Time (getCurrentTimestamp)

foreign import decodeHTML :: String -> String

parseHTMLImport :: String -> Either AppError String
parseHTMLImport html = 
  let eitherRegex = regex "<textarea>.+<\\/textarea>" global
  in (lmap (\_ -> ImportError "The regex written by the developers is not correct") eitherRegex) >>= useRegex

  where 
    useRegex :: Regex -> Either AppError String
    useRegex r = useMaybeMatches $ match r html

    useMaybeMatches :: Maybe (NonEmptyArray (Maybe String)) -> Either AppError String
    useMaybeMatches matches = fromMaybe (Left $ ImportError "Invalid file: no card data found") (useMatches <$> matches)

    useMatches :: NonEmptyArray (Maybe String) -> Either AppError String
    useMatches matches
      | ANE.length matches == 1 = note (ImportError "Invalid file: no card data found") (useMatch <$> (ANE.head matches))
      | otherwise               = Left $ ImportError "Invalid file: too many data fields found"

    useMatch :: String -> String
    useMatch = (replace (Pattern "<textarea>") (Replacement "")) <<< (replace (Pattern "</textarea>") (Replacement ""))


decodeImport :: String -> Effect (Either AppError (Array Card))
decodeImport s = do
  currentTime <- getCurrentTimestamp
  let eitherJson = jsonParser s
  pure $ case eitherJson of
      Left err -> Left $ ImportError err
      Right json -> caseJsonArray (Left $ ImportError "Cannot convert json to json array") (\a -> sequence $ (decodeCard currentTime) <$> a) json

decodeCard :: Number -> Json -> Either AppError Card
decodeCard timestamp json = 
  let epsilonTryResult = decodeJson json -- assumes version currently in use
  in case epsilonTryResult of
    Right c -> lmap (\_ -> ImportError "Cannot convert json to array of card") epsilonTryResult
    Left err -> 
      let deltaTryResult = caseJsonObject (Left $ ImportError "Cannot conver json to json object") (decodeDeltaCardObject timestamp) json
      in case deltaTryResult of
        Right c -> deltaTryResult
        Left err -> Left $ ImportError "Import file is formatted neither by delta nor by epsilon version"

decodeDeltaCardObject :: Number -> Object Json -> Either AppError Card
decodeDeltaCardObject timestamp obj = runExcept $ do
  titleAndTags :: Array String <- split (Pattern " ") <$> (except $ note (ImportError "Cannot find card label") $ (toString =<< lookup "label" obj))
  let title    = fromMaybe "" $ head titleAndTags
  let tags     = filter (\s -> not $ eq "ARCH" s) $ fromMaybe [] $ tail titleAndTags
  let archived = elem "ARCH" titleAndTags
  fields :: Array CardField <- do
    a <- except $ note (ImportError "Cannot find card fields") $ (values <$> (toObject =<< (lookup "fields") =<< toObject =<< lookup "currentVersion" obj))
    except $ sequence (decodeCardField <$> a)
  notes  <- except $ note (ImportError "Cannot find card notes") $ (toString =<< (lookup "notes") =<< toObject =<< lookup "data" obj)
  pure $ Card { timestamp: timestamp
                  , archived: archived
                  , content: CardValues { title: title
                                          , tags: tags
                                          , fields: fields
                                          , notes: notes
                                          }
                  }

decodeCardField :: Json -> Either AppError CardField
decodeCardField json = runExcept $ do
  obj    <- except $ note (ImportError "Cannot convert json to json object") $ (toObject json)
  label  <- except $ note (ImportError "Cannot find field label")  $ (toString  =<< lookup "label"  obj)
  value  <- except $ note (ImportError "Cannot find field value")  $ (toString  =<< lookup "value"  obj)
  let hidden = fromMaybe false $ (toBoolean =<< lookup "hidden" obj)
  pure $ CardField {name: label, value: value, locked: hidden}
