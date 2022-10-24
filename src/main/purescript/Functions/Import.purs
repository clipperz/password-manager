module Functions.Import where

import Control.Applicative (pure)
import Control.Bind (bind, (=<<))
import Control.Monad.Except (except, runExcept)
import Data.Argonaut.Core (Json, caseJsonArray, caseJsonObject, toBoolean, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (head, tail, elem, filter)
import Data.Either (Either(..), note)
import Data.Eq (eq)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.Maybe (fromMaybe)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (sequence)
import DataModel.AppState (AppError(..))
import DataModel.Card (Card(..), CardValues(..), CardField(..))
import Effect (Effect)
import Foreign.Object (Object, lookup, values)
import Functions.Time (getCurrentTimestamp)

decodeImport :: String -> Effect (Either AppError (Array Card))
decodeImport s = do
  currentTime <- getCurrentTimestamp
  let eitherJson = jsonParser s
  pure $ case eitherJson of
      Left err -> Left $ ImportError err
      Right json -> caseJsonArray (Left $ ImportError "Cannot convert json to json array") (\a -> sequence $ (decodeCard currentTime) <$> a) json

-- caseJsonArray :: forall a. a -> (Array Json -> a) -> Json -> a
-- a :: Effect (Either Error (Array Card))
-- Array Json -> Effect (Either Error (Array Card))
-- arr
-- t (m a) -> m (t a)
-- t (m (m a))
-- Array Effect Either -> Effect Either Error Array

decodeCard :: Int -> Json -> Either AppError Card
decodeCard timestamp = caseJsonObject (Left $ ImportError "Cannot conver json to json object") decodeCardObject

  where
    decodeCardObject :: Object Json -> Either AppError Card
    decodeCardObject obj = runExcept $ do
      titleAndTags :: Array String <- split (Pattern " ") <$> (except $ note (ImportError "Cannot find card label") $ (toString =<< lookup "label" obj))
      let title    = fromMaybe "" $ head titleAndTags
      let tags     = filter (\s -> not $ eq "ARCH" s) $ fromMaybe [] $ tail titleAndTags
      let archived = elem "ARCH" titleAndTags
      fields :: Array CardField <- do
        a <- except $ note (ImportError "Cannot find card fields") $ (values <$> (toObject =<< (lookup "fields") =<< toObject =<< lookup "currentVersion" obj))
        except $ sequence (decodeCardField <$> a)
      notes  <- except $ note (ImportError "Cannot find card notes") $ (toString =<< (lookup "notes") =<< toObject =<< lookup "data" obj)
      pure $ Card_v1 { timestamp: timestamp
                     , archived: archived
                     , content: CardValues_v1 { title: title
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
      pure $ CardField_v1 {name: label, value: value, locked: hidden}