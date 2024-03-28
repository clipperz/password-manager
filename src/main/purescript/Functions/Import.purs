module Functions.Import where

import Prelude

import Control.Monad.Except (ExceptT, except, runExcept, throwError, withExceptT)
import Data.Argonaut.Core (Json, caseJsonArray, caseJsonObject, stringify, toBoolean, toObject, toString)
import Data.Argonaut.Decode (parseJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (elem, filter, head, tail)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (decode, encode)
import Data.Either (Either, hush, note)
import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Set as Set
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (sequence)
import DataModel.AppError (AppError(..))
import DataModel.CardVersions.Card (Card(..), CardField(..), CardValues(..), CardVersion(..), cardVersionCodec, toCard)
import DataModel.CardVersions.CurrentCardVersions (currentCardCodecVersion)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Foreign.Object (Object, lookup, values)
import Functions.Time (getCurrentTimestamp)
import Web.File.Blob (Blob)
import Web.File.File (File)

foreign import _readFile :: File -> EffectFnAff String

readFile :: Maybe File -> ExceptT AppError Aff String
readFile maybeFile = do
  file   <- except $ note (ImportError "File not found") maybeFile
  liftAff $ fromEffectFnAff (_readFile file)

foreign import decodeHTML :: String -> String

foreign import createFile :: Blob -> File

data ImportVersion = Delta | Epsilon CardVersion

instance showImportVersion :: Show ImportVersion where
  show  Delta      = "Delta"
  show (Epsilon v) = "Epsilon: " <> (stringify $ encode cardVersionCodec v)

parseImport :: String -> ExceptT AppError Aff (Array Card)
parseImport html = do
  regex <- except $ regex "<textarea( class=\'({\"tag\":\".+\"})\')?>(\\[[\\s\\S]+\\])<\\/textarea>" noFlags # lmap (\_ -> ImportError "The regex written by the developers is not correct")
  case (match regex (decodeHTML html) <#> fromFoldable) of
    Just (_ : _ : maybeVersion : (Just cards) : Nil) -> do
      version <- pure $ fromMaybe Delta (Epsilon <$> (
                                               (decode cardVersionCodec >>> hush)
                                           =<< (parseJson               >>> hush)
                                           =<<  maybeVersion
                                        ))                             
      decodeImport version cards
    _                                                -> 
      throwError $ ImportError ("Invalid file: unable to decode data [" <> decodeHTML html <> "]")

decodeImport :: ImportVersion -> String -> ExceptT AppError Aff (Array Card)
decodeImport version cards = 
  (\json -> caseJsonArray (throwError $ ImportError "Cannot convert json to json array") (\array -> sequence $ array <#> 
    (\card -> case version of
      Delta                 -> caseJsonObject (throwError $ ImportError "Cannot conver json to json object") decodeDeltaCardObject card
      Epsilon CardVersion_1 -> (except $ toCard <$> decode currentCardCodecVersion card) # withExceptT (ProtocolError <<< DecodeError <<< show)
    )
  ) json) =<< (except $ lmap (ProtocolError <<< DecodeError <<< show) (jsonParser cards))

decodeDeltaCardObject :: Object Json -> ExceptT AppError Aff Card
decodeDeltaCardObject obj = do
  timestamp <- liftEffect getCurrentTimestamp
  titleAndTags :: Array String <- split (Pattern " ") <$> (except $ note (ImportError "Cannot find card label") $ (toString =<< lookup "label" obj))
  let title    = fromMaybe "" $ head titleAndTags
  let tags     = filter (\s -> not $ eq "ARCH" s) $ fromMaybe [] $ tail titleAndTags
  let archived = elem "ARCH" titleAndTags
  fields :: Array CardField <- do
    a <- except $ note (ImportError "Cannot find card fields") $ (values <$> (toObject =<< (lookup "fields") =<< toObject =<< lookup "currentVersion" obj))
    except $ sequence (decodeCardField <$> a)
  notes  <- except $ note (ImportError "Cannot find card notes") $ (toString =<< (lookup "notes") =<< toObject =<< lookup "data" obj)
  pure $ Card { timestamp: timestamp
              , secrets: []
              , archived: archived
              , content: CardValues { title: title
                                    , tags: Set.fromFoldable tags
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
  pure $ CardField {name: label, value: value, locked: hidden, settings: Nothing}
