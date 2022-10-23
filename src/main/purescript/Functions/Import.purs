module Functions.Import where

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Function (($))
import Data.List (List(..))
import DataModel.AppState (AppError(..))
import DataModel.Card (Card(..))

decodeImport :: String -> Either AppError (List Card)
decodeImport s =
  let eitherJson = jsonParser s
  in case eitherJson of
      Left err -> Left $ ImportError err
      Right json -> Right Nil
