module DataModel.AppError where

import Data.Eq (class Eq)
import Data.PrettyShow (class PrettyShow, prettyShow)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import DataModel.Communication.ProtocolError (ProtocolError)
import DataModel.AppState (InvalidStateError)

type Element = String
type WrongVersion = String

data AppError = InvalidStateError InvalidStateError | ProtocolError ProtocolError | ImportError String | CannotInitState String | InvalidOperationError String | InvalidVersioning Element WrongVersion | UnhandledCondition String
instance showAppError :: Show AppError where
  show (InvalidStateError err)     = "Invalid state Error: "  <> show err
  show (ProtocolError err)         = "Protocol Error: " <> show err
  show (ImportError err)           = "Import Error: " <> err
  show (CannotInitState err)       = "Cannot init state: " <> err
  show (InvalidOperationError err) = "Invalid operation error: " <> err
  show (InvalidVersioning elem v)  = "Invalid Versioning [" <> v <> "] of " <> elem 
  show (UnhandledCondition err)    = "Unahandled Condition: " <> err

instance prettyShowAppError :: PrettyShow AppError where
  prettyShow (InvalidStateError err)     = prettyShow err
  prettyShow (ProtocolError err)         = prettyShow err
  prettyShow (ImportError err)           = "Your imported values are not in the right format! (" <> err <> ")" 
  prettyShow (CannotInitState _)         = "Cannot init state, please try to reload"
  prettyShow (InvalidOperationError _)   = "Invalid operation error, something was not programmed correctly."
  prettyShow (InvalidVersioning elem _)  = elem <> " is encoded in an unsupported version" 
  prettyShow (UnhandledCondition     _)  = ""

derive instance eqAppError :: Eq AppError
