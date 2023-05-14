module DataModel.Credentials where

type Credentials =  { username :: String
                    , password :: String
                    }

emptyCredentials :: Credentials
emptyCredentials = { username: "", password: "" }
