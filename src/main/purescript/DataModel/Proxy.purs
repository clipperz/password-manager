module DataModel.Proxy where


import Data.Semigroup ((<>))
import Data.Show (class Show)

data Proxy = OnlineProxy String | OfflineProxy

instance showProxy :: Show Proxy where
  show (OnlineProxy s) = "Online Proxy: " <> s
  show (OfflineProxy)  = "Offline Proxy"
