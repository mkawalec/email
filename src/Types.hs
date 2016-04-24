module Types where

import Data.Text
import GHC.Generics
import Data.Yaml
import Control.Applicative

data AccountConfig = AccountConfig {
  accountName :: Text,
  accountLogin :: Text,
  accountPassword :: Text
} deriving (Show, Eq, Ord, Generic)

data Config = Config {
  accounts :: [AccountConfig]
} deriving (Show, Eq, Ord, Generic)

instance FromJSON Config

instance FromJSON AccountConfig where
  parseJSON (Object v) = AccountConfig <$>
                         v .: "name" <*>
                         v .: "login" <*>
                         v .: "password"
  parseJSON _ = error "Wrong input format, needs an object"
