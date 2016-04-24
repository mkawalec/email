module Config (readConfig) where

import Types
import Data.Yaml

readConfig :: IO (Either ParseException Config)
readConfig = decodeFileEither "config.yml"
