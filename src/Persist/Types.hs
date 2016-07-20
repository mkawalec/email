module Persist.Types where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.UUID.Types (UUID)

type EmailIdMap = M.Map T.Text UUID
