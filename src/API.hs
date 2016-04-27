module API (api) where

import qualified API.Messages as M
import Servant
import Network.Wai (Application)
import Database.PostgreSQL.Simple (Connection)

api :: Connection -> Application
api conn = serve M.messagesAPI $ M.server conn
