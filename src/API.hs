module API (api) where

import qualified API.Messages as M
import Servant
import Network.Wai (Application)
import Database.PostgreSQL.Simple (Connection)

type API = "api" :> M.MessagesAPI

globalAPI :: Proxy API
globalAPI = Proxy

server :: Connection -> Server API
server conn = M.server conn

api :: Connection -> Application
api conn = serve globalAPI $ server conn
