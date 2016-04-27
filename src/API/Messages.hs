module API.Messages (messagesAPI, MessagesAPI, server) where

import qualified Data.Text as T
import Types
import qualified Network.Mail.Parse.Types as MPT
import Servant
import Data.Aeson (ToJSON, decode)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Control.Applicative

type MessagesAPI = "messages" :> Get '[JSON] [Message]

data Message = Message {
  uid :: Int,
  emailMessage :: Maybe MPT.EmailMessage
} deriving (Eq, Show, Generic)

messagesAPI :: Proxy MessagesAPI
messagesAPI = Proxy

instance ToJSON Message
instance FromRow Message where
  fromRow = Message <$> field <*> (decode <$> field)

server :: Connection -> Server MessagesAPI
server conn = do
  liftIO $ query_ conn "SELECT uid, message from message"
