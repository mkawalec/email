module Frontend.Stores.Messages where

import React.Flux
import Control.DeepSeq
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

data Message = Message {
    msgText :: Text
  , msgId :: Integer
  , msgFrom :: Text
  , msgRead :: Bool
} deriving (Show, Typeable)

newtype MessageState = MessageState {
  messages :: [Message]
} deriving (Show, Typeable)

data MessageAction = MarkRead Integer
                   | Delete Integer
  deriving (Show, Typeable, Generic, NFData)

instance StoreData MessageState where
  type StoreAction MessageState = MessageAction
  transform action (MessageState messages) = do
    return . MessageState $ case action of
      (MarkRead i) -> let f msg@(Message _ msgId _ _) | msgId == i = msg { msgRead = True }
                          f msg = msg
                        in map f messages
      (Delete i) -> filter ((/=i) . msgId) messages

messageStore :: ReactStore MessageState
messageStore = mkStore $ MessageState
  [ Message "I am a first message" 0 "joe@joe.com" False
  , Message "I am the second message" 12 "mike@joe.com" False
  , Message "I am the third message" 36 "mike@mike.com" False
  , Message "I am the thourth message" 2 "sth@sth.com" True
  ]
