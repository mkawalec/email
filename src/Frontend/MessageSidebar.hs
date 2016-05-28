module Frontend.MessageSidebar where

import Data.Typeable (Typeable)
import React.Flux

import Frontend.Stores.Messages
import Frontend.MessageDispatcher
import qualified Data.Text as T


messageSidebar :: ReactView ()
messageSidebar = defineControllerView "message sidebar" messageStore $ \st () ->
  div_ ["className" $= "messages"] $ mapM_ message_ $ messages st


message :: ReactView Message
message = defineView "message" $ \msg ->
  div_ [ "className" $= "message"
       , onClick $ \_ _ -> dispatchMessage . MarkRead . msgId $ msg
       ] $ do
    label_ . elemText . T.unpack $ T.concat [msgText msg, " ", T.pack . show $ msgRead msg]

message_ :: Message -> ReactElementM eventHandler ()
message_ !msg = viewWithKey message (fromIntegral $ msgId msg :: Int) msg mempty
