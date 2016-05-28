module Frontend.MessageDispatcher where

import React.Flux
import Frontend.Stores.Messages

dispatchMessage :: MessageAction -> [SomeStoreAction]
dispatchMessage a = [SomeStoreAction messageStore a]
