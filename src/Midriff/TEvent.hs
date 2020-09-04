module Midriff.TEvent
  ( TEvent
  , newTEvent
  , isSetTEvent
  , setTEvent
  ) where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar

newtype TEvent = TEvent { unTEvent :: TVar Bool }

newTEvent :: STM TEvent
newTEvent = fmap TEvent (TVar.newTVar False)

isSetTEvent :: TEvent -> STM Bool
isSetTEvent = TVar.readTVar . unTEvent

setTEvent :: TEvent -> STM ()
setTEvent = flip TVar.writeTVar True . unTEvent
