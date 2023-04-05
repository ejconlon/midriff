module Midriff.TEvent
  ( TEvent
  , newTEvent
  , isSetTEvent
  , setTEvent
  )
where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import Control.DeepSeq (NFData (..))

{- | An variable that starts unset (as False) and can be set once (to True).
 This is useful for recording one-way transitions like /open/ to /closed/.
-}
newtype TEvent = TEvent {unTEvent :: TVar Bool}
  deriving stock (Eq)

instance NFData TEvent where
  rnf (TEvent v) = seq v ()

-- | Create a new 'TEvent'.
newTEvent :: STM TEvent
newTEvent = fmap TEvent (TVar.newTVar False)

{- | Return true if the event has been set.
 Once it has been set, it remains so.
-}
isSetTEvent :: TEvent -> STM Bool
isSetTEvent = TVar.readTVar . unTEvent

-- | Set the event. Idempotent; additional calls have no observable effect.
setTEvent :: TEvent -> STM ()
setTEvent = flip TVar.writeTVar True . unTEvent
