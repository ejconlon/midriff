module Midriff.Lock
  ( Lock
  , lockNew
  , lockPeek
  , lockRead
  , lockState
  , lockEscalate
  )
where

import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Exception (bracket)

-- | A reader/writer lock
data Lock a = Lock
  { lockCount :: !(TVar Int)
  , lockWaiter :: !(TVar Bool)
  , lockVar :: !(TVar a)
  }

lockNew :: a -> IO (Lock a)
lockNew a = Lock <$> newTVarIO 0 <*> newTVarIO False <*> newTVarIO a

lockPeek :: Lock a -> STM a
lockPeek (Lock cnt _ var) = do
  c <- readTVar cnt
  if c < 0
    then retry
    else readTVar var

lockRead :: Lock a -> (a -> IO b) -> IO b
lockRead (Lock cnt _ var) = bracket start end
 where
  start = atomically $ do
    c <- readTVar cnt
    if c < 0
      then retry
      else modifyTVar' cnt (+ 1) *> readTVar var
  end _ = atomically (modifyTVar' cnt (+ (-1)))

lockState :: Lock a -> (a -> IO (b, a)) -> IO b
lockState (Lock cnt _ var) edit = bracket start end middle
 where
  start = atomically $ do
    c <- readTVar cnt
    if c > 0
      then retry
      else modifyTVar' cnt (+ (-1)) *> readTVar var
  middle a = do
    (b, a') <- edit a
    atomically (writeTVar var a')
    pure b
  end _ = atomically (modifyTVar' cnt (+ 1))

lockEscalate :: Lock a -> (a -> IO (Either z b)) -> (z -> IO (b, a)) -> IO b
lockEscalate (Lock cnt wtr var) check edit = bracket start end middle
 where
  start = atomically $ do
    w <- readTVar wtr
    if w
      then retry
      else do
        c <- readTVar cnt
        if c < 0
          then retry
          else writeTVar wtr True *> modifyTVar' cnt (+ 1) *> readTVar var
  middle a = do
    e <- check a
    case e of
      Right b -> pure b
      Left z -> do
        atomically $ do
          c <- readTVar cnt
          if c > 1
            then retry
            else writeTVar wtr False *> writeTVar cnt (-1)
        (b, a') <- edit z
        atomically (writeTVar var a')
        pure b
  end _ = atomically (writeTVar wtr False *> modifyTVar' cnt (\c -> if c < 0 then 0 else c - 1))
