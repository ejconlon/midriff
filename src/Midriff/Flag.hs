module Midriff.Flag
  ( Done (..)
  , Flag
  , flagNew
  , flagNewIO
  , flagSet
  , flagRead
  , flagCheck
  )
where

import Control.Concurrent.STM (STM, check)
import Control.Concurrent.STM.TVar (TVar, newTVar, newTVarIO, readTVar, writeTVar)
import Control.Monad ((>=>))

data Done = DoneNo | DoneYes
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Semigroup Done where
  DoneYes <> _ = DoneYes
  _ <> d = d

instance Monoid Done where
  mempty = DoneNo
  mappend = (<>)

newtype Flag = Flag {unFlag :: TVar Done}
  deriving newtype (Eq)

flagNew :: STM Flag
flagNew = fmap Flag (newTVar DoneNo)

flagNewIO :: IO Flag
flagNewIO = fmap Flag (newTVarIO DoneNo)

flagSet :: Flag -> STM ()
flagSet = flip writeTVar DoneYes . unFlag

flagRead :: Flag -> STM Done
flagRead = readTVar . unFlag

flagCheck :: Flag -> STM ()
flagCheck = flagRead >=> check . (== DoneYes)
