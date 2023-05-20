module Midriff.Gate
  ( Gate (..)
  , gateIsOpen
  , gateAwait
  , gateCheck
  )
where

import Control.Concurrent.STM (STM, check)
import Data.Foldable (toList)

data Gate = GateClosed | GateClosing | GateOpen
  deriving stock (Eq, Ord, Show, Enum, Bounded)

gateIsOpen :: Gate -> Bool
gateIsOpen = (== GateOpen)

gateAwait :: Gate -> STM ()
gateAwait = check . (== GateClosed)

gateCheck :: (Monad m, Foldable f) => f (m Gate) -> m Gate
gateCheck = go . toList
 where
  go = \case
    [] -> pure GateOpen
    m : ms -> do
      g <- m
      case g of
        GateClosed -> pure GateClosed
        GateClosing -> pure GateClosing
        GateOpen -> go ms
