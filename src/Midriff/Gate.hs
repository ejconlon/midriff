module Midriff.Gate
  ( Gate (..)
  , gateCheck
  )
where

import Data.Foldable (toList)

data Gate = GateClosed | GateClosing | GateOpen
  deriving stock (Eq, Ord, Show, Enum, Bounded)

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
