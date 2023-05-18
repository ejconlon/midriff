module Midriff.Gate
  ( Gate (..)
  , gateCheck
  )
where

import Data.Foldable (toList)

data Gate = GateClosed | GateOpen
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Semigroup Gate where
  GateClosed <> _ = GateClosed
  GateOpen <> a = a

instance Monoid Gate where
  mempty = GateOpen
  mappend = (<>)

gateCheck :: (Monad m, Foldable f) => f (m Gate) -> m Gate
gateCheck = go . toList where
  go = \case
    [] -> pure mempty
    m:ms -> do
      g <- m
      case g of
        GateClosed -> pure GateClosed
        GateOpen -> go ms

