module Midriff.Callback
  ( Callback
  , contramapIO
  , contramapM
  , forCallback
  , newCallback
  , newCallbackIO
  , runCallback
  ) where

import Control.DeepSeq (NFData)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO)
import Data.Foldable (for_)
import Data.Functor.Contravariant (Contravariant (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (sconcat)

newtype Callback a = Callback (a -> IO ())
  deriving newtype (NFData)

instance Semigroup (Callback a) where
  (Callback h1) <> (Callback h2) = Callback (\a -> h1 a *> h2 a)
  sconcat (cb :| cbs) =
    case cbs of
      [] -> cb
      _ -> Callback (\a -> for_ (cb:cbs) (\(Callback h) -> h a))

instance Monoid (Callback a) where
  mempty = Callback (const (pure ()))
  mappend = (<>)
  mconcat cbs =
    case cbs of
      [] -> mempty
      _ -> Callback (\a -> for_ cbs (\(Callback h) -> h a))

instance Contravariant Callback where
  contramap f (Callback h) = Callback (h . f)

contramapM :: MonadUnliftIO m => (a -> m b) -> Callback b -> m (Callback a)
contramapM f (Callback h) = fmap (\r -> Callback (r . f >=> h)) askRunInIO

contramapIO :: (a -> IO b) -> Callback b -> Callback a
contramapIO f (Callback h) = Callback (f >=> h)

forCallback :: Foldable f => (a -> f b) -> Callback b -> Callback a
forCallback f (Callback h) = Callback (\a -> for_ (f a) h)

runCallback :: MonadIO m => Callback a -> a -> m ()
runCallback (Callback h) = liftIO . h

newCallback :: MonadUnliftIO m => (a -> m ()) -> m (Callback a)
newCallback h = fmap (\r -> Callback (r . h)) askRunInIO

newCallbackIO :: (a -> IO ()) -> Callback a
newCallbackIO = Callback
