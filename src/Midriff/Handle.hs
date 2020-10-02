module Midriff.Handle
  ( Handle
  , contramapIO
  , contramapM
  , forHandle
  , newHandle
  , newHandleIO
  , runHandle
  ) where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO)
import Data.Foldable (for_)
import Data.Functor.Contravariant (Contravariant (..))

newtype Handle a = Handle { unHandle :: a -> IO () }

instance Semigroup (Handle a) where
  (Handle h1) <> (Handle h2) = Handle (\a -> h1 a *> h2 a)

instance Monoid (Handle a) where
  mempty = Handle (const (pure ()))
  mappend = (<>)

instance Contravariant Handle where
  contramap f (Handle h) = Handle (h . f)

contramapM :: MonadUnliftIO m => (a -> m b) -> Handle b -> m (Handle a)
contramapM f (Handle h) = fmap (\r -> Handle (r . f >=> h)) askRunInIO

contramapIO :: (a -> IO b) -> Handle b -> Handle a
contramapIO f (Handle h) = Handle (f >=> h)

forHandle :: Foldable f => (a -> f b) -> Handle b -> Handle a
forHandle f (Handle h) = Handle (\a -> for_ (f a) h)

runHandle :: MonadIO m => Handle a -> a -> m ()
runHandle (Handle h) = liftIO . h

newHandle :: MonadUnliftIO m => (a -> m ()) -> m (Handle a)
newHandle h = fmap (\r -> Handle (r . h)) askRunInIO

newHandleIO :: (a -> IO ()) -> Handle a
newHandleIO = Handle
