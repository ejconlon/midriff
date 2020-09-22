module Midriff.Handle
  ( Handle
  , newHandle
  , newHandleIO
  , runHandle
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO)
import Data.Functor.Contravariant (Contravariant (..))

newtype Handle a = Handle { unHandle :: a -> IO () }

instance Semigroup (Handle a) where
  (Handle h1) <> (Handle h2) = Handle (h1 *> h2)

instance Monoid (Handle a) where
  mempty = Handle (const (pure ()))
  mappend = (<>)

instance Contravariant Handle where
  contramap f (Handle h) = Handle (h . f)

runHandle :: MonadIO m => Handle a -> a -> m ()
runHandle (Handle h) = liftIO . h

newHandle :: MonadUnliftIO m => (a -> m ()) -> m (Handle a)
newHandle h = fmap (\r -> Handle (r . h)) askRunInIO

newHandleIO :: (a -> IO ()) -> Handle a
newHandleIO = Handle
