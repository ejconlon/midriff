module Midriff.Handle where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO)

newtype Handle a = Handle { unHandle :: a -> IO () }

runHandle :: MonadIO m => Handle a -> a -> m ()
runHandle (Handle h) = liftIO . h

newHandle :: MonadUnliftIO m => (a -> m ()) -> m (Handle a)
newHandle h = fmap (\r -> Handle (r . h)) askRunInIO
