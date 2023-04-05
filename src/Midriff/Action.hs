module Midriff.Action
  ( Action
  , runAction
  , newAction
  , newActionIO
  )
where

import Control.DeepSeq (NFData (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO)

newtype Action = Action (IO ())

instance NFData Action where
  rnf (Action i) = seq i ()

runAction :: MonadIO m => Action -> m ()
runAction (Action i) = liftIO i

newAction :: MonadUnliftIO m => m () -> m Action
newAction h = fmap (\r -> Action (r h)) askRunInIO

newActionIO :: IO () -> Action
newActionIO = Action
