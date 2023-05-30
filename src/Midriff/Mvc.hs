-- | Based on https://hackage.haskell.org/package/mvc
module Midriff.Mvc where

import Control.Applicative (Alternative (..), liftA2)
import Control.Concurrent.STM (STM, atomically)
import Control.Foldl qualified as F
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Acquire (Acquire, withAcquire)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Functor.Contravariant.Divisible (Decidable (..), Divisible (..))
import Data.Profunctor (Profunctor (..))
import Data.Void (Void, absurd)
import Midriff.Coro (CoroT (..), ListT (..), awaitC, foldC, liftC, runC, yieldC, (.|))
import Midriff.Flag (Done (..), Flag, flagCheck, flagRead)
import Optics (A_Traversal, An_AffineFold, Is, Optic', preview, traverseOf)

newtype Input i = Input {inputRecv :: STM (Maybe i)}
  deriving (Functor, Applicative, Monad, Alternative) via (MaybeT STM)

instance Semigroup (Input i) where
  (<>) = (<|>)

instance Monoid (Input i) where
  mempty = empty
  mappend = (<>)

-- | Can use Lens, Prism, or Iso to filter what input to keep
inputKeeps :: Is k An_AffineFold => Optic' k is i j -> Input i -> Input j
inputKeeps optic x = Input (fmap (>>= preview optic) (inputRecv x))

inputL :: MonadIO m => Input i -> ListT m i
inputL inp = ListT go
 where
  go = liftC (liftIO (atomically (inputRecv inp))) >>= maybe (pure ()) (\i -> yieldC i >> go)

newtype Output a = Output {outputSend :: a -> STM Done}

instance Semigroup (Output a) where
  x <> y = Output (\a -> liftA2 (<>) (outputSend x a) (outputSend y a))

instance Monoid (Output a) where
  mempty = Output (const (pure DoneNo))
  mappend = (<>)

instance Contravariant Output where
  contramap f x = Output (outputSend x . f)

instance Divisible Output where
  divide f x y = Output (\a -> let (b, c) = f a in liftA2 (<>) (outputSend x b) (outputSend y c))
  conquer = mempty

instance Decidable Output where
  lose f = Output (absurd . f)
  choose f x y = Output $ \a -> case f a of
    Left b -> outputSend x b
    Right c -> outputSend y c

outputC :: MonadIO m => Output o -> CoroT o Void m Done
outputC out = go
 where
  go = do
    mo <- awaitC
    case mo of
      Nothing -> pure DoneNo
      Just o -> do
        done <- liftC (liftIO (atomically (outputSend out o)))
        case done of
          DoneYes -> pure DoneYes
          DoneNo -> go

newtype View m a = View {unView :: F.FoldM m a ()}

viewSink :: Monad m => (a -> m ()) -> View m a
viewSink = View . F.sink

instance Functor m => Contravariant (View m) where
  contramap f (View x) = View (lmap f x)

instance Monad m => Semigroup (View m a) where
  View x <> View y = View (x <> y)

instance Monad m => Monoid (View m a) where
  mempty = View mempty
  mappend = (<>)

-- | Can use Traversal, Lens, Prism, or Iso to enumerate things to handle
viewHandles :: (Monad m, Is k A_Traversal) => Optic' k is a b -> View m b -> View m a
viewHandles optic (View x) = View (F.handlesM (traverseOf optic) x)

viewC :: MonadIO m => View m o -> CoroT o Void m ()
viewC = foldC . unView

runApp :: MonadUnliftIO m => Acquire (Input i, CoroT i o m (), View m o) -> m ()
runApp acq = withAcquire acq $ \(inp, coro, view) ->
  runC (enumerateC (inputL inp) .| coro .| viewC view)

consumeL :: MonadIO m => Flag -> STM a -> ListT m a
consumeL flag act = ListT go
 where
  go =
    liftIO (atomically (fmap Just act <|> Nothing <$ flagCheck flag))
      >>= maybe (pure ()) ((>> go) . yieldC)

tryConsumeL :: MonadIO m => Flag -> STM (Maybe a) -> ListT m (Maybe a)
tryConsumeL flag act = ListT go
 where
  go = do
    mma <- liftIO $ atomically $ do
      ma <- act
      case ma of
        Just _ -> pure (Just ma)
        Nothing -> do
          d <- flagRead flag
          case d of
            DoneYes -> pure Nothing
            DoneNo -> pure (Just ma)
    maybe (pure ()) ((>> go) . yieldC) mma
