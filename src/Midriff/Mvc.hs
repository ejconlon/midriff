-- | Based on https://hackage.haskell.org/package/mvc
module Midriff.Mvc where

import Control.Applicative (Alternative (..), liftA2)
import Control.Concurrent.STM (STM, atomically)
import Control.Foldl qualified as F
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Acquire (Acquire, withAcquire)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Functor.Contravariant.Divisible (Decidable (..), Divisible (..))
import Data.Profunctor (Profunctor (..))
import Data.Void (Void, absurd)
import Midriff.Coro (CoroIO, CoroT (..), ListIO, ListT (..), awaitC, foldC, forC, liftC, runForeverC, yieldC)
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

inputL :: Input i -> ListIO i
inputL inp = ListT go
 where
  go = liftC (atomically (inputRecv inp)) >>= maybe (pure ()) (\i -> yieldC i >> go)

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

outputC :: Output o -> CoroIO o Void ()
outputC out = go
 where
  go = awaitC >>= \o -> liftC (atomically (outputSend out o)) >>= \case DoneYes -> pure (); DoneNo -> go

newtype View a = View {unView :: F.FoldM IO a ()}

viewSink :: (a -> IO ()) -> View a
viewSink = View . F.sink

instance Contravariant View where
  contramap f (View x) = View (lmap f x)

instance Semigroup (View a) where
  View x <> View y = View (x <> y)

instance Monoid (View a) where
  mempty = View mempty
  mappend = (<>)

-- | Can use Traversal, Lens, Prism, or Iso to enumerate things to handle
viewHandles :: Is k A_Traversal => Optic' k is a b -> View b -> View a
viewHandles optic (View x) = View (F.handlesM (traverseOf optic) x)

viewC :: View o -> CoroIO o p ()
viewC = foldC . unView

runMVC :: Acquire (Input i, CoroIO i o a, View o) -> IO (Maybe a)
runMVC acq = withAcquire acq $ \(inp, coro, view) ->
  runForeverC id (atomically (inputRecv inp)) (forC coro (viewC view))

data Term a = TermAwait | TermYield | TermEnd !a
  deriving stock (Eq, Ord, Show)

runTermC :: Monad m => (forall x. n x -> m x) -> m (Maybe i) -> (o -> m Done) -> CoroT i o n a -> m (Term a)
runTermC nat inp out (CoroT c) = c req rep lif end
 where
  req k = inp >>= maybe (pure TermAwait) k
  rep o r = out o >>= \case DoneYes -> pure TermYield; DoneNo -> r
  lif mc = join (nat mc)
  end = pure . TermEnd

consumeL :: Flag -> STM a -> ListIO a
consumeL flag act = ListT go
 where
  go =
    liftIO (atomically (fmap Just act <|> Nothing <$ flagCheck flag))
      >>= maybe (pure ()) ((>> go) . yieldC)

tryConsumeL :: Flag -> STM (Maybe a) -> ListIO (Maybe a)
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
