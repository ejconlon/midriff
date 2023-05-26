module Midriff.Mvc where

import Control.Applicative (Alternative (..), liftA2)
import Control.Concurrent.STM (STM, atomically)
import Control.Foldl qualified as F
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Acquire (Acquire, withAcquire)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Functor.Contravariant.Divisible (Decidable (..), Divisible (..))
import Data.Profunctor (Profunctor (..))
import Data.Void (Void, absurd)
import Midriff.Coro (CoroIO, Done (..), consumeC, forC, runForeverC, yieldC, liftC, awaitC)
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

inputC :: Input i -> CoroIO Void i ()
inputC inp = go where
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
outputC out = go where
  go = awaitC >>= \o -> liftC (atomically (outputSend out o)) >>= \case { DoneYes -> pure () ; DoneNo -> go }

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

viewC :: View o -> CoroIO o () ()
viewC = consumeC . unView

runMVC :: Acquire (Input i, CoroIO i o a, View o) -> IO (Maybe a)
runMVC acq = withAcquire acq $ \(inp, coro, view) ->
  runForeverC (atomically (inputRecv inp)) (forC coro (viewC view))
