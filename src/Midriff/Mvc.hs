module Midriff.Mvc where

import Control.Applicative (Alternative (..), liftA2)
import Control.Concurrent.STM (STM, atomically)
import Control.Foldl qualified as F
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Acquire (Acquire, withAcquire)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Functor.Contravariant.Divisible (Decidable (..), Divisible (..))
import Data.Profunctor (Profunctor (..))
import Data.Void (absurd)
import Midriff.Coro (CoroIO, Done (..), forC, runForeverC, scanC)
import Optics (A_Traversal, An_AffineFold, Is, Optic', preview, traverseOf)

newtype Input a = Input {recv :: STM (Maybe a)}
  deriving (Functor, Applicative, Monad, Alternative) via (MaybeT STM)

instance Semigroup (Input a) where
  (<>) = (<|>)

instance Monoid (Input a) where
  mempty = empty
  mappend = (<>)

keeps :: Is k An_AffineFold => Optic' k is a b -> Input a -> Input b
keeps optic x = Input (fmap (>>= preview optic) (recv x))

newtype Output a = Output {send :: a -> STM Done}

instance Semigroup (Output a) where
  x <> y = Output (\a -> liftA2 (<>) (send x a) (send y a))

instance Monoid (Output a) where
  mempty = Output (const (pure DoneNo))
  mappend = (<>)

instance Contravariant Output where
  contramap f x = Output (send x . f)

instance Divisible Output where
  divide f x y = Output (\a -> let (b, c) = f a in liftA2 (<>) (send x b) (send y c))
  conquer = mempty

instance Decidable Output where
  lose f = Output (absurd . f)
  choose f x y = Output $ \a -> case f a of
    Left b -> send x b
    Right c -> send y c

newtype View a = View {unView :: F.FoldM IO a ()}

sink :: (a -> IO ()) -> View a
sink = View . F.sink

instance Contravariant View where
  contramap f (View x) = View (lmap f x)

instance Semigroup (View a) where
  View x <> View y = View (x <> y)

instance Monoid (View a) where
  mempty = View mempty
  mappend = (<>)

handles :: Is k A_Traversal => Optic' k is a b -> View b -> View a
handles optic (View x) = View (F.handlesM (traverseOf optic) x)

viewC :: View o -> CoroIO o () ()
viewC = scanC . unView

runMVC :: Acquire (Input i, CoroIO i o a, View o) -> IO (Maybe a)
runMVC acq = withAcquire acq $ \(inp, coro, view) ->
  runForeverC (atomically (recv inp)) (forC coro (viewC view))
