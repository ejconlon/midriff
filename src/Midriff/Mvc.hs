module Midriff.Mvc where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad (ap)
import Data.Acquire (Acquire, withAcquire)
import Control.Concurrent.STM (STM, atomically)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Functor.Contravariant.Divisible (Divisible (..), Decidable (..))
import Data.Void (absurd)
import Optics (Is, Optic', preview, An_AffineFold, A_Traversal, traverseOf)
import qualified Control.Foldl as F
import Data.Profunctor (Profunctor (..))
import Control.Monad.IO.Class (MonadIO (..))

newtype Input a = Input { recv :: STM (Maybe a) }
  deriving (Functor, Applicative, Monad, Alternative) via (MaybeT STM)

instance Semigroup (Input a) where
  (<>) = (<|>)

instance Monoid (Input a) where
  mempty = empty
  mappend = (<>)

keeps :: Is k An_AffineFold => Optic' k is a b -> Input a -> Input b
keeps optic x = Input (fmap (>>= preview optic) (recv x))

data Done = DoneNo | DoneYes
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Semigroup Done where
  DoneYes <> _ = DoneYes
  _ <> d = d

instance Monoid Done where
  mempty = DoneNo
  mappend = (<>)

newtype Output a = Output { send :: a -> STM Done }

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

newtype View a = View { unView :: F.FoldM IO a () }

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

data Coro m a b
    = CoroAwait (a -> Coro m a b)
    | CoroYield b (Coro m a b)
    | CoroEffect (m (Coro m a b))
    | CoroEnd

instance Functor m => Functor (Coro m a) where
  fmap f = go where
    go = \case
      CoroAwait k -> CoroAwait (go . k)
      CoroYield b x -> CoroYield (f b) (go x)
      CoroEffect mx -> CoroEffect (fmap go mx)
      CoroEnd -> CoroEnd

instance Functor m => Applicative (Coro m a) where
  pure a = CoroYield a CoroEnd
  (<*>) = ap

instance Functor m => Monad (Coro m a) where
  return = pure
  x0 >>= f = go x0 where
    go = \case
      CoroAwait k -> CoroAwait (go . k)
      CoroYield b x1 -> f b >>= \c -> CoroYield c (go x1)
      CoroEffect mx1 -> CoroEffect (fmap go mx1)
      CoroEnd -> CoroEnd

instance MonadIO m => MonadIO (Coro m a) where
  liftIO = coroLift . liftIO

instance Functor m => Profunctor (Coro m) where
  dimap g f = go where
    go = \case
      CoroAwait k -> CoroAwait (go . k . g)
      CoroYield b x -> CoroYield (f b) (go x)
      CoroEffect mx -> CoroEffect (fmap go mx)
      CoroEnd -> CoroEnd
  lmap g = go where
    go = \case
      CoroAwait k -> CoroAwait (go . k . g)
      CoroYield b x -> CoroYield b (go x)
      CoroEffect mx -> CoroEffect (fmap go mx)
      CoroEnd -> CoroEnd
  rmap = fmap

coroLift :: Functor m => m b -> Coro m a b
coroLift act = CoroEffect (fmap pure act)

coroArr :: (a -> b) -> Coro m a b
coroArr f = CoroAwait (\a -> CoroYield (f a) CoroEnd)

coroCat :: Coro m a a
coroCat = CoroAwait (`CoroYield` CoroEnd)

-- Pull-based composition.
coroCompose :: Functor m => Coro m a b -> Coro m b c -> Coro m a c
coroCompose x0 y0 = go y0 where
  go = \case
    CoroAwait j -> case x0 of
      CoroAwait k -> CoroAwait ((`coroCompose` y0) . k)
      CoroYield a x1 -> coroCompose x1 (j a)
      CoroEffect mx1 -> CoroEffect (fmap (`coroCompose` y0) mx1)
      CoroEnd -> CoroEnd
    CoroYield c y1 -> CoroYield c (go y1)
    CoroEffect my1 -> CoroEffect (fmap go my1)
    CoroEnd -> CoroEnd

coroFold :: Monad m => F.FoldM m a b -> Coro m a b
coroFold (F.FoldM step initial extract) = start where
  start = CoroEffect $ do
    v0 <- initial
    loop v0
  loop v0 = do
    b0 <- extract v0
    pure $ CoroYield b0 $ CoroAwait $ \a1 -> CoroEffect $ do
      v1 <- step v0 a1
      loop v1

coroView :: View a -> Coro IO a ()
coroView = coroFold . unView

coroStep :: Input a -> Coro IO a () -> IO (Maybe (Coro IO a ()))
coroStep inp = go where
  go = \case
    CoroAwait k -> atomically (recv inp) >>= maybe (pure Nothing) (go . k)
    CoroYield () x1 -> pure (Just x1)
    CoroEffect mx1 -> mx1 >>= go
    CoroEnd -> pure Nothing

coroRun :: Input a -> Coro IO a () -> IO ()
coroRun inp = go where
  go = \case
    CoroAwait k -> atomically (recv inp) >>= maybe (pure ()) (go . k)
    CoroYield () x1 -> go x1
    CoroEffect mx1 -> mx1 >>= go
    CoroEnd -> pure ()

runMVC :: Acquire (Input a, Coro IO a b, View b) -> IO ()
runMVC acq = withAcquire acq $ \(inp, coro, view) ->
  coroRun inp (coroCompose coro (coroView view))

