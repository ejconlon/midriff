module Midriff.Coro where

import Control.Applicative (Alternative (..))
import Control.Foldl qualified as F
import Control.Monad (ap, join)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (Identity)
import Control.Monad.Morph (MFunctor (..), MMonad (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Void (Void)

data Done = DoneNo | DoneYes
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Semigroup Done where
  DoneYes <> _ = DoneYes
  _ <> d = d

instance Monoid Done where
  mempty = DoneNo
  mappend = (<>)

data Term a = TermAwait | TermYield | TermEnd !a
  deriving stock (Eq, Ord, Show)

newtype CoroT i o m a = CoroT
  { unCoroT
      :: forall r
       . ((i -> r) -> r)
      -> (o -> r)
      -> (m r -> r)
      -> (a -> r)
      -> r
  }

type Coro i o = CoroT i o Identity

type CoroIO i o = CoroT i o IO

instance Functor (CoroT i o m) where
  fmap f (CoroT c) = CoroT (\req rep lif end -> c req rep lif (end . f))

instance Applicative (CoroT i o m) where
  pure a = CoroT (\_ _ _ end -> end a)
  (<*>) = ap

instance Monad (CoroT i o m) where
  return = pure
  CoroT c >>= f = CoroT $ \req rep lif end ->
    let end' a = let (CoroT d) = f a in d req rep lif end
    in  c req rep lif end'

instance MonadTrans (CoroT i o) where
  lift = liftC

instance MonadIO m => MonadIO (CoroT i o m) where
  liftIO = liftC . liftIO

instance MFunctor (CoroT i o) where
  hoist nat (CoroT c) = CoroT (\req rep lif end -> c req rep (lif . nat) end)

inMapC :: (j -> i) -> CoroT i o m a -> CoroT j o m a
inMapC g (CoroT c) = CoroT (\req rep lif end -> c (\r -> req (r . g)) rep lif end)

outMapC :: (o -> p) -> CoroT i o m a -> CoroT i p m a
outMapC f (CoroT c) = CoroT (\req rep lif end -> c req (rep . f) lif end)

bothMapC :: (j -> i) -> (o -> p) -> CoroT i o m a -> CoroT j p m a
bothMapC g f (CoroT c) = CoroT (\req rep lif end -> c (\r -> req (r . g)) (rep . f) lif end)

joinC :: CoroT i o m (CoroT i o m a) -> CoroT i o m a
joinC (CoroT c) = CoroT $ \req rep lif end ->
  c req rep lif (\(CoroT d) -> d req rep lif end)

runForeverC :: Monad m => m (Maybe i) -> CoroT i () m a -> m (Maybe a)
runForeverC inp (CoroT c) = go
 where
  go = c req rep lif end
  req r = inp >>= maybe (pure Nothing) r
  rep _ = go
  lif = join
  end = pure . Just

runTermC :: Monad m => (forall x. n x -> m x) -> m (Maybe i) -> (o -> m Done) -> CoroT i o n a -> m (Term a)
runTermC nat inp out (CoroT c) = go
 where
  go = c req rep lif end
  req r = inp >>= maybe (pure TermAwait) r
  rep o = out o >>= \case DoneYes -> pure TermYield; DoneNo -> go
  lif mc = join (nat mc)
  end = pure . TermEnd

awaitC :: CoroT i o m i
awaitC = CoroT (\req _ _ end -> req end)

yieldC :: o -> CoroT i o m ()
yieldC o = CoroT (\_ rep _ _ -> rep o)

liftC :: Functor m => m a -> CoroT i o m a
liftC act = CoroT (\_ _ lif end -> lif (fmap end act))

wrapC :: Functor m => m (CoroT i o m a) -> CoroT i o m a
wrapC mc = CoroT (\req rep lif end -> lif (fmap (\(CoroT c) -> c req rep lif end) mc))

scanC :: Monad m => F.FoldM m i o -> CoroT i o m ()
scanC (F.FoldM step initial extract) = start
 where
  start = wrapC $ do
    v0 <- initial
    loop v0
  loop v0 = do
    b0 <- extract v0
    pure $ do
      yieldC b0
      a1 <- awaitC
      wrapC $ do
        v1 <- step v0 a1
        loop v1

effectC :: Monad m => F.FoldM m i () -> CoroT i () m ()
effectC (F.FoldM step initial _) = start
 where
  start = wrapC $ do
    v0 <- initial
    loop v0
  loop v0 = do
    pure $ do
      a1 <- awaitC
      wrapC $ do
        v1 <- step v0 a1
        loop v1

loopC :: (i -> ListT m o) -> CoroT i o m ()
loopC f = CoroT $ \req rep lif end ->
  req (\i -> let (ListT (CoroT c)) = f i in c (const (end ())) rep lif end)

forC :: CoroT i o m a -> CoroT o p m () -> CoroT i p m a
forC (CoroT x) (CoroT y) = CoroT $ \req rep lif end ->
  let rep' o = undefined
  in  x req rep' lif end

drawC :: CoroT i o m a -> CoroT a o m b -> CoroT i o m b
drawC (CoroT x) (CoroT y) = CoroT $ \req rep lif end -> undefined

newtype ListT m a = ListT {selectC :: CoroT Void a m ()}

type List = ListT Identity

type ListIO = ListT IO

instance Functor (ListT m) where
  fmap f (ListT x) = ListT (outMapC f x)

instance Applicative (ListT m) where
  pure = ListT . yieldC
  (<*>) = ap

instance Monad (ListT m) where
  return = pure
  ListT x >>= f = ListT (forC x (loopC f))

instance MonadTrans ListT where
  lift = liftL

instance MonadIO m => MonadIO (ListT m) where
  liftIO = liftL . liftIO

instance Alternative (ListT m) where
  empty = ListT (pure ())
  ListT x <|> ListT y = ListT (x >> y)

instance Semigroup (ListT m a) where
  (<>) = (<|>)

instance Monoid (ListT m a) where
  mempty = empty
  mappend = (<>)

instance MFunctor ListT where
  -- hoist :: forall m n (b :: k). Monad m => (forall a. m a -> n a) -> t m b -> t n b
  hoist nat (ListT x) = ListT (hoist nat x)

instance MMonad ListT where
  embed spread (ListT (CoroT c)) = ListT $ CoroT $ \req rep lif end ->
    let lif' act = let (ListT (CoroT d)) = spread act in d req id lif end
    in  c req rep lif' end

liftL :: Functor m => m a -> ListT m a
liftL act = ListT (liftC act >>= yieldC)
