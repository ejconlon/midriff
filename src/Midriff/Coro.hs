module Midriff.Coro where

import Control.Applicative (Alternative (..))
import Control.Foldl qualified as F
import Control.Monad (ap, join)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (Identity)
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Foldable (toList)
import Data.Sequence (Seq (..))

newtype CoroT i o m a = CoroT
  { unCoroT
      :: forall r
       . ((i -> r) -> r)
      -> (o -> r -> r)
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
inMapC g (CoroT c) = CoroT (\req rep lif end -> c (\k -> req (k . g)) rep lif end)

outMapC :: (o -> p) -> CoroT i o m a -> CoroT i p m a
outMapC f (CoroT c) = CoroT (\req rep lif end -> c req (rep . f) lif end)

bothMapC :: (j -> i) -> (o -> p) -> CoroT i o m a -> CoroT j p m a
bothMapC g f (CoroT c) = CoroT (\req rep lif end -> c (\k -> req (k . g)) (rep . f) lif end)

joinC :: CoroT i o m (CoroT i o m a) -> CoroT i o m a
joinC (CoroT c) = CoroT $ \req rep lif end ->
  c req rep lif (\(CoroT d) -> d req rep lif end)

runForeverC :: Monad m => (forall x. n x -> m x) -> m (Maybe i) -> CoroT i o n a -> m (Maybe a)
runForeverC nat inp (CoroT c) = c req rep lif end
 where
  req k = inp >>= maybe (pure Nothing) k
  rep _ r = r
  lif = join . nat
  end = pure . Just

runEffectC :: Monad m => (forall x. n x -> m x) -> CoroT () o n a -> m a
runEffectC nat (CoroT c) = c req rep lif end
 where
  req k = k ()
  rep _ r = r
  lif mc = join (nat mc)
  end = pure

awaitC :: CoroT i o m i
awaitC = CoroT (\req _ _ end -> req end)

yieldC :: o -> CoroT i o m ()
yieldC o = CoroT (\_ rep _ end -> rep o (end ()))

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

foldC :: Monad m => F.FoldM m i () -> CoroT i o m ()
foldC (F.FoldM step initial _) = start
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
  req (\i -> let (ListT (CoroT c)) = f i in c (\r -> r ()) rep lif end)

printC :: Show i => CoroIO i o ()
printC = awaitC >>= liftIO . print

-- | Run the first coroutine, replacing yields with the second
forC :: CoroT i o m a -> CoroT o p m () -> CoroT i p m a
forC (CoroT x) (CoroT y) = CoroT $ \req rep lif end ->
  let rep' o r = y (\k -> k o) rep lif (const r)
  in  x req rep' lif end

-- | Run the second coroutine, replacing awaits with the first
drawC :: CoroT i o m a -> CoroT a o m b -> CoroT i o m b
drawC (CoroT x) (CoroT y) = CoroT $ \req rep lif end ->
  let req' = x req rep lif
  in  y req' rep lif end

eachC :: Foldable f => f o -> CoroT i o m ()
eachC fa =
  let as = toList fa
  in  CoroT $ \_ rep _ end -> go rep end as
 where
  go w z = \case
    [] -> z ()
    a : as' -> w a (go w z as')

newtype ListT m a = ListT {selectC :: CoroT () a m ()}

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

liftL :: Functor m => m a -> ListT m a
liftL act = ListT (liftC act >>= yieldC)

eachL :: Foldable f => f a -> ListT m a
eachL = ListT . eachC

consL :: a -> ListT m a -> ListT m a
consL a (ListT x) = ListT (yieldC a >> x)

wrapL :: Functor m => m (ListT m a) -> ListT m a
wrapL ml = ListT (CoroT (\req rep lif end -> lif (fmap (\(ListT (CoroT c)) -> c req rep lif end) ml)))

reconsL :: Functor m => m (Maybe (a, ListT m a)) -> ListT m a
reconsL = wrapL . fmap (maybe empty (uncurry consL))

unconsL :: Monad m => ListT m a -> m (Maybe (a, ListT m a))
unconsL (ListT (CoroT c)) = c req rep lif end
 where
  req k = k ()
  rep a r = pure (Just (a, reconsL r))
  lif = join
  end () = pure Nothing

forceL :: Monad m => ListT m a -> m (Seq a)
forceL = go Empty
 where
  go !acc l0 = do
    mayPair <- unconsL l0
    case mayPair of
      Nothing -> pure acc
      Just (a, l1) -> go (acc :|> a) l1
