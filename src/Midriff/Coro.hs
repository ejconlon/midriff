-- {-# LANGUAGE UndecidableInstances #-}

module Midriff.Coro
  ( CoroT
  , Coro
  , CoroIO
  , inMapC
  , outMapC
  , bothMapC
  , joinC
  , runNatC
  , runC
  , awaitC
  , yieldC
  , liftC
  , wrapC
  , scanC
  , foldC
  , loopC
  , printC
  , forC
  , drawC
  , catC
  , fuseC
  , (.|)
  , eachC
  , bracketC
  , bracketC_
  , ListT (..)
  , List
  , ListIO
  , liftL
  , eachL
  , consL
  , wrapL
  )
where

import Control.Applicative (Alternative (..))
import Control.Foldl qualified as F
import Control.Monad (ap, join)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (Identity)
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Resource (MonadResource (..), allocate, release, allocate_, register)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Void (Void, absurd)

newtype CoroT i o m a = CoroT
  { unCoroT
      :: forall r
       . ((Maybe i -> r) -> r) --^ await
      -> (o -> r -> r)         --^ yield
      -> (m r -> r)            --^ lift
      -> (a -> r)              --^ end
      -> m ()                  --^ cleanup
      -> r
  }

type Coro i o = CoroT i o Identity

type CoroIO i o = CoroT i o IO

instance Functor (CoroT i o m) where
  fmap f (CoroT c) = CoroT (\req rep lif end cle -> c req rep lif (end . f) cle)

instance Applicative (CoroT i o m) where
  pure a = CoroT (\_ _ _ end cle -> lif (end a <$ cle))
  (<*>) = ap

instance Monad (CoroT i o m) where
  return = pure
  CoroT c >>= f = CoroT $ \req rep lif end cle ->
    let end' a = let (CoroT d) = f a in d req rep lif end (pure ())
    in  c req rep lif end' cle

instance MonadTrans (CoroT i o) where
  lift = liftC

instance MonadIO m => MonadIO (CoroT i o m) where
  liftIO = liftC . liftIO

instance MFunctor (CoroT i o) where
  hoist nat (CoroT c) = CoroT (\req rep lif end cle -> c req rep (lif . nat) end (nat cle))

instance MonadResource m => MonadResource (CoroT i o m) where
  liftResourceT = liftC . liftResourceT

inMapC :: (j -> i) -> CoroT i o m a -> CoroT j o m a
inMapC g (CoroT c) = CoroT (\req rep lif end -> c (\k -> req (k . fmap g)) rep lif end)

outMapC :: (o -> p) -> CoroT i o m a -> CoroT i p m a
outMapC f (CoroT c) = CoroT (\req rep lif end -> c req (rep . f) lif end)

bothMapC :: (j -> i) -> (o -> p) -> CoroT i o m a -> CoroT j p m a
bothMapC g f (CoroT c) = CoroT (\req rep lif end -> c (\k -> req (k . fmap g)) (rep . f) lif end)

joinC :: CoroT i o m (CoroT i o m a) -> CoroT i o m a
joinC (CoroT c) = CoroT $ \req rep lif end ->
  c req rep lif (\(CoroT d) -> d req rep lif end)

runNatC :: Monad m => (forall x. n x -> m x) -> CoroT () Void n a -> m a
runNatC nat (CoroT c) = c req rep lif end
 where
  req k = k (Just ())
  rep v _ = absurd v
  lif = join . nat
  end = pure

runC :: Monad m => CoroT () Void m a -> m a
runC (CoroT c) = c req rep lif end
 where
  req k = k (Just ())
  rep v _ = absurd v
  lif = join
  end = pure

awaitC :: CoroT i o m (Maybe i)
awaitC = CoroT (\req _ _ end -> req end)

yieldC :: o -> CoroT i o m ()
yieldC o = CoroT (\_ rep _ end -> rep o (end ()))

liftC :: Functor m => m a -> CoroT i o m a
liftC act = CoroT (\_ _ lif end -> lif (fmap end act))

wrapC :: Functor m => m (CoroT i o m a) -> CoroT i o m a
wrapC mc = CoroT (\req rep lif end -> lif (fmap (\(CoroT c) -> c req rep lif end) mc))

scanC :: Monad m => F.FoldM m i o -> CoroT i o m o
scanC (F.FoldM step initial extract) = start
 where
  start = wrapC $ do
    v0 <- initial
    loop v0
  loop v0 = do
    b0 <- extract v0
    pure $ do
      yieldC b0
      ma1 <- awaitC
      case ma1 of
        Nothing -> pure b0
        Just a1 -> wrapC $ do
          v1 <- step v0 a1
          loop v1

foldC :: Monad m => F.FoldM m i o -> CoroT i o m o
foldC (F.FoldM step initial extract) = start
 where
  start = wrapC $ do
    v0 <- initial
    loop v0
  loop v0 = do
    pure $ do
      ma1 <- awaitC
      case ma1 of
        Nothing -> liftC (extract v0)
        Just a1 -> wrapC $ do
          v1 <- step v0 a1
          loop v1

-- purelyC :: F.Fold o a -> CoroT () o m () -> m a
-- purelyC = undefined

-- impurelyC :: F.FoldM m o a -> CoroT () o m () -> m a
-- impurelyC = undefined

-- TODO generalize to coro
loopC :: (i -> ListT m o) -> CoroT i o m ()
loopC f = CoroT $ \req rep lif end ->
  req $ \case
    Nothing -> end ()
    Just i -> let (ListT (CoroT c)) = f i in c (\r -> r (Just ())) rep lif end

printC :: Show i => CoroIO i o ()
printC = awaitC >>= maybe (pure ()) (liftIO . print)

-- | Run the first coroutine, replacing yields with the second.
-- This corresponds to the pipes "respond" category where "yieldC"
-- is the right identity.
forC :: CoroT i o m a -> (o -> CoroT i p m ()) -> CoroT i p m a
forC (CoroT x) f = CoroT $ \req rep lif end ->
  let rep' o r = let (CoroT y) = f o in y req rep lif (const r)
  in  x req rep' lif end

-- | Run the second coroutine, replacing awaits with the first.
-- This corresponds to the pipes "request" category" where "awaitC"
-- is the left identity.
drawC :: CoroT i o m a -> CoroT a o m b -> CoroT i o m b
drawC (CoroT x) (CoroT y) = CoroT $ \req rep lif end ->
  let req' k = x req rep lif (k . Just)
  in  y req' rep lif end

-- | This is an identity of fuseC
catC :: CoroT a a m ()
catC = go
 where
  go = awaitC >>= maybe (pure ()) yieldC >> go

data F i o m a r
  = FAwait (Maybe i -> r)
  | FYield o r
  | FLift (m r)
  | FEnd a
  deriving stock (Functor)

newtype X i o m a = X {unX :: F i o m a (X i o m a)}

reflectC :: CoroT i o m a -> X i o m a
reflectC (CoroT c) = c (X . FAwait) (\o -> X . FYield o) (X . FLift) (X . FEnd)

-- | Run the second coroutine until blocked on await, then run the first
-- to respond; continuing until the second terminates.
fuseC :: Functor m => CoroT a b m () -> CoroT b c m r -> CoroT a c m r
fuseC c1 c2 = CoroT $ \req rep lif end ->
  let x1 = reflectC c1
      x2 = reflectC c2
      go1 (X z1) k = case z1 of
        FAwait j -> req (\mi -> go1 (j mi) k)
        FYield o r -> go2 r (k (Just o))
        FLift mr -> lif (fmap (`go1` k) mr)
        FEnd _ -> go3 (k Nothing)
      go2 y1 (X z2) = case z2 of
        FAwait k -> go1 y1 k
        FYield o r -> rep o (go2 y1 r)
        FLift mr -> lif (fmap (go2 y1) mr)
        FEnd a -> end a
      go3 (X z2) = case z2 of
        FAwait k -> go3 (k Nothing)
        FYield o r -> rep o (go3 r)
        FLift mr -> lif (fmap go3 mr)
        FEnd a -> end a
  in  go2 x1 x2

-- | An inline synonym for 'fuseC'
(.|) :: Functor m => CoroT a b m () -> CoroT b c m r -> CoroT a c m r
(.|) = fuseC
{-# INLINE (.|) #-}

infixr 2 .|

eachC :: Foldable f => f o -> CoroT i o m ()
eachC fa = CoroT $ \_ rep _ end ->
  let go = \case
        [] -> end ()
        a : as' -> rep a (go as')
  in  go (toList fa)

bracketC :: MonadResource m => IO a -> (a -> IO ()) -> (a -> CoroT i o m b) -> CoroT i o m b
bracketC acq rel use = wrapC $ allocate acq rel <&> \(key, a) ->
  CoroT $ \req rep lif end ->
    let (CoroT c) = use a
        end' b = lif (end b <$ liftIO (release key))
    in c req rep lif end'

bracketC_ :: MonadResource m => IO a -> IO () -> CoroT i o m b -> CoroT i o m b
bracketC_ acq rel (CoroT c) = wrapC $ allocate_ acq rel <&> \key ->
  CoroT $ \req rep lif end ->
    let end' b = lif (end b <$ liftIO (release key))
    in c req rep lif end'

cleanupC :: MonadResource m => IO () -> CoroT i o m b -> CoroT i o m b
cleanupC rel (CoroT c) = wrapC $ register rel <&> \key ->
  CoroT $ \req rep lif end ->
    let end' b = lif (end b <$ liftIO (release key))
    in c req rep lif end'

-- teeC :: CoroT i Void m () -> CoroT i i m ()
-- teeC = undefined

newtype ListT m a = ListT {enumerateC :: CoroT () a m ()}

type List = ListT Identity

type ListIO = ListT IO

instance Functor (ListT m) where
  fmap f (ListT x) = ListT (outMapC f x)

instance Applicative (ListT m) where
  pure = ListT . yieldC
  (<*>) = ap

instance Monad (ListT m) where
  return = pure
  ListT x >>= f = ListT (forC x (enumerateC . f))

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

-- reconsL :: Functor m => m (Maybe (a, ListT m a)) -> ListT m a
-- reconsL = wrapL . fmap (maybe empty (uncurry consL))

-- -- Expensive, avoid?
-- unconsL :: Monad m => ListT m a -> m (Maybe (a, ListT m a))
-- unconsL (ListT (CoroT c)) = c req rep lif end
--  where
--   req k = k (Just ())
--   rep a r = pure (Just (a, reconsL r))
--   lif = join
--   end () = pure Nothing

-- forceL :: Monad m => ListT m a -> m (Seq a)
-- forceL = go Empty
--  where
--   go !acc l0 = do
--     mayPair <- unconsL l0
--     case mayPair of
--       Nothing -> pure acc
--       Just (a, l1) -> go (acc :|> a) l1
