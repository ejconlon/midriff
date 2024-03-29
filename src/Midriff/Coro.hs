module Midriff.Coro
  ( Consumed (..)
  , Sig (..)
  , CoroT (..)
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
  , registerC
  , passthroughC
  , mergeSourceC
  , ZipSourceC (..)
  , zipWithSourceC
  , ZipSinkC (..)
  , zipWithSinkC
  , ZipC (..)
  , zipWithC
  , ListT (..)
  , liftL
  , eachL
  , consL
  , wrapL
  )
where

import Control.Applicative (Alternative (..), liftA2)
import Control.Foldl qualified as F
import Control.Monad (ap, join)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Resource (MonadResource (..), allocate, allocate_, register, release)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Void (Void, absurd)

data Consumed = ConsumedNo | ConsumedYes
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Sig = SigCont | SigEnd Consumed
  deriving stock (Eq, Ord, Show)

newtype CoroT i o m a = CoroT
  { unCoroT
      :: forall r
       . ((Maybe i -> r) -> r)
      -> (o -> (Sig -> r) -> r)
      -> (m r -> r)
      -> (a -> r)
      -> r
  }

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
yieldC o = CoroT (\_ rep _ end -> rep o (const (end ())))

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
      ma1 <- awaitC
      case ma1 of
        Nothing -> pure ()
        Just a1 -> wrapC $ do
          v1 <- step v0 a1
          loop v1

foldC :: Monad m => F.FoldM m i o -> CoroT i Void m o
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

loopC :: (i -> ListT m o) -> CoroT i o m ()
loopC f = CoroT $ \req rep lif end ->
  req $ \case
    Nothing -> end ()
    Just i -> let (ListT (CoroT c)) = f i in c (\r -> r (Just ())) rep lif end

printC :: (Show i, MonadIO m) => CoroT i Void m ()
printC = awaitC >>= maybe (pure ()) (liftIO . print)

-- | Run the first coroutine, replacing yields with the second.
-- This corresponds to the pipes "respond" category where "yieldC"
-- is the right identity.
forC :: CoroT i o m a -> (o -> CoroT i p m ()) -> CoroT i p m a
forC (CoroT x) f = CoroT $ \req rep lif end ->
  let rep' o k = let (CoroT y) = f o in y req rep lif (const (k SigCont))
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
  | FYield o (Sig -> r)
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
  let stepFirst (X z1) k = case z1 of
        FAwait j -> req (\mi -> stepFirst (j mi) k)
        FYield o j -> stepSecond j (k (Just o))
        FLift mr -> lif (fmap (`stepFirst` k) mr)
        FEnd _ -> drainSecond (k Nothing)
      stepSecond j (X z2) = case z2 of
        FAwait k -> stepFirst (j SigCont) k
        FYield o k -> rep o (stepSecond j . k)
        FLift mr -> lif (fmap (stepSecond j) mr)
        FEnd a -> drainFirst (j (SigEnd ConsumedYes)) a
      drainFirst (X z1) a = case z1 of
        FAwait j -> req (\mi -> drainFirst (j mi) a)
        FYield _ j -> drainFirst (j (SigEnd ConsumedNo)) a
        FLift mr -> lif (fmap (`drainFirst` a) mr)
        FEnd _ -> end a
      drainSecond (X z2) = case z2 of
        FAwait k -> drainSecond (k Nothing)
        FYield o k -> rep o (drainSecond . k)
        FLift mr -> lif (fmap drainSecond mr)
        FEnd a -> end a
      mkX1 = \case
        SigCont -> reflectC c1
        SigEnd _ -> X (FEnd ())
      x2 = reflectC c2
  in  stepSecond mkX1 x2

-- | An inline synonym for 'fuseC'
(.|) :: Functor m => CoroT a b m () -> CoroT b c m r -> CoroT a c m r
(.|) = fuseC
{-# INLINE (.|) #-}

infixr 2 .|

eachC :: Foldable f => f o -> CoroT i o m ()
eachC fa = CoroT $ \_ rep _ end ->
  let go = \case
        [] -> end ()
        a : as' -> rep a $ \case
          SigCont -> go as'
          SigEnd _ -> end ()
  in  go (toList fa)

bracketC :: MonadResource m => IO a -> (a -> IO ()) -> (a -> CoroT i o m b) -> CoroT i o m b
bracketC acq rel use =
  wrapC $
    allocate acq rel <&> \(key, a) ->
      CoroT $ \req rep lif end ->
        let (CoroT c) = use a
            end' b = lif (end b <$ liftIO (release key))
        in  c req rep lif end'

bracketC_ :: MonadResource m => IO a -> IO () -> CoroT i o m b -> CoroT i o m b
bracketC_ acq rel (CoroT c) =
  wrapC $
    allocate_ acq rel <&> \key ->
      CoroT $ \req rep lif end ->
        let end' b = lif (end b <$ liftIO (release key))
        in  c req rep lif end'

registerC :: MonadResource m => IO () -> CoroT i o m b -> CoroT i o m b
registerC rel (CoroT c) =
  wrapC $
    register rel <&> \key ->
      CoroT $ \req rep lif end ->
        let end' b = lif (end b <$ liftIO (release key))
        in  c req rep lif end'

passthroughC :: Functor m => CoroT i Void m () -> CoroT i i m ()
passthroughC c = CoroT $ \req rep lif end ->
  let step mi (X z) = case z of
        FAwait k -> do
          case mi of
            Nothing -> req (\mi' -> step mi' (k mi'))
            Just i -> rep i $ \case
              SigCont -> req (\mi' -> step mi' (k mi'))
              SigEnd _ -> end ()
        FYield o _ -> absurd o
        FLift mr -> lif (fmap (step mi) mr)
        FEnd _ ->
          case mi of
            Nothing -> end ()
            Just i -> rep i (const (end ()))
  in  step Nothing (reflectC c)

mergeSourceC :: Functor m => CoroT () i m () -> CoroT a (i, a) m ()
mergeSourceC c = CoroT $ \req rep lif end ->
  let step (X z) = case z of
        FAwait k -> step (k (Just ()))
        FYield i k -> req $ \case
          Nothing -> end ()
          Just a -> rep (i, a) (step . k)
        FLift mr -> lif (fmap step mr)
        FEnd _ -> end ()
  in  step (reflectC c)

newtype ZipSourceC m o = ZipSourceC {unZipSourceC :: CoroT () o m ()}

instance Functor (ZipSourceC m) where
  fmap f (ZipSourceC c) = ZipSourceC (outMapC f c)

instance Functor m => Applicative (ZipSourceC m) where
  pure = ZipSourceC . yieldC
  liftA2 f (ZipSourceC c1) (ZipSourceC c2) = ZipSourceC (zipWithSourceC f c1 c2)

zipWithSourceC :: Functor m => (o1 -> o2 -> o3) -> CoroT () o1 m () -> CoroT () o2 m () -> CoroT () o3 m ()
zipWithSourceC f c1 c2 = CoroT $ \_ rep lif end ->
  let stepFirst (X z1) j = case z1 of
        FAwait k -> stepFirst (k (Just ())) j
        FYield o1 k -> stepSecond o1 k (j SigCont)
        FLift mr -> lif (fmap (`stepFirst` j) mr)
        FEnd _ -> end ()
      stepSecond o1 k (X z2) = case z2 of
        FAwait j -> stepSecond o1 k (j (Just ()))
        FYield o2 j -> rep (f o1 o2) (\s -> stepFirst (k s) j)
        FLift mr -> lif (fmap (stepSecond o1 k) mr)
        FEnd _ -> end ()
  in  stepFirst (reflectC c1) (\case SigCont -> reflectC c2; _ -> X (FEnd ()))

seqSourceC :: (Traversable f, Functor m) => f (CoroT () o m ()) -> CoroT () (f o) m ()
seqSourceC = unZipSourceC . traverse ZipSourceC

newtype ZipSinkC i m r = ZipSinkC {unZipSinkC :: CoroT i Void m r}

instance Functor (ZipSinkC i m) where
  fmap f (ZipSinkC c) = ZipSinkC (fmap f c)

instance Functor m => Applicative (ZipSinkC i m) where
  pure = ZipSinkC . pure
  liftA2 f (ZipSinkC c1) (ZipSinkC c2) = ZipSinkC (zipWithSinkC f c1 c2)

zipWithSinkC :: Functor m => (r1 -> r2 -> r3) -> CoroT i Void m r1 -> CoroT i Void m r2 -> CoroT i Void m r3
zipWithSinkC f c1 c2 = CoroT $ \req _ lif end ->
  let stepFirst mi y1@(X z1) y2 = case z1 of
        FAwait k ->
          case mi of
            Nothing -> req (\mi' -> stepSecond mi' (k mi') y2)
            Just _ -> stepSecond mi y1 y2
        FYield o _ -> absurd o
        FLift mr -> lif (fmap (\r -> stepFirst mi r y2) mr)
        FEnd r1 -> drainSecond r1 mi y2
      stepSecond mi y1 (X z2) = case z2 of
        FAwait j -> stepFirst Nothing y1 (j mi)
        FYield o _ -> absurd o
        FLift mr -> lif (fmap (stepSecond mi y1) mr)
        FEnd r2 -> drainFirst r2 y1
      drainFirst r2 (X z1) = case z1 of
        FAwait k -> req (drainFirst r2 . k)
        FYield o _ -> absurd o
        FLift mr -> lif (fmap (drainFirst r2) mr)
        FEnd r1 -> end (f r1 r2)
      drainSecond r1 mi (X z2) = case z2 of
        FAwait j ->
          case mi of
            Nothing -> req (drainSecond r1 Nothing . j)
            Just _ -> drainSecond r1 Nothing (j mi)
        FYield o _ -> absurd o
        FLift mr -> lif (fmap (drainSecond r1 mi) mr)
        FEnd r2 -> end (f r1 r2)
  in  stepFirst Nothing (reflectC c1) (reflectC c2)

seqSinkC :: (Traversable f, Functor m) => f (CoroT i Void m r) -> CoroT i Void m (f r)
seqSinkC = unZipSinkC . traverse ZipSinkC

newtype ZipC i o m r = ZipC {unZipC :: CoroT i o m r}

instance Functor (ZipC i o m) where
  fmap f (ZipC c) = ZipC (fmap f c)

instance Functor m => Applicative (ZipC i o m) where
  pure = ZipC . pure
  liftA2 f (ZipC c1) (ZipC c2) = ZipC (zipWithC f c1 c2)

zipWithC :: Functor m => (r1 -> r2 -> r3) -> CoroT i o m r1 -> CoroT i o m r2 -> CoroT i o m r3
zipWithC f c1 c2 = CoroT $ \req rep lif end ->
  let stepFirst mi y1@(X z1) y2 = case z1 of
        FAwait k ->
          case mi of
            Nothing -> req (\mi' -> stepSecond mi' (k mi') y2)
            Just _ -> stepSecond mi y1 y2
        FYield o k -> rep o (\s -> stepFirst mi (k s) y2)
        FLift mr -> lif (fmap (\r -> stepFirst mi r y2) mr)
        FEnd r1 -> drainSecond r1 mi y2
      stepSecond mi y1 (X z2) = case z2 of
        FAwait j -> stepFirst Nothing y1 (j mi)
        FYield o j -> rep o (stepSecond mi y1 . j)
        FLift mr -> lif (fmap (stepSecond mi y1) mr)
        FEnd r2 -> drainFirst r2 y1
      drainFirst r2 (X z1) = case z1 of
        FAwait k -> req (drainFirst r2 . k)
        FYield o k -> rep o (drainFirst r2 . k)
        FLift mr -> lif (fmap (drainFirst r2) mr)
        FEnd r1 -> end (f r1 r2)
      drainSecond r1 mi (X z2) = case z2 of
        FAwait j ->
          case mi of
            Nothing -> req (drainSecond r1 Nothing . j)
            Just _ -> drainSecond r1 Nothing (j mi)
        FYield o j -> rep o (drainSecond r1 mi . j)
        FLift mr -> lif (fmap (drainSecond r1 mi) mr)
        FEnd r2 -> end (f r1 r2)
  in  stepFirst Nothing (reflectC c1) (reflectC c2)

seqC :: (Traversable f, Functor m) => f (CoroT i o m r) -> CoroT i o m (f r)
seqC = unZipC . traverse ZipC

newtype ListT m a = ListT {enumerateC :: CoroT () a m ()}

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
