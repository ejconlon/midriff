module Midriff.Ring
  ( Ring
  , ringCap
  , ringNew
  , ringNewIO
  , ringWrite
  , Next (..)
  , Cursor
  , cursorNew
  , cursorNewIO
  , cursorHasNext
  , cursorTryNext
  , cursorNext
  , cursorFlush
  )
where

import Midriff.Gate (Gate (..))
import Midriff.Callback (Callback (..))
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TVar (TVar, newTVar, newTVarIO, readTVar, readTVarIO, stateTVar, writeTVar)
import Control.Monad ((>=>))
import Data.Maybe (isJust)
import Data.Vector (Vector)
import Data.Vector qualified as V

data Pos = Pos
  { posGen :: !Int
  , posIndex :: !Int
  }
  deriving stock (Eq, Ord, Show)

initPos :: Pos
initPos = Pos 0 0

nextPos :: Int -> Pos -> Pos
nextPos cap (Pos gen ix) =
  let ix' = ix + 1
  in  if ix' == cap
        then Pos (gen + 1) 0
        else Pos gen ix'

readLimit :: Pos -> Maybe Pos
readLimit (Pos gen ix) = do
  if gen == 0
    then if ix == 0 then Nothing else Just (Pos 0 0)
    else Just (Pos (gen - 1) ix)

elemDiff :: Int -> Pos -> Pos -> Int
elemDiff cap (Pos endGen endIx) (Pos startGen startIx) =
  cap * (endGen - startGen) + endIx - startIx

data Ring a = Ring
  { ringCap :: !Int
  , ringBuf :: !(Vector (TVar a))
  , ringWriteHead :: !(TVar Pos)
  } deriving stock (Eq)

data Next a = Next {nextDropped :: !Int, nextValue :: !a}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data Cursor a = Cursor
  { cursorRing :: !(Ring a)
  , cursorReadHead :: !(TVar Pos)
  } deriving stock (Eq)

uninit :: a
uninit = error "Evaluated uninitialized ring element"

ringNew :: Int -> STM (Ring a)
ringNew cap = Ring cap <$> V.generateM cap (const (newTVar uninit)) <*> newTVar initPos

ringNewIO :: Int -> IO (Ring a)
ringNewIO cap = Ring cap <$> V.generateM cap (const (newTVarIO uninit)) <*> newTVarIO initPos

ringWrite :: Ring a -> a -> STM ()
ringWrite (Ring cap buf hdVar) !val = do
  ix <- stateTVar hdVar (\hd -> let hd'@(Pos _ ix) = nextPos cap hd in (ix, hd'))
  writeTVar (buf V.! ix) val

cursorNew :: Ring a -> STM (Cursor a)
cursorNew r = do
  h <- readTVar (ringWriteHead r)
  v <- newTVar h
  pure (Cursor r v)

cursorNewIO :: Ring a -> IO (Cursor a)
cursorNewIO r = do
  h <- readTVarIO (ringWriteHead r)
  v <- newTVarIO h
  pure (Cursor r v)

cursorHasNext :: Cursor a -> STM Bool
cursorHasNext (Cursor r rhVar) = do
  rh <- readTVar rhVar
  wh <- readTVar (ringWriteHead r)
  pure ((rh /= wh) && isJust (readLimit wh))

cursorNext :: Cursor a -> STM (Next a)
cursorNext = cursorTryNext >=> maybe retry pure

cursorTryNext :: Cursor a -> STM (Maybe (Next a))
cursorTryNext (Cursor r rhVar) = do
  rh <- readTVar rhVar
  wh <- readTVar (ringWriteHead r)
  if rh == wh
    then pure Nothing
    else case readLimit wh of
      Nothing -> pure Nothing
      Just rl -> do
        let (dropped, used) = if rh >= rl then (0, rh) else (elemDiff (ringCap r) rl rh, rl)
            !pos = nextPos (ringCap r) used
        writeTVar rhVar pos
        fmap (Just . Next dropped) (readTVar (ringBuf r V.! posIndex used))

-- | Flush at most capacity elements. Most useful if you know there are no more writes.
cursorFlush :: Cursor a -> Callback STM (Next a) -> IO ()
cursorFlush cur k = go (ringCap (cursorRing cur)) where
  go !left = do
    if left == 0
      then pure ()
      else do
        g <- atomically (cursorTryNext cur >>= maybe (pure GateClosed) (cbRun k))
        case g of
          GateClosed -> pure ()
          GateOpen -> go (left - 1)

