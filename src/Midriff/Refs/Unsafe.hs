module Midriff.Refs.Unsafe
  ( UnsafeRef (..)
  ) where

import Data.Kind (Type)
import Midriff.Refs.Classes (AtomicRef (..), LockRef (..), ModifyRef (..), NewRef (..), ReadWriteRef (..),
                             unsafeAtomicModifyRef, unsafeModifyLockRef, unsafeModifyLockRef_, unsafeModifyRef,
                             unsafeWithLockRef)

-- | Sometimes you want to write your app with max correctness
-- (e.g. using 'LockRef' constraints) but you don't always want to use it.
-- If you know you're not going to need it, wrap a lesser ref in 'UnsafeRef' to
-- have those more expensive operations translated to use 'ReadWriteRef'.
newtype UnsafeRef (r :: Type -> Type) (m :: Type -> Type) (a :: Type) =
  UnsafeRef { unUnsafeRef :: r a }

instance (NewRef r m, Functor m) => NewRef (UnsafeRef r m) m where
  newRef = fmap UnsafeRef . newRef

instance ReadWriteRef r m => ReadWriteRef (UnsafeRef r m) m where
  readRef = readRef . unUnsafeRef
  writeRef = writeRef . unUnsafeRef

instance (ReadWriteRef r m, Monad m) => ModifyRef (UnsafeRef r m) m where
  modifyRef = unsafeModifyRef . unUnsafeRef

instance (ReadWriteRef r m, Monad m) => AtomicRef (UnsafeRef r m) m where
  atomicModifyRef = unsafeAtomicModifyRef . unUnsafeRef

instance (ReadWriteRef r m, Monad m) => LockRef (UnsafeRef r m) m where
  withLockRef = unsafeWithLockRef . unUnsafeRef
  modifyLockRef = unsafeModifyLockRef . unUnsafeRef
  modifyLockRef_ = unsafeModifyLockRef_ . unUnsafeRef
