module Midriff.Mvc where

import Control.Applicative (Alternative (..), liftA2)
-- import Data.Acquire (Acquire)
import Control.Concurrent.STM (STM)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Functor.Contravariant.Divisible (Divisible (..), Decidable (..))
import Data.Void (absurd)
import Optics (Is, Optic', preview, An_AffineFold)

newtype Input a = Input { recv :: STM (Maybe a) }
  deriving (Functor, Applicative, Monad, Alternative) via (MaybeT STM)

instance Semigroup (Input a) where
  (<>) = (<|>)

instance Monoid (Input a) where
  mempty = empty
  mappend = (<>)

keep :: Is k An_AffineFold => Optic' k is a b -> Input  a -> Input b
keep optic x = Input (fmap (>>= preview optic) (recv x))

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

-- newtype InputStream m a = InputStream m { unSource :: Producer a }

-- inputSink = undeifned

-- newtype Sink a = Sink { unSink :: Sink a }

-- outputSink :: Output a -> Sink a
-- outputSink = undefined

