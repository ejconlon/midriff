module Midriff.Connect
  ( InputMsg
  , OutputAction (..)
  , OutputCallback
  , acquireInput
  , acquireOutput
  )
where

import Control.Concurrent.STM (atomically)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Primitive (PrimState)
import Data.Acquire (Acquire, mkAcquire)
import Data.Vector.Storable (Vector)
import Data.Vector.Storable.Mutable (MVector)
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Word (Word8)
import Foreign.ForeignPtr.Unsafe qualified as FPU
import Midriff.CQueue (CQueue, cqNewIO)
import Midriff.Callback (Callback)
import Midriff.Config (DeviceConfig (..), Ignores (..), InputConfig (..), PortConfig (..), PortId (..))
import Midriff.Gate (Gate (..))
import Midriff.Latch (latchClose, latchNewIO)
import Midriff.Ring (Ring, ringNewIO, ringWrite)
import Sound.RtMidi
  ( InputDevice
  , OutputDevice
  , cancelCallback
  , closePort
  , createInput
  , createOutput
  , defaultInput
  , defaultOutput
  , ignoreTypes
  , openPort
  , openVirtualPort
  , sendUnsafeMessage
  , setCallback
  )

data InputMsg = InputMsg {imDelta :: !Double, imPayload :: !(Vector Word8)}
  deriving stock (Eq, Ord, Show)

newtype OutputAction = OutputAction {runOutputAction :: forall m. MVector (PrimState m) Word8 -> m Int}

type OutputCallback = Callback IO OutputAction

openInputDev :: Maybe DeviceConfig -> IO InputDevice
openInputDev mdc = do
  case mdc of
    Nothing -> defaultInput
    -- Empty queue is fine for this, since we set a callback
    Just (DeviceConfig api client) -> createInput api client 0

openInputPort :: Ring InputMsg -> InputDevice -> InputConfig -> IO ()
openInputPort ring dev (InputConfig (PortConfig name pid) migs) = do
  -- Set our ignored message types
  case migs of
    Nothing -> pure ()
    Just (Ignores x y z) -> ignoreTypes dev x y z
  -- Set our callback to enqueue to our own ring buffer
  let cb d vs = atomically (void (ringWrite ring (InputMsg d vs)))
  setCallback dev cb
  -- Open the port for input
  case pid of
    PortIdReal pnum -> openPort dev pnum name
    PortIdVirtual -> openVirtualPort dev name

acquireInput :: Maybe DeviceConfig -> InputConfig -> Int -> Acquire (CQueue InputMsg)
acquireInput mdc ic cap = fmap (\(_, _, cq) -> cq) (mkAcquire alloc free)
 where
  alloc = do
    ring <- ringNewIO cap
    latch <- latchNewIO
    cq <- cqNewIO latch ring
    dev <- liftIO (openInputDev mdc)
    openInputPort ring dev ic
    pure (dev, latch, cq)
  free (dev, latch, _) = do
    closePort dev
    cancelCallback dev
    atomically (latchClose latch)

openOutputDev :: Maybe DeviceConfig -> IO OutputDevice
openOutputDev mdc = do
  case mdc of
    Nothing -> defaultOutput
    Just (DeviceConfig api client) -> createOutput api client

openOutputPort :: OutputDevice -> PortConfig -> IO ()
openOutputPort dev (PortConfig name pid) = do
  -- Open the port for output
  case pid of
    PortIdReal pnum -> openPort dev pnum name
    PortIdVirtual -> openVirtualPort dev name

-- | Returned callback is not thread-safe!
-- TODO turn into ref
acquireOutput :: Maybe DeviceConfig -> PortConfig -> Int -> Acquire OutputCallback
acquireOutput mdc pc cap = fmap mkCb (mkAcquire alloc free)
 where
  alloc = do
    dev <- openOutputDev mdc
    mvec <- VSM.new @IO @Word8 cap
    let (fptr, _) = VSM.unsafeToForeignPtr0 mvec
        ptr = FPU.unsafeForeignPtrToPtr fptr
    openOutputPort dev pc
    pure (dev, ptr, mvec)
  free (dev, _, _) = closePort dev
  mkCb (dev, ptr, mvec) act = do
    used <- runOutputAction act mvec
    when (used > 0) (sendUnsafeMessage dev ptr used)
    pure GateOpen
