module Midriff.Connect
  ( InputMsg
  , OutputAction (..)
  , OutputCallback
  , InputArgs (..)
  , inputNew
  , inputPlex
  , OutputArgs (..)
  , outputNew
  , outputPlex
  , ConnArgs (..)
  , ConnDevice (..)
  , connNew
  , connPlex
  )
where

import Control.Monad (when, (>=>))
import Control.Monad.Primitive (PrimState)
import Control.Monad.Trans.Resource (MonadResource, MonadUnliftIO)
import Data.Vector.Storable (Vector)
import Data.Vector.Storable.Mutable (MVector)
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Word (Word8)
import Foreign.ForeignPtr.Unsafe qualified as FPU
import Midriff.Callback (Callback)
import Midriff.Config (DeviceConfig (..), Ignores (..), InputConfig (..), PortConfig (..), PortId (..))
import Midriff.Gate (Gate (..))
import Midriff.Plex (Plex, plexNewU)
import Midriff.Resource (Manager, managerNew, refNew)
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

type InputCallback = InputMsg -> IO ()

newtype OutputAction = OutputAction {runOutputAction :: forall m. MVector (PrimState m) Word8 -> m Int}

type OutputCallback = Callback IO OutputAction

inputOpenDev :: Maybe DeviceConfig -> IO InputDevice
inputOpenDev mdc = do
  case mdc of
    Nothing -> defaultInput
    -- Empty queue is fine for this, since we set a callback
    Just (DeviceConfig api client) -> createInput api client 0

inputOpenPort :: InputCallback -> InputDevice -> InputConfig -> IO ()
inputOpenPort cb dev (InputConfig (PortConfig name pid) migs) = do
  -- Set our ignored message types
  case migs of
    Nothing -> pure ()
    Just (Ignores x y z) -> ignoreTypes dev x y z
  -- Set a callback to enqueue to our own ring buffer, for example
  setCallback dev (\d vs -> cb (InputMsg d vs))
  -- Open the port for input
  case pid of
    PortIdReal pnum -> openPort dev pnum name
    PortIdVirtual -> openVirtualPort dev name

data InputArgs = InputArgs !(Maybe DeviceConfig) !InputConfig !InputCallback

inputNew :: InputArgs -> Manager InputDevice
inputNew (InputArgs mdc ic cb) = managerNew alloc free
 where
  alloc = do
    dev <- inputOpenDev mdc
    inputOpenPort cb dev ic
    pure dev
  free dev = do
    closePort dev
    cancelCallback dev

inputPlex :: (MonadResource m, MonadUnliftIO m) => (k -> m InputArgs) -> m (Plex k InputDevice)
inputPlex f = plexNewU (f >=> refNew . inputNew)

outputOpenDev :: Maybe DeviceConfig -> IO OutputDevice
outputOpenDev mdc = do
  case mdc of
    Nothing -> defaultOutput
    Just (DeviceConfig api client) -> createOutput api client

outputOpenPort :: OutputDevice -> PortConfig -> IO ()
outputOpenPort dev (PortConfig name pid) = do
  -- Open the port for output
  case pid of
    PortIdReal pnum -> openPort dev pnum name
    PortIdVirtual -> openVirtualPort dev name

data OutputArgs = OutputArgs !(Maybe DeviceConfig) !PortConfig
  deriving stock (Eq)

outputNew :: OutputArgs -> Manager OutputDevice
outputNew (OutputArgs mdc pc) = managerNew alloc free
 where
  alloc = do
    dev <- outputOpenDev mdc
    outputOpenPort dev pc
    pure dev
  free = closePort

outputCb :: OutputDevice -> Int -> IO OutputCallback
outputCb dev cap = do
  mvec <- VSM.new @IO @Word8 cap
  let (fptr, _) = VSM.unsafeToForeignPtr0 mvec
      ptr = FPU.unsafeForeignPtrToPtr fptr
  pure $ \act -> do
    used <- runOutputAction act mvec
    when (used > 0) (sendUnsafeMessage dev ptr used)
    pure GateOpen

outputPlex :: (MonadResource m, MonadUnliftIO m) => (k -> m OutputArgs) -> m (Plex k OutputDevice)
outputPlex f = plexNewU (f >=> refNew . outputNew)

data ConnArgs = ConnArgsInput !InputArgs | ConnArgsOutput !OutputArgs

data ConnDevice = ConnDeviceInput !InputDevice | ConnDeviceOutput !OutputDevice

connNew :: ConnArgs -> Manager ConnDevice
connNew = \case
  ConnArgsInput ia -> fmap ConnDeviceInput (inputNew ia)
  ConnArgsOutput oa -> fmap ConnDeviceOutput (outputNew oa)

connPlex :: (MonadResource m, MonadUnliftIO m) => (k -> m ConnArgs) -> m (Plex k ConnDevice)
connPlex f = plexNewU (f >=> refNew . connNew)
