module Midriff.Config
  ( PortConfig (..)
  , DeviceConfig (..)
  , Ignores (..)
  , InputConfig (..)
  , PortId (..)
  , unspecDeviceConfig
  , defaultIgnores
  , ignoreAll
  , ignoreNone
  ) where

import Sound.RtMidi (Api (..))

-- | Config for allocating an input or output device.
data DeviceConfig = DeviceConfig
  { dcApi :: !Api
  , dcClient :: !String
  } deriving (Eq, Show)

-- | Effective default device config for when you only want to specify client name.
unspecDeviceConfig :: String -> DeviceConfig
unspecDeviceConfig = DeviceConfig UnspecifiedApi

-- | Identifies the port you want to open. Can be a real port, represented by a port
-- number, or a virtual port.
data PortId =
    PortIdReal !Int
  | PortIdVirtual
  deriving (Eq, Show)

-- | Specifies a named port.
data PortConfig = PortConfig
  { pcName :: !String
  , pcPortId :: !PortId
  } deriving (Eq, Show)

-- | Message type ignores to reduce data rate.
data Ignores = Ignores
  { igSysEx :: !Bool
  , igTime :: !Bool
  , igSense :: !Bool
  } deriving (Eq, Show)

-- | This is effectively the default ignore config
-- in RtMidi if not otherwise specified.
defaultIgnores :: Ignores
defaultIgnores = Ignores False True True

-- | Ignore all additional message types.
ignoreAll :: Ignores
ignoreAll = Ignores True True True

-- | Ignore no additional message types.
ignoreNone :: Ignores
ignoreNone = Ignores False False False

-- | Additional config for input.
data InputConfig = InputConfig
  { icPortConfig :: !PortConfig   -- ^ Which port to read from
  , icCapacity :: !Int            -- ^ Capacity of the input buffer
  , icIgnore :: !(Maybe Ignores)  -- ^ Optional ignores
  }
