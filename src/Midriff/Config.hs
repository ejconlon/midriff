module Midriff.Config
  ( Config (..)
  , DeviceConfig (..)
  , Ignores (..)
  , InputConfig (..)
  , PortId (..)
  , unspecDeviceConfig
  ) where

import Sound.RtMidi (Api (..))

data DeviceConfig = DeviceConfig
  { dcApi :: !Api
  , dcClient :: !String
  } deriving (Eq, Show)

unspecDeviceConfig :: String -> DeviceConfig
unspecDeviceConfig = DeviceConfig UnspecifiedApi

data PortId =
    PortIdReal !Int
  | PortIdVirtual
  deriving (Eq, Show)

data Config = Config
  { cfgName :: !String
  , cfgPortId :: !PortId
  } deriving (Eq, Show)

data Ignores = Ignores
  { igSysEx :: !Bool
  , igTime :: !Bool
  , igSense :: !Bool
  } deriving (Eq, Show)

defaultIgnores :: Ignores
defaultIgnores = Ignores False True True

ignoreAll :: Ignores
ignoreAll = Ignores True True True

ignoreNone :: Ignores
ignoreNone = Ignores False False False

data InputConfig = InputConfig
  { icConfig :: !Config
  , icIgnore :: !(Maybe Ignores)
  }
