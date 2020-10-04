{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}

-- | Many of the codec details are lifted from hmidi (see README for license and attribution).
module Midriff.Msg
  ( Channel (..)
  , Note (..)
  , Velocity (..)
  , ControlNum (..)
  , ControlVal (..)
  , Pressure (..)
  , ProgramNum (..)
  , PitchBend (..)
  , noteOn
  , noteOff
  , ChanVoiceMsg (..)
  , ChanVoiceMsgData (..)
  , MidiMsg (..)
  , MidiEvent (..)
  , MidiParsed (..)
  , decodeEvent
  , decodeMsg
  , encodeMsg
  , isShortFrame
  , isSysexFrame
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import GHC.Generics (Generic)
import Midriff.Time (TimeDelta, timeDeltaFromFracSecs)
import Midriff.Word (Word7 (..))

newtype Channel = Channel { unChannel :: Word7 }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)
  deriving anyclass (NFData, Hashable)

newtype Note = Note { unNote :: Word7 }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)
  deriving anyclass (NFData, Hashable)

newtype Velocity = Velocity { unVelocity :: Word7 }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)
  deriving anyclass (NFData, Hashable)

newtype ControlNum = ControlNum { unControlNum :: Word7 }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)
  deriving anyclass (NFData, Hashable)

newtype ControlVal = ControlVal { unControlVal :: Word7 }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)
  deriving anyclass (NFData, Hashable)

newtype Pressure = Pressure { unPressure :: Word7 }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)
  deriving anyclass (NFData, Hashable)

newtype ProgramNum = ProgramNum { unProgramNum :: Word7 }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)
  deriving anyclass (NFData, Hashable)

newtype PitchBend = PitchBend { unPitchBend :: Word7 }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)
  deriving anyclass (NFData, Hashable)

noteOn :: Channel -> Note -> Velocity -> MidiMsg
noteOn c k v = ParsedMidiMsg (MidiChanVoice (ChanVoiceMsg c (ChanVoiceNoteOnOff k v)))

noteOff :: Channel -> Note -> MidiMsg
noteOff c k = noteOn c k 0

data ChanVoiceMsgData =
    ChanVoiceNoteOnOff !Note !Velocity
  | ChanVoicePolyAftertouch !Note !Pressure
  | ChanVoiceCC !ControlNum !ControlVal
  | ChanVoiceProgramChange !ProgramNum
  | ChanVoiceAftertouch !Pressure
  | ChanVoicePitchWheel !PitchBend
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data ChanVoiceMsg =
  ChanVoiceMsg !Channel !ChanVoiceMsgData
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- TODO(ejconlon) Implement ChannelMode message
-- https://www.midi.org/specifications/item/table-1-summary-of-midi-message
data MidiParsed =
    MidiChanVoice !ChanVoiceMsg
  | MidiSongPosition !Word7
  | MidiSongSelect !Word7
  | MidiTuneRequest
  | MidiSRTClock
  | MidiSRTStart
  | MidiSRTContinue
  | MidiSRTStop
  | MidiActiveSensing
  | MidiReset
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data MidiMsg =
    UnparsedMidiMsg !(VS.Vector Word8)
  | ParsedMidiMsg !MidiParsed
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data MidiEvent =
  MidiEvent !TimeDelta !MidiMsg
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

decodeShortMsgParsed :: ShortMsg -> Maybe MidiParsed
decodeShortMsgParsed (ShortMsg chn msg bs) =
  if msg < 15
    then fmap (MidiChanVoice . ChanVoiceMsg (fromIntegral chn)) (decodeChanVoice msg bs)
    else decodeOther chn bs

decodeChanVoice :: Word7 -> ShortBytes -> Maybe ChanVoiceMsgData
decodeChanVoice msg bs = case (msg, bs) of
   (8,  ShortBytes2 k _) -> Just (ChanVoiceNoteOnOff (coerce k) 0)
   (9,  ShortBytes2 k v) -> Just (ChanVoiceNoteOnOff (coerce k) (coerce v))
   (10, ShortBytes2 k v) -> Just (ChanVoicePolyAftertouch (coerce k) (coerce v))
   (11, ShortBytes2 k v) -> Just (ChanVoiceCC (coerce k) (coerce v))
   (12, ShortBytes1 k)   -> Just (ChanVoiceProgramChange (coerce k))
   (13, ShortBytes1 k)   -> Just (ChanVoiceAftertouch (coerce k))
   (14, ShortBytes2 k v) -> Just (ChanVoicePitchWheel (coerce (k + shiftL v 7 - 8192)))
   _ -> Nothing

decodeOther :: Word7 -> ShortBytes -> Maybe MidiParsed
decodeOther lo bs = case (lo, bs) of
  -- TODO(ejconlon) parse timecode quarter frame (msg 1)
  (2, ShortBytes2 a b) -> Just (MidiSongPosition (a + shiftL b 7))
  (3, ShortBytes1 a)   -> Just (MidiSongSelect a)
  (6, ShortBytes0)     -> Just MidiTuneRequest
  (8, ShortBytes0)     -> Just MidiSRTClock
  (10, ShortBytes0)    -> Just MidiSRTStart
  (11, ShortBytes0)    -> Just MidiSRTContinue
  (12, ShortBytes0)    -> Just MidiSRTStop
  (14, ShortBytes0)    -> Just MidiActiveSensing
  (15, ShortBytes0)    -> Just MidiReset
  _ -> Nothing

encodeShortMsgParsed :: MidiParsed -> ShortMsg
encodeShortMsgParsed (MidiChanVoice (ChanVoiceMsg (coerce -> chn) msg')) =
  case msg' of
    ChanVoiceNoteOnOff k v       -> ShortMsg chn  9 (ShortBytes2 (coerce k) (coerce v))
    ChanVoicePolyAftertouch k v  -> ShortMsg chn 10 (ShortBytes2 (coerce k) (coerce v))
    ChanVoiceCC k v              -> ShortMsg chn 11 (ShortBytes2 (coerce k) (coerce v))
    ChanVoiceProgramChange k     -> ShortMsg chn 12 (ShortBytes1 (coerce k))
    ChanVoiceAftertouch k        -> ShortMsg chn 13 (ShortBytes1 (coerce k))
    ChanVoicePitchWheel n        -> let m = min 16383 (max 0 (coerce n + 8192)) in ShortMsg chn 14 (ShortBytes2 (m .&. 127) (shiftR m 7))

encodeShortMsgParsed (MidiSongPosition p) = ShortMsg 15  3 (ShortBytes2 (p .&. 7) (shiftR p 7))
encodeShortMsgParsed (MidiSongSelect   s) = ShortMsg 15  3 (ShortBytes1 s)
encodeShortMsgParsed  MidiTuneRequest     = ShortMsg 15  6 ShortBytes0
encodeShortMsgParsed  MidiSRTClock        = ShortMsg 15  8 ShortBytes0
encodeShortMsgParsed  MidiSRTStart        = ShortMsg 15 10 ShortBytes0
encodeShortMsgParsed  MidiSRTContinue     = ShortMsg 15 11 ShortBytes0
encodeShortMsgParsed  MidiSRTStop         = ShortMsg 15 12 ShortBytes0
encodeShortMsgParsed  MidiActiveSensing   = ShortMsg 15 14 ShortBytes0
encodeShortMsgParsed  MidiReset           = ShortMsg 15 15 ShortBytes0

data ShortBytes =
    ShortBytes0
  | ShortBytes1 !Word7
  | ShortBytes2 !Word7 !Word7
  deriving stock (Eq, Show)

data ShortMsg = ShortMsg
  { smChannel :: !Word7
  , smMsg     :: !Word7
  , smBytes   :: !ShortBytes
  } deriving stock (Eq, Show)

isShortFrame :: VS.Vector Word8 -> Bool
isShortFrame bytes = not (VS.null bytes) && VS.head bytes /= 0xf0

isSysexFrame :: VS.Vector Word8 -> Bool
isSysexFrame bytes =
  not (VS.null bytes) && VS.head bytes == 0xf0 && VS.last bytes == 0x7f

decodeShortMsg :: VS.Vector Word8 -> Maybe ShortMsg
decodeShortMsg bytes =
  if VS.null bytes
    then Nothing
    else
      let cmd = coerce (VS.head bytes)
          chn = cmd .&. 15
          msg = shiftR cmd 4
      in case VS.length bytes of
          1     -> Just (ShortMsg chn msg ShortBytes0)
          2     -> Just (ShortMsg chn msg (ShortBytes1 (coerce (bytes VS.! 1))))
          3     -> Just (ShortMsg chn msg (ShortBytes2 (coerce (bytes VS.! 1)) (coerce (bytes VS.! 2))))
          _     -> Nothing

encodeShortMsg :: ShortMsg -> VS.Vector Word8
encodeShortMsg (ShortMsg chn msg bs) = VS.fromList bytes where
  cmd = coerce ((chn .&. 15) .|. shiftL msg 4)
  bytes = case bs of
        ShortBytes0 -> [cmd]
        ShortBytes1 a -> [cmd, coerce a]
        ShortBytes2 a b -> [cmd, coerce a, coerce b]

decodeMsg :: VS.Vector Word8 -> MidiMsg
decodeMsg bytes = maybe (UnparsedMidiMsg bytes) ParsedMidiMsg res where
  res
    | isShortFrame bytes = decodeShortMsg bytes >>= decodeShortMsgParsed
    | otherwise = Nothing

encodeMsg :: MidiMsg -> VS.Vector Word8
encodeMsg msg =
  case msg of
    UnparsedMidiMsg bytes -> bytes
    ParsedMidiMsg parsed -> encodeShortMsg (encodeShortMsgParsed parsed)

decodeEvent :: Double -> VS.Vector Word8 -> MidiEvent
decodeEvent fracs bytes = MidiEvent (timeDeltaFromFracSecs fracs) (decodeMsg bytes)
