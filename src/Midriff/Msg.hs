-- | Many of the codec details are lifted from hmidi (see README for license and attribution).
module Midriff.Msg
  ( ChanVoiceMsg (..)
  , BasicMidiMsg (..)
  , MidiMsg (..)
  , MidiEvent (..)
  , MidiParsed (..)
  , decodeEvent
  , decodeParsed
  , encodeParsed
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word8)
import Midriff.Time (TimeDelta, timeDeltaFromFracSecs)

data ChanVoiceMsg =
    ChanVoiceNoteOn !Int !Int
  | ChanVoicePolyAftertouch !Int !Int
  | ChanVoiceCC !Int !Int
  | ChanVoiceProgramChange !Int
  | ChanVoiceAftertouch !Int
  | ChanVoicePitchWheel !Int
  deriving (Eq, Show)

-- TODO(ejconlon) Implement ChannelMode message
-- https://www.midi.org/specifications/item/table-1-summary-of-midi-message
data BasicMidiMsg =
    MidiChanVoice !Int !ChanVoiceMsg
  | MidiSongPosition !Int
  | MidiSongSelect !Int
  | MidiTuneRequest
  | MidiSRTClock
  | MidiSRTStart
  | MidiSRTContinue
  | MidiSRTStop
  | MidiActiveSensing
  | MidiReset
  deriving (Eq, Show)

data MidiMsg =
    BasicMsg !BasicMidiMsg
  | SysExMsg ![Word8]
  deriving (Eq, Show)

newtype MidiParsed = MidiParsed { unMidiParsed :: Either [Word8] MidiMsg } deriving (Eq, Show)

data MidiEvent = MidiEvent !TimeDelta !MidiParsed deriving (Eq, Show)

decodeShortMsgBasic :: ShortMsg -> Maybe BasicMidiMsg
decodeShortMsgBasic (ShortMsg chn msg bs) =
  let bs' = convertShortBytes bs
  in if msg < 15
    then fmap (MidiChanVoice (fromIntegral chn)) (decodeChanVoice msg bs')
    else decodeOther chn bs'

decodeChanVoice :: Word8 -> ShortBytes Int -> Maybe ChanVoiceMsg
decodeChanVoice msg bs = case (msg, bs) of
   (8,  ShortBytes2 k _) -> Just (ChanVoiceNoteOn k 0)
   (9,  ShortBytes2 k v) -> Just (ChanVoiceNoteOn k v)
   (10, ShortBytes2 k v) -> Just (ChanVoicePolyAftertouch k v)
   (11, ShortBytes2 k v) -> Just (ChanVoiceCC k v)
   (12, ShortBytes1 k)   -> Just (ChanVoiceProgramChange k)
   (13, ShortBytes1 k)   -> Just (ChanVoiceAftertouch k)
   (14, ShortBytes2 k v) -> Just (ChanVoicePitchWheel (k + shiftL v 7 - 8192))
   _ -> Nothing

decodeOther :: Word8 -> ShortBytes Int -> Maybe BasicMidiMsg
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

encodeShortMsgBasic :: BasicMidiMsg -> ShortMsg
encodeShortMsgBasic (MidiChanVoice chn msg') =
  case msg' of
    ChanVoiceNoteOn  k v         -> mkShortMsg chn  9 (ShortBytes2 k v)
    ChanVoicePolyAftertouch k v  -> mkShortMsg chn 10 (ShortBytes2 k v)
    ChanVoiceCC k v              -> mkShortMsg chn 11 (ShortBytes2 k v)
    ChanVoiceProgramChange k     -> mkShortMsg chn 12 (ShortBytes1 k)
    ChanVoiceAftertouch k        -> mkShortMsg chn 13 (ShortBytes1 k)
    ChanVoicePitchWheel n        -> let m = min 16383 (max 0 (n + 8192)) in mkShortMsg chn 14 (ShortBytes2 (m .&. 127) (shiftR m 7))

encodeShortMsgBasic (MidiSongPosition p) = mkShortMsg 15  3 (ShortBytes2 (p .&. 7) (shiftR p 7))
encodeShortMsgBasic (MidiSongSelect   s) = mkShortMsg 15  3 (ShortBytes1 s)
encodeShortMsgBasic  MidiTuneRequest     = mkShortMsg 15  6 ShortBytes0
encodeShortMsgBasic  MidiSRTClock        = mkShortMsg 15  8 ShortBytes0
encodeShortMsgBasic  MidiSRTStart        = mkShortMsg 15 10 ShortBytes0
encodeShortMsgBasic  MidiSRTContinue     = mkShortMsg 15 11 ShortBytes0
encodeShortMsgBasic  MidiSRTStop         = mkShortMsg 15 12 ShortBytes0
encodeShortMsgBasic  MidiActiveSensing   = mkShortMsg 15 14 ShortBytes0
encodeShortMsgBasic  MidiReset           = mkShortMsg 15 15 ShortBytes0

mkShortMsg :: Int -> Int -> ShortBytes Int -> ShortMsg
mkShortMsg chn msg bs =
  ShortMsg (fromIntegral chn) (fromIntegral msg) (convertShortBytes bs)

data ShortBytes a =
    ShortBytes0
  | ShortBytes1 !a
  | ShortBytes2 !a !a
  deriving (Eq, Show)

convertShortBytes :: (Integral a, Num b) => ShortBytes a -> ShortBytes b
convertShortBytes bs =
  case bs of
    ShortBytes0 -> ShortBytes0
    ShortBytes1 x -> ShortBytes1 (fromIntegral x)
    ShortBytes2 x y -> ShortBytes2 (fromIntegral x) (fromIntegral y)

data ShortMsg = ShortMsg
  { smChannel :: !Word8
  , smMsg     :: !Word8
  , smBytes   :: !(ShortBytes Word8)
  } deriving (Eq, Show)

isShortMsg :: [Word8] -> Bool
isShortMsg bytes =
  case bytes of
    [] -> False
    cmd:_ -> cmd /= 0xf0

decodeShortMsg :: [Word8] -> Maybe ShortMsg
decodeShortMsg bytes =
  case bytes of
    [] -> Nothing
    cmd:rest ->
      let chn = cmd .&. 15
          msg = shiftR cmd 4
      in case rest of
        []    -> Just (ShortMsg chn msg ShortBytes0)
        [a]   -> Just (ShortMsg chn msg (ShortBytes1 a))
        [a,b] -> Just (ShortMsg chn msg (ShortBytes2 a b))
        _     -> Nothing

encodeShortMsg :: ShortMsg -> [Word8]
encodeShortMsg (ShortMsg chn msg bs) =
  let cmd = (chn .&. 15) .|. shiftL msg 4
  in case bs of
    ShortBytes0 -> [cmd]
    ShortBytes1 a -> [cmd, a]
    ShortBytes2 a b -> [cmd, a, b]

isSysexMsg :: [Word8] -> Bool
isSysexMsg bytes =
  case bytes of
    [] -> False
    cmd:_ -> cmd == 0xf0

endingWith :: Eq a => a -> [a] -> Maybe [a]
endingWith a = go [] where
  go acc rest =
    case rest of
      [] -> Nothing
      [x] -> if x == a then Just (reverse acc) else Nothing
      x:xs -> go (x:acc) xs

decodeSysexMsg :: [Word8] -> Maybe [Word8]
decodeSysexMsg bytes =
  case bytes of
    0xf0:rest -> endingWith 0xf7 rest
    _ -> Nothing

decodeParsed :: [Word8] -> MidiParsed
decodeParsed bytes = MidiParsed (maybe (Left bytes) Right res) where
  res
    | isShortMsg bytes = decodeShortMsg bytes >>= fmap BasicMsg . decodeShortMsgBasic
    | isSysexMsg bytes = fmap SysExMsg (decodeSysexMsg bytes)
    | otherwise = Nothing

encodeParsed :: MidiParsed -> [Word8]
encodeParsed (MidiParsed mp) =
  case mp of
    Left bytes -> bytes
    Right msg ->
      case msg of
        BasicMsg basic -> encodeShortMsg (encodeShortMsgBasic basic)
        _ -> undefined -- TODO

decodeEvent :: Double -> [Word8] -> MidiEvent
decodeEvent fracs bytes = MidiEvent (timeDeltaFromFracSecs fracs) (decodeParsed bytes)
