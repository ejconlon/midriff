module Midriff.Msg
  ( TimeDelta
  , timeDeltaFromFracSecs
  , timeDeltaFromNanos
  , timeDeltaToFracSecs
  , timeDeltaToNanos
  , ChanVoiceMsg (..)
  , MidiMsg (..)
  , MidiEvent (..)
  , decodeMsg
  ) where

import Data.Bits (shiftL, shiftR, (.&.))
import Data.Word (Word8)

-- Time delta in nanoseconds since last event
newtype TimeDelta = TimeDelta { unTimeDelta :: Integer } deriving (Eq, Show, Ord, Num)

timeDeltaFromFracSecs :: RealFrac a => a -> TimeDelta
timeDeltaFromFracSecs d = TimeDelta (round (1000000000 * toRational d))

timeDeltaFromNanos :: Integral a => a -> TimeDelta
timeDeltaFromNanos n = TimeDelta (fromIntegral n)

timeDeltaToFracSecs :: RealFrac a => TimeDelta -> a
timeDeltaToFracSecs (TimeDelta n) = fromIntegral n / 1000000000

timeDeltaToNanos :: TimeDelta -> Integer
timeDeltaToNanos = unTimeDelta

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
data MidiMsg =
    MidiChanVoice !Int !ChanVoiceMsg
  | MidiSysEx ![Word8]  -- contents from original message [0xf0, ...contents, 0xf7]
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

data MidiEvent = MidiEvent !(Either [Word8] MidiMsg) !TimeDelta deriving (Eq, Show)

translateShortMsg :: ShortMsg -> Maybe MidiMsg
translateShortMsg (ShortMsg chn msg bt1 bt2) =
  if msg < 15
    then fmap (MidiChanVoice (fromIntegral chn)) (translateChanVoice msg k v)
    else translateOther chn k v
  where
    k = fromIntegral bt1
    v = fromIntegral bt2

translateChanVoice :: Word8 -> Int -> Int -> Maybe ChanVoiceMsg
translateChanVoice msg k v = case msg of
   8  -> Just (ChanVoiceNoteOn k 0)
   9  -> Just (ChanVoiceNoteOn k v)
   10 -> Just (ChanVoicePolyAftertouch k v)
   11 -> Just (ChanVoiceCC k v)
   12 -> Just (ChanVoiceProgramChange k)
   13 -> Just (ChanVoiceAftertouch k)
   14 -> Just (ChanVoicePitchWheel (k + shiftL v 7 - 8192))
   _ -> Nothing

translateOther :: Word8 -> Int -> Int -> Maybe MidiMsg
translateOther lo a b = case lo of
  2  -> Just (MidiSongPosition (a + shiftL b 7))
  3  -> Just (MidiSongSelect a)
  6  -> Just MidiTuneRequest
  8  -> Just MidiSRTClock
  10 -> Just MidiSRTStart
  11 -> Just MidiSRTContinue
  12 -> Just MidiSRTStop
  14 -> Just MidiActiveSensing
  15 -> Just MidiReset
  _ -> Nothing

untranslateShortMsg :: MidiMsg -> Maybe ShortMsg
untranslateShortMsg (MidiChanVoice chn msg') =
  case msg' of
    ChanVoiceNoteOn  k v         -> Just (shortMsg chn  9 k v)
    ChanVoicePolyAftertouch k v  -> Just (shortMsg chn 10 k v)
    ChanVoiceCC k v              -> Just (shortMsg chn 11 k v)
    ChanVoiceProgramChange k     -> Just (shortMsg chn 12 k 0)
    ChanVoiceAftertouch k        -> Just (shortMsg chn 13 k 0)
    ChanVoicePitchWheel n        -> let m = min 16383 (max 0 (n + 8192)) in Just (shortMsg chn 14 (m .&. 127) (shiftR m 7))

untranslateShortMsg (MidiSongPosition p) = Just (shortMsg 15  3 (p .&. 7) (shiftR p 7))
untranslateShortMsg (MidiSongSelect   s) = Just (shortMsg 15  3 s 0)
untranslateShortMsg  MidiTuneRequest     = Just (shortMsg 15  6 0 0)
untranslateShortMsg  MidiSRTClock        = Just (shortMsg 15  8 0 0)
untranslateShortMsg  MidiSRTStart        = Just (shortMsg 15 10 0 0)
untranslateShortMsg  MidiSRTContinue     = Just (shortMsg 15 11 0 0)
untranslateShortMsg  MidiSRTStop         = Just (shortMsg 15 12 0 0)
untranslateShortMsg  MidiActiveSensing   = Just (shortMsg 15 14 0 0)
untranslateShortMsg  MidiReset           = Just (shortMsg 15 15 0 0)
untranslateShortMsg (MidiSysEx _)        = Nothing

shortMsg :: Int -> Int -> Int -> Int -> ShortMsg
shortMsg chn msg bt1 bt2 =
  ShortMsg (fromIntegral chn) (fromIntegral msg) (fromIntegral bt1) (fromIntegral bt2)

data ShortMsg = ShortMsg
  { smChannel :: !Word8
  , smMsg     :: !Word8
  , smByte1   :: !Word8
  , smByte2   :: !Word8
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
        []    -> Just (ShortMsg chn msg 0 0)
        [a]   -> Just (ShortMsg chn msg a 0)
        [a,b] -> Just (ShortMsg chn msg a b)
        _     -> Nothing

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

decodeMsg :: [Word8] -> Double -> MidiEvent
decodeMsg bytes fracs = MidiEvent (maybe (Left bytes) Right res) (timeDeltaFromFracSecs fracs) where
  res
    | isShortMsg bytes = decodeShortMsg bytes >>= translateShortMsg
    | isSysexMsg bytes = fmap MidiSysEx (decodeSysexMsg bytes)
    | otherwise = Nothing
