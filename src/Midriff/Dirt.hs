{-# LANGUAGE OverloadedStrings #-}

module Midriff.Dirt where

import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.State.Strict (MonadState (..), StateT, evalStateT)
import Dahdit (ShortByteString)
import Data.Int (Int32, Int64)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Midiot.Osc (Datum (..), DatumType (..), Msg (..), Packet (..), PortMsg, datumType)
import Midiot.OscAddr (RawAddrPat)
import Midiot.Time (NtpTime)
import Optics (Prism', preview, prism', review)

data S = SPacket !Packet | SMsg !Msg | SArgs !(Seq Datum)

data Err
  = ErrState
  | ErrNotMsg
  | ErrAddrMismatch !RawAddrPat !RawAddrPat
  | ErrEmptyArgs
  | ErrDatumMismatch !(Maybe Datum) !Datum
  | ErrDatumTypeMismatch !(Maybe DatumType) !DatumType
  | ErrDatumLeftoverArgs !Int
  deriving stock (Eq, Ord, Show)

newtype M a = M {unM :: StateT S (Except Err) a}
  deriving newtype (Functor, Applicative, Monad)

parse :: M a -> Packet -> Either Err a
parse m p = runExcept (evalStateT (unM m) (SPacket p))

expectMsg :: M ()
expectMsg = do
  st <- M get
  case st of
    SPacket pack ->
      case pack of
        PacketMsg msg -> M (put (SMsg msg))
        _ -> M (throwError ErrNotMsg)
    _ -> M (throwError ErrState)

expectAddr :: RawAddrPat -> M ()
expectAddr wantAddr = do
  st <- M get
  case st of
    SMsg (Msg actualAddr args) -> do
      if actualAddr == wantAddr
        then M (put (SArgs args))
        else M (throwError (ErrAddrMismatch actualAddr wantAddr))
    _ -> M (throwError ErrState)

expectExactDatum :: Datum -> M ()
expectExactDatum wantDat = do
  st <- M get
  case st of
    SArgs args ->
      case args of
        actualDat :<| rest ->
          if actualDat == wantDat
            then M (put (SArgs rest))
            else M (throwError (ErrDatumMismatch (Just actualDat) wantDat))
        Empty -> M (throwError (ErrDatumMismatch Nothing wantDat))
    _ -> M (throwError ErrState)

data DatumReader a = DatumReader
  { drType :: !DatumType
  , drPrism :: !(Prism' Datum a)
  }

drInt32 :: DatumReader Int32
drInt32 =
  DatumReader DatumTypeInt32 $
    prism' DatumInt32 (\case DatumInt32 x -> Just x; _ -> Nothing)

drInt64 :: DatumReader Int64
drInt64 =
  DatumReader DatumTypeInt64 $
    prism' DatumInt64 (\case DatumInt64 x -> Just x; _ -> Nothing)

drFloat :: DatumReader Float
drFloat =
  DatumReader DatumTypeFloat $
    prism' DatumFloat (\case DatumFloat x -> Just x; _ -> Nothing)

drDouble :: DatumReader Double
drDouble =
  DatumReader DatumTypeDouble $
    prism' DatumDouble (\case DatumDouble x -> Just x; _ -> Nothing)

drString :: DatumReader Text
drString =
  DatumReader DatumTypeString $
    prism' DatumString (\case DatumString x -> Just x; _ -> Nothing)

drBlob :: DatumReader ShortByteString
drBlob =
  DatumReader DatumTypeBlob $
    prism' DatumBlob (\case DatumBlob x -> Just x; _ -> Nothing)

drTime :: DatumReader NtpTime
drTime =
  DatumReader DatumTypeTime $
    prism' DatumTime (\case DatumTime x -> Just x; _ -> Nothing)

drMidi :: DatumReader PortMsg
drMidi =
  DatumReader DatumTypeMidi $
    prism' DatumMidi (\case DatumMidi x -> Just x; _ -> Nothing)

drGet :: DatumReader a -> Datum -> Either DatumType a
drGet (DatumReader _ pr) dat =
  case preview pr dat of
    Just val -> Right val
    Nothing -> Left (datumType dat)

drPut :: DatumReader a -> a -> Datum
drPut (DatumReader _ pr) = review pr

expectReadDatum :: DatumReader a -> M a
expectReadDatum dr = do
  st <- M get
  case st of
    SArgs args ->
      case args of
        actualDat :<| rest ->
          case drGet dr actualDat of
            Left actualTy -> M (throwError (ErrDatumTypeMismatch (Just actualTy) (drType dr)))
            Right a -> M (put (SArgs rest)) >> pure a
        Empty -> M (throwError (ErrDatumTypeMismatch Nothing (drType dr)))
    _ -> M (throwError ErrState)

expectWhileArgs :: M a -> M (Seq a)
expectWhileArgs act = go Empty
 where
  go !acc = do
    st <- M get
    case st of
      SArgs args ->
        case args of
          Empty -> pure acc
          _ -> do
            a <- act
            st' <- M get
            case st' of
              SArgs args' | Seq.length args' < Seq.length args -> pure ()
              _ -> M (throwError ErrState)
            go (acc :|> a)
      _ -> M (throwError ErrState)

expectEndArgs :: M ()
expectEndArgs = do
  st <- M get
  case st of
    SArgs args ->
      case args of
        Empty -> pure ()
        _ -> M (throwError (ErrDatumLeftoverArgs (Seq.length args)))
    _ -> M (throwError ErrState)

data Serde a = Serde
  { serdeTo :: !(a -> Packet)
  , serdeFrom :: !(Packet -> Either Err a)
  }

data Handshake = Handshake
  deriving stock (Eq, Ord, Show)

handshakeS :: Serde Handshake
handshakeS = Serde to from
 where
  to _ = PacketMsg (Msg "/dirt/handshake" Empty)
  from = parse $ do
    expectMsg
    expectAddr "/dirt/handshake"
    expectEndArgs
    pure Handshake

data HandshakeReply = HandshakeReply
  { hrServerHostname :: !Text
  , hrServerPort :: !Int32
  , hrControlBusIndices :: !(Seq Int32)
  }
  deriving stock (Eq, Ord, Show)

handshakeReplyS :: Serde HandshakeReply
handshakeReplyS = Serde to from
 where
  to (HandshakeReply host port idxs) = PacketMsg (Msg "/dirt/handshake/reply" args)
   where
    prefix =
      Seq.fromList
        [ DatumString "&serverHostname"
        , DatumString host
        , DatumString "&serverPort"
        , DatumInt32 port
        , DatumString "&controlBusIndices"
        ]
    args = prefix <> fmap DatumInt32 idxs
  from = parse $ do
    expectMsg
    expectAddr "/dirt/handshake/reply"
    expectExactDatum (DatumString "&serverHostname")
    host <- expectReadDatum drString
    expectExactDatum (DatumString "&serverPort")
    port <- expectReadDatum drInt32
    expectExactDatum (DatumString "&controlBusIndices")
    idxs <- expectWhileArgs (expectReadDatum drInt32)
    pure (HandshakeReply host port idxs)
