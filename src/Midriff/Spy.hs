{-# LANGUAGE MultiWayIf #-}

module Midriff.Spy where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Dahdit.Network (Conn (..), HostPort (..), resolveAddr, runDecoder, runEncoder, udpServerConn)
import Data.Acquire (allocateAcquire)
import Midiot.Osc (Packet)
import Network.Socket qualified as NS

oscSpyLocal :: Int -> Int -> Int -> IO ()
oscSpyLocal inPort spyPort outPort = oscSpy (mkHp inPort) (mkHp spyPort) (mkHp outPort)
 where
  mkHp = HostPort (Just "127.0.0.1")

oscSpy :: HostPort -> HostPort -> HostPort -> IO ()
oscSpy inHost spyHost outHost = runResourceT $ do
  inAddr <- liftIO (resolveAddr inHost)
  outAddr <- liftIO (resolveAddr outHost)
  (_, srvConn) <- allocateAcquire (udpServerConn Nothing spyHost)
  liftIO (oscSpyLoop inAddr outAddr srvConn)

oscSpyLoop :: NS.SockAddr -> NS.SockAddr -> Conn NS.SockAddr -> IO ()
oscSpyLoop inAddr outAddr (Conn dec enc) = go
 where
  go = do
    (recvAddr, res) <- runDecoder dec
    case res of
      Left err -> print err
      Right (msg :: Packet) -> do
        print (recvAddr, msg)
        if
            | recvAddr == inAddr -> runEncoder enc outAddr msg
            | recvAddr == outAddr -> runEncoder enc inAddr msg
            | otherwise -> pure ()
        go
