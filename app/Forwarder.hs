module Main (main) where

import Ixgbe (Stats(stRxPkts), Device, receive, send, stats, memPoolOf, newDriver)

import Control.Monad (when, forever)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.Text as T (pack, show)
import Foreign.Storable (peekByteOff, pokeByteOff)
import System.Clock (Clock(Monotonic), TimeSpec(..), getTime, diffTimeSpec)
import Data.Text.IO as T (putStrLn)
import Data.Text (Text)
import Data.Word (Word8)
import System.Environment (getArgs)
import Data.Bits ((.&.))

main :: IO ()
main = do
  args <- getArgs
  let bdfT1 = T.pack $ args !! 0
      bdfT2 = T.pack $ args !! 1
      batchSize = read $ (args !! 2) :: Int
  run bdfT1 bdfT2 batchSize

run :: Text -> Text -> Int -> IO ()
run bdfT1 bdfT2 batchSize = do
  dev1    <- fromJust <$> newDriver bdfT1 1 1
  dev2    <- fromJust <$> newDriver bdfT2 1 1
  counter <- newIORef (0 :: Int)
  loop counter dev1 dev2 batchSize

loop :: IORef Int -> Device -> Device -> Int -> IO ()
loop counter dev1 dev2 batchSize = do
  timeRef <- newIORef (TimeSpec {sec = 0, nsec = 0})
  forever $ do
    forward dev1 dev2 batchSize
    forward dev2 dev1 batchSize
    !c <- readIORef counter
    when
      (c .&. 0xF == 0)
      (do
        !t          <- getTime Monotonic
        !beforeTime <- readIORef timeRef
        let diffTime = diffTimeSpec t beforeTime
        when
          (sec diffTime >= 1)
          (do
            !st1 <- stats dev1
            !st2 <- stats dev2
            let mult =
                  fromIntegral (sec diffTime)
                    + (fromIntegral (nsec diffTime) / 1.0e9) :: Float
                divisor = 1000000 * mult
                rxStats st = T.show (fromIntegral (stRxPkts st) / divisor)
                txStats st = T.show (fromIntegral (stRxPkts st) / divisor)
            T.putStrLn $ "Driver 1 -> RX: " <> rxStats st1 <> "Mpps | TX: " <> txStats st1 <> "Mpps"
            T.putStrLn $ "Driver 2 -> RX: " <> rxStats st2 <> "Mpps | TX: " <> rxStats st2 <> "Mpps"
            writeIORef timeRef t
          )
      )
    modifyIORef' counter (+ 1)

forward :: Device -> Device -> Int -> IO ()
forward rxDev txDev batchSize = do
  !pkts <- receive rxDev 0 batchSize
  mapM_ touchPacket pkts
  send txDev 0 (memPoolOf rxDev 0) (reverse pkts)
 where
  touchPacket ptr =
    pokeByteOff ptr 24 =<< (+ 1) <$> (peekByteOff ptr 24 :: IO Word8)
