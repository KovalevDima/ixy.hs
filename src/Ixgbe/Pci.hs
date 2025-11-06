-- |
-- Module      :  Ixgbe.Pci
-- Copyright   :  Alex Egger 2018
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :  unknown
--
-- Provides utility function to work with PCI devices using sysfs.
module Ixgbe.Pci
  ( -- * BusDeviceFunction
    BusDeviceFunction(unBusDeviceFunction)
  , busDeviceFunction
    -- * Operations
  , mapResource
  )
where


import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import           System.IO.Error                ( isDoesNotExistError )
import           System.Path                    ( (</>) )
import qualified System.Path                   as Path
import qualified System.Path.IO                as PathIO
import           System.Posix.IO                ( handleToFd )
import           System.Posix.Memory            ( MemoryMapFlag(MemoryMapShared)
                                                , MemoryProtection
                                                  ( MemoryProtectionRead
                                                  , MemoryProtectionWrite
                                                  )
                                                , memoryMap
                                                )
import Data.Text
import Foreign (Ptr)
import Data.Bits
import Data.Char
import Data.Text.IO as T (putStrLn)
import Control.Exception

isPCI :: T.Text -> Bool
isPCI txt =
  case T.splitOn ":" txt of
    [p1, p2, rest] ->
      T.length p1 == 4 && T.all isHexDigit p1 &&
      T.length p2 == 2 && T.all isHexDigit p2 &&
      case T.breakOn "." rest of
        (p3, dotAndLast) ->
          case T.uncons dotAndLast of
            Just ('.', lastPart) ->
              T.length p3 == 2 && T.all isHexDigit p3 &&
              T.length lastPart == 1 && T.all isDigit lastPart
            _ -> False
    _ -> False


-- | A set of Bus, Device, and Function identifiers that identify a PCI device.
--
-- The identifier follows the extended BDF form: XXXX:BB:DD.F
--
-- where X = PCI Domain Number
--
--       B = PCI Bus Number
--
--       D = PCI Device Number
--
--       F = PCI Function Number
newtype BusDeviceFunction = BDF
  { unBusDeviceFunction :: Text
  } deriving (Show)

-- | Creates a 'BusDeviceFunction' when supplied with an appropiately formed 'Text', or return
-- 'Nothing' if the argument was malformed.
--
-- To see the formatting requirements see 'BusDeviceFunction'.
busDeviceFunction :: Text -> Maybe BusDeviceFunction
busDeviceFunction bdfText
  | isPCI bdfText
  = Just (BDF bdfText)
busDeviceFunction _ = Nothing

base :: Path.AbsDir
base = Path.absDir "/sys/bus/pci/devices/"

-- $ Operations

-- | Map a resource of a PCI device into memory.
mapResource
  :: BusDeviceFunction -- ^ The 'BusDeviceFunction' of the device the resource that will be mapped belongs to.
  -> Text -- ^ The filename of the resource that will be mapped.
  -> IO (Ptr a) -- ^ A 'Ptr' to the beginning of the mapped resource.
mapResource bdf resource =
  let path =
        base
          </> Path.relPath (T.unpack $ unBusDeviceFunction bdf)
          </> Path.relFile (T.unpack resource)
  in  do
        T.putStrLn
          $  "Mapping resource \'"
          <> resource
          <> "\' for device "
          <> unBusDeviceFunction bdf
          <> "."
        unbind bdf
        enableDMA bdf
        PathIO.withBinaryFile path PathIO.ReadWriteMode inner
 where
  inner h = do
    size <- PathIO.hFileSize h
    fd   <- handleToFd h
    memoryMap Nothing
              (fromIntegral size)
              [MemoryProtectionRead, MemoryProtectionWrite]
              MemoryMapShared
              (Just fd)
              0

-- $Internal
-- | Enable DMA for a PCI device.
enableDMA
  :: BusDeviceFunction -- ^ The 'BusDeviceFunction' of the device for which DMA will be enabled.
  -> IO ()
enableDMA bdf =
  let path =
        base
          </> Path.relPath (T.unpack $ unBusDeviceFunction bdf)
          </> Path.relFile "config"
  in  inner path
 where
  inner path = do
    T.putStrLn $ "Enabling DMA for device " <> unBusDeviceFunction bdf <> "."
    PathIO.withBinaryFile
      path
      PathIO.ReadWriteMode
      (\h -> do
        PathIO.hSeek h PathIO.AbsoluteSeek cmdRegOffset
        value <- B.hGet h 2
        PathIO.hSeek h PathIO.AbsoluteSeek cmdRegOffset
        B.hPut h $ setDMA value
      )
  cmdRegOffset         = 4
  busMasterEnableIndex = 2
  setDMA b = (B.head b .|. shift 1 busMasterEnableIndex) `B.cons` B.tail b

-- | Unbind a PCI device from its driver.
unbind
  :: BusDeviceFunction -- ^ The 'BusDeviceFunction' of the device that will be unbound.
  -> IO ()
unbind bdf =
  let path =
        base
          </> Path.relPath (T.unpack $ unBusDeviceFunction bdf)
          </> Path.filePath "driver/unbind"
  in  catch (inner path) handler
 where
  inner path = do
    T.putStrLn
      $  "Unbinding driver for device "
      <> unBusDeviceFunction bdf
      <> "."
    PathIO.writeFile path $ T.unpack $ unBusDeviceFunction bdf
  handler e | isDoesNotExistError e = T.putStrLn "Device was already unbound."
  handler e                         = throwIO e
