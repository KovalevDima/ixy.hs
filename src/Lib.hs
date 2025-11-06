-- |
-- Module      :  Lib
-- Copyright   :  Alex Egger 2018
-- License     :  BSD3
--
-- Maintainer  :  alex.egger96@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--
module Lib
  ( -- * Driver
    newDriver
  , module Lib.Ixgbe
  )
where

import           Lib.Ixgbe
import           Lib.Pci                        ( busDeviceFunction )

import Data.Text (Text)

-- | Initializes a driver for a device.
--
-- Currently only supports IXGBE.
newDriver
  :: Text -- ^ The 'BusDeviceFunction' of the device.
  -> Int -- ^ The number of rx queues to initialize.
  -> Int -- ^ The number of tx queues to initialize.
  -> IO (Maybe Device)
newDriver bdfT numRx numTx = case busDeviceFunction bdfT of
  Just bdf -> do
    !dev <- initDev bdf numRx numTx
    return $ Just dev
  Nothing -> return Nothing
