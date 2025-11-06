{-# LANGUAGE InstanceSigs #-}
-- |
-- Module      :  Ixgbe.Memory
-- Copyright   :  Alex Egger 2018
-- License     :  BSD3
--
-- Maintainer  :  alex.egger96@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
--
--
module Ixgbe.Memory
  ( allocateMem
  , mkMemPool
  , allocateBuf
  , idToPtr
  , freeBuf
  , translate
  , peekId
  , peekAddr
  , peekSize
  , pokeSize
  , MemPool(..)
  , PacketBuf(..)
  , PhysAddr(..)
  , VirtAddr(..)
  )
where

import Data.Array.IO (IOUArray, newListArray, readArray, writeArray)
import Data.Binary.Get (getWord64le, pushChunk, runGetIncremental, Decoder(Fail, Done, Partial))
import Data.ByteString as B (empty, hGet)
import Data.ByteString.Unsafe (unsafePackCStringLen, unsafeUseAsCStringLen)
import Data.IORef (modifyIORef', newIORef, readIORef, IORef)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (WordPtr(..), castPtr, plusPtr, ptrToWordPtr, Ptr)
import Foreign.Storable (sizeOf, alignment, peek, peekByteOff, poke, pokeByteOff, Storable)
import System.Path as Path (absDir, relFile, absFile)
import System.Path.IO as PathIO
import System.Posix.IO (closeFd, handleToFd)
import System.Posix.Memory ( MemoryMapFlag(MemoryMapShared)
                           , MemoryProtection
                             ( MemoryProtectionRead
                             , MemoryProtectionWrite
                             )
                           , memoryLock
                           , memoryMap
                           , sysconfPageSize
                           )
import Data.ByteString (ByteString)
import Data.Word (Word8, Word64)
import Data.Text as T (show)
import Control.Exception (bracket, throwIO)
import Data.Bits (Bits((.&.), shift, shiftR))
import Data.Text.IO as T (putStrLn)

newtype PhysAddr = PhysAddr Word64
newtype VirtAddr a = VirtAddr (Ptr a)

-- $ Huge Pages

hugepageBits :: Int
hugepageBits = 21

hugepageSize :: Int
hugepageSize = shift 1 hugepageBits

-- $ Allocations

allocateMem :: Int -> Bool -> IO (Ptr a)
allocateMem size contiguous = do
  T.putStrLn
    $  "Allocating a memory chunk with size " <> T.show size
    <> "B (contiguous=" <> T.show contiguous <> ")."
  let s = if size `mod` hugepageSize /= 0
        then shift (shiftR size hugepageBits + 1) hugepageBits
        else size
  
  (_, h) <- PathIO.openBinaryTempFile (Path.absDir "/mnt/huge")
                                      (Path.relFile "ixy.huge")
  PathIO.hSetFileSize h $ fromIntegral s
  let f = memoryMap Nothing
                    (fromIntegral s)
                    [MemoryProtectionRead, MemoryProtectionWrite]
                    MemoryMapShared
  ptr <- bracket (handleToFd h) closeFd (\fd -> Just fd `f` 0)
  memoryLock ptr $ fromIntegral s
  -- TODO: We should remove this, but not here.
  -- Dir.removeFile fname
  return ptr

-- $ Memory Pools

data PacketBuf = PacketBuf { pbId :: Int
                           , pbAddr :: PhysAddr
                           , pbSize :: Int
                           , pbData :: ByteString }

instance Storable PacketBuf where
  sizeOf _ = 2048
  alignment = sizeOf
  peek :: Ptr PacketBuf -> IO PacketBuf
  peek ptr = do
    id' <- peek (castPtr ptr)
    addr <- peekByteOff ptr addrOffset
    size <- peekByteOff ptr sizeOffset
    bufData <- unsafePackCStringLen (castPtr (ptr `plusPtr` dataOffset), size)
    return PacketBuf {pbId = id', pbAddr = PhysAddr addr, pbSize = size, pbData = bufData}
  poke ptr buf = do
    poke (castPtr ptr) $ pbId buf
    pokeByteOff ptr addrOffset bufAddr
    pokeByteOff ptr sizeOffset $ pbSize buf
    unsafeUseAsCStringLen (pbData buf) $ uncurry (copyBytes (ptr `plusPtr` dataOffset))
   where PhysAddr bufAddr = pbAddr buf

addrOffset :: Int
addrOffset = sizeOf (0 :: Int)

sizeOffset :: Int
sizeOffset = addrOffset + sizeOf (0 :: Word64)

dataOffset :: Int
dataOffset = sizeOffset + sizeOf (0 :: Int)

data MemPool = MemPool { mpBaseAddr :: Ptr Word8
                       , mpNumEntries :: Int
                       , mpFreeBufs :: IOUArray Int Int
                       , mpTop :: IORef Int
                       }

mkMemPool :: Int -> IO MemPool
mkMemPool numEntries = do
  ptr <- allocateMem (numEntries * bufSize) False
  mapM_ initBuf
        [ (ptr `plusPtr` (i * bufSize), i) | i <- [0 .. numEntries - 1] ]
  freeBufs <- newListArray (0, numEntries - 1) [0 .. numEntries - 1]
  topRef <- newIORef (numEntries :: Int)
  return MemPool
    { mpBaseAddr   = ptr
    , mpNumEntries = numEntries
    , mpFreeBufs   = freeBufs
    , mpTop        = topRef
    }
 where
  initBuf (bufPtr, i) = do
    bufPhysAddr <- translate $ VirtAddr (bufPtr `plusPtr` dataOffset)
    poke
      bufPtr
      PacketBuf {pbId = i, pbAddr = bufPhysAddr, pbSize = 0, pbData = B.empty}
  bufSize = sizeOf (undefined :: PacketBuf)

allocateBuf :: MemPool -> IO (Ptr PacketBuf)
allocateBuf memPool = do
  let topRef = mpTop memPool
  modifyIORef' topRef (\i -> i - 1)
  top <- readIORef topRef
  id'  <- readArray (mpFreeBufs memPool) top
  return $ idToPtr memPool id'

idToPtr :: MemPool -> Int -> Ptr PacketBuf
idToPtr memPool id' =
  (mpBaseAddr memPool) `plusPtr` (id' * sizeOf (undefined :: PacketBuf))

peekId :: Ptr PacketBuf -> IO Int
peekId ptr = peek (castPtr ptr)

peekAddr :: Ptr PacketBuf -> IO PhysAddr
peekAddr ptr = PhysAddr <$> peekByteOff ptr addrOffset

peekSize :: Ptr PacketBuf -> IO Int
peekSize ptr = peekByteOff ptr sizeOffset

pokeSize :: Ptr PacketBuf -> Int -> IO ()
pokeSize ptr = pokeByteOff ptr sizeOffset

freeBuf :: MemPool -> Int -> IO ()
freeBuf memPool id' = do
  let topRef = mpTop memPool
  top <- readIORef topRef
  writeArray (mpFreeBufs memPool) top id'
  modifyIORef' topRef (+ 1)

-- $ Utility

translate :: VirtAddr a -> IO PhysAddr
translate (VirtAddr virt) = PathIO.withBinaryFile path PathIO.ReadMode inner
 where
  inner h = do
    PathIO.hSeek h PathIO.AbsoluteSeek $ fromIntegral offset
    buf <- B.hGet h 8
    case runGetIncremental getWord64le `pushChunk` buf of
      Done _ _ b -> return $ PhysAddr $ getAddr $ fromIntegral b
      Partial _ ->
        throwIO $ userError "Partial input when parsing physical address."
      Fail{} -> throwIO $ userError "Physical address was malformed."
  path         = Path.absFile "/proc/self/pagemap"
  WordPtr addr = ptrToWordPtr virt
  offset       = (addr `quot` pageSize) * 8 -- This is not arch-specific, hence the magic number.
  getAddr x =
    fromIntegral $ (x .&. 0x7fffffffffffff) * pageSize + addr `mod` pageSize
  pageSize = fromIntegral sysconfPageSize
