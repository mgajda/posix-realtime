{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Realtime.Aio
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  William N. Halchin (vigalchin@gmail.com)
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX 1003.1b  POSIX Asynchronous I\/O.  See
-- <http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/aio.h.html>.
--
-----------------------------------------------------------------------------

module System.Posix.Realtime.Aio (
  AIOCB,
  makeAIOCB,
  aioRead,
  aioWrite,
  aioReturn,
  aioError,
  aioCancel,
  SyncOp(..),
  aioFsync,
  aioSuspend,
  ListIOMode,
  lioListIO,
  ) where


import System.IO
import System.IO.Error
import System.Posix.Realtime.RTDataTypes
import System.Posix.Types
import System.Posix.Error
import System.Posix.Internals

import Foreign
import Foreign.C
import Data.Bits

#ifdef __GLASGOW_HASKELL__
import GHC.IO
import GHC.IO.Handle hiding (fdToHandle)
import qualified GHC.IO.Handle
#endif

#ifdef __HUGS__
import Hugs.Prelude (IOException(..), IOErrorType(..))
import qualified Hugs.IO (handleToFd, openFd)
#endif

#include "HsUnix.h"
#include <aio.h>


data AIOCBStruct

type AIOCB = ForeignPtr AIOCBStruct


-- | Posix AIO return
aioRead :: AIOCB -> IO ()
aioRead p_aiocb = do
  withForeignPtr p_aiocb $ \ p_aiocb -> do
    throwErrnoIfMinus1 "aioRead" (c_aio_read  p_aiocb)
    return ()

foreign import ccall safe "aio.h aio_read"
  c_aio_read :: Ptr AIOCBStruct -> IO CInt


-- | Posix AIO write
aioWrite :: AIOCB -> IO ()
aioWrite p_aiocb = do
  withForeignPtr p_aiocb $ \ p_aiocb -> do
    throwErrnoIfMinus1 "aioWrite" (c_aio_write  p_aiocb)
    return ()

foreign import ccall safe "aio.h aio_write"
  c_aio_write :: Ptr AIOCBStruct -> IO CInt


-- | Posix AIO return
aioReturn :: AIOCB -> IO ByteCount
aioReturn p_aiocb = do
  withForeignPtr p_aiocb $ \ p_aiocb -> do
    count <- (c_aio_return  p_aiocb)
    return (fromIntegral count)

foreign import ccall safe "aio.h aio_return"
  c_aio_return :: Ptr AIOCBStruct -> IO CInt


-- | Posix AIO error
aioError :: AIOCB -> IO Errno
aioError p_aiocb = do
  withForeignPtr p_aiocb $ \ p_aiocb -> do
    errno <- (c_aio_error  p_aiocb)
    return (Errno errno)

foreign import ccall safe "aio.h aio_error"
  c_aio_error :: Ptr AIOCBStruct -> IO CInt


-- | Posix AIO cancel
aioCancel :: Fd -> AIOCB -> IO ()
aioCancel (Fd fd) p_aiocb = do
  withForeignPtr p_aiocb $ \ p_aiocb -> do
    throwErrnoIfMinus1 "aioCancel" (c_aio_cancel fd p_aiocb)
    return ()

foreign import ccall safe "aio.h aio_cancel"
  c_aio_cancel :: CInt -> Ptr AIOCBStruct -> IO CInt


-- | Posix asynchronous file synchronization!
data SyncOp = DSync | Sync

aioFsync :: SyncOp -> AIOCB -> IO ()
aioFsync DSync p_aiocb = do
  withForeignPtr p_aiocb $ \ p_aiocb -> do
    throwErrnoIfMinus1 "aioFsync" (c_aio_fsync (#const O_DSYNC) p_aiocb)
    return ()

aioFsync Sync p_aiocb = do
  withForeignPtr p_aiocb $ \ p_aiocb -> do
    throwErrnoIfMinus1 "aioFsync" (c_aio_fsync (#const O_SYNC) p_aiocb)
    return ()

foreign import ccall safe "aio.h aio_fsync"
  c_aio_fsync :: CInt -> Ptr AIOCBStruct -> IO CInt


-- | Posix AIO lio_listio
type ListIOMode = Int -- <<< need to have mapping from Haskell datatype->Int?

lioListIO :: ListIOMode -> [AIOCB] -> Sigevent -> IO ()
lioListIO mode [] sigEvent = return ()

lioListIO mode aiocbs sigEvent = do
  let numAiocbs = length aiocbs
  p_aiocbs <- mapM foreignPtrToPtr aiocbs
  p_p_aiocbs <- newArray p_aiocbs
  allocaBytes (#const sizeof(struct sigevent)) $ \ p_sigevent -> do
    poke p_sigevent sigEvent
    throwErrnoIfMinus1 "lioListIO" (c_lio_listio (fromIntegral mode) p_p_aiocbs (fromIntegral numAiocbs) p_sigevent)
    return ()

foreign import ccall safe "aio.h lio_listio"
  c_lio_listio :: CInt -> Ptr (Ptr AIOCBStruct) -> CInt -> Ptr Sigevent -> IO CInt


-- | Posix AIO suspend
aioSuspend :: [AIOCB] -> TimeSpec -> IO ()
aioSuspend [] timeSpec = return ()

aioSuspend aiocbs timeSpec = do
  let numAiocbs = length aiocbs
  p_aiocbs <- mapM foreignPtrToPtr aiocbs
  p_p_aiocbs <- newArray p_aiocbs
  allocaBytes (#const sizeof(struct timespec)) $ \ p_timespec -> do
    poke p_timespec timeSpec
    throwErrnoIfMinus1 "aioSuspend" (c_aio_suspend p_p_aiocbs (fromIntegral numAiocbs) p_timespec)
    return ()

foreign import ccall safe "aio.h aio_suspend"
  c_aio_suspend :: Ptr (Ptr AIOCBStruct) -> CInt -> Ptr TimeSpec -> IO CInt


-- | a helper function that builds an 'AIOCB' from all its fields
makeAIOCB :: Fd -> Int -> Int -> FileOffset -> Ptr Word8 -> ByteCount -> Sigevent -> IO AIOCB
makeAIOCB fd lioOpcode reqPrio fileOffset buffer byteCount sigEvent = do
  fptr <- mallocForeignPtrBytes (#const sizeof (struct aiocb))
  withForeignPtr fptr $ \ptr -> do
    (#poke struct aiocb, aio_fildes) ptr fd
    (#poke struct aiocb, aio_lio_opcode) ptr lioOpcode
    (#poke struct aiocb, aio_reqprio) ptr reqPrio
    (#poke struct aiocb, aio_offset) ptr fileOffset
    (#poke struct aiocb, aio_buf) ptr buffer
    (#poke struct aiocb, aio_nbytes) ptr byteCount
    (#poke struct aiocb, aio_sigevent) ptr sigEvent
    return (fptr)


foreignPtrToPtr :: ForeignPtr AIOCBStruct -> IO (Ptr AIOCBStruct)
foreignPtrToPtr  fptr = do
  withForeignPtr fptr return
