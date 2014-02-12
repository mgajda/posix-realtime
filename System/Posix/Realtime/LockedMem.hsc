{-# OPTIONS -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Realtime.LockedMem
-- Copyright   :  (c) The University of Glasgow 2002   
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  William N. Halchin (vigalchin@gmail.com)
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX 1003.1b memory locking support.  
--
-----------------------------------------------------------------------------

module System.Posix.Realtime.LockedMem (

   LockAllFlags(..),

   lockMemory,               -- Ptr Word8 -> ByteCount -> IO ()

   unlockMemory,             -- Ptr Word8 -> ByteCount -> IO ()

   lockAllMemory,            -- LockAllFlags -> IO ()

   unlockAllMemory           -- IO ()   

  ) where



import System.IO
import System.IO.Error
import System.Posix.Types
import System.Posix.Error
import System.Posix.Internals

import Foreign
import Foreign.C
import Data.Bits


#ifdef __GLASGOW_HASKELL__
import GHC.IOBase
import GHC.Handle hiding (fdToHandle)
import qualified GHC.Handle
#endif

#ifdef __HUGS__
import Hugs.Prelude (IOException(..), IOErrorType(..))
import qualified Hugs.IO (handleToFd, openFd)
#endif

#include "HsUnix.h"
#include <sys/mman.h>

-- -----------------------------------------------------------------------------


data LockAllFlags = CURRENT | FUTURE


-- | lock a region of memory
lockMemory :: Ptr Word8 -> ByteCount -> IO ()
lockMemory mem len = do
   throwErrnoIfMinus1 "lockMemory" (c_mlock mem len)
   return ()

foreign import ccall unsafe "sys/mman.h mlock"
    c_mlock :: Ptr Word8 -> CSize -> IO CInt


-- | unlock a region of memory
unlockMemory :: Ptr Word8 -> ByteCount -> IO ()
unlockMemory mem len = do
   throwErrnoIfMinus1 "lockMemory" (c_mlock mem len)
   return ()

foreign import ccall unsafe "sys/mman.h mlock"
    c_munlock :: Ptr Word8 -> CSize -> IO CInt


-- | lock all of a prcocess's memory space
lockAllMemory :: LockAllFlags -> IO ()
lockAllMemory flags = do
    throwErrnoIfMinus1 "lockAllMemory" (c_mlockall cflags)
    return ()
  where
    cflags = case flags of
                   CURRENT -> (#const MCL_CURRENT)
                   FUTURE   -> (#const MCL_FUTURE)

foreign import ccall unsafe "sys/mman.h mlockall"
    c_mlockall :: CInt -> IO CInt


-- | unlock all mapped pages of a process!
unlockAllMemory :: IO ()   
unlockAllMemory = do
   throwErrnoIfMinus1 "unlockAllMemory" (c_munlockall)
   return ()

foreign import ccall unsafe "sys/mman.h munlockall"
    c_munlockall :: IO CInt
