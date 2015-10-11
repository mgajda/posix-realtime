{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Realtime.RTDataTypes
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  William N. Halchin (vigalchin@gmail.com)
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX 1003.1b  real-time data types used by multiple Haskell modules
--
-----------------------------------------------------------------------------

module System.Posix.Realtime.RTDataTypes (
  Sigval(..),
  Sigevent(..),
  TimeSpec(..),
  ItimerSpec(..)
  ) where

import System.IO
import System.IO.Error
import System.Posix.Signals
import System.Posix.Types
import System.Posix.Error
import System.Posix.Internals

import Foreign
import Foreign.C
import Data.Bits
import Data.Word

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
#include <time.h>
#include <signal.h>
#include <aio.h>


-- data Sigval = SivalInt Int | SivalPtr (Ptr Char) -- TBD!!!!!!!!!!
newtype Sigval = SivalInt Int  -- TEMP
  deriving(Show)

instance Storable Sigval where
  sizeOf _ = (#const sizeof (sigval_t))
  alignment _ = 1
  poke p_Sigval (SivalInt i) = do
    (#poke sigval_t, sival_int) p_Sigval i
  peek p_Sigval = do
    sigvalInt <- (#peek sigval_t, sival_int) p_Sigval
    return (SivalInt sigvalInt)   -- TBD ?????

data Sigevent = Sigevent {
  sigevVal :: Sigval,
  sigevSigno :: Signal,
  sigevNotify :: Int,
  sigevFunction :: FunPtr (Sigval -> IO ()),
  sigevAttribute :: Ptr Char
  } deriving(Show)

instance Storable Sigevent where
  sizeOf _ = (#const sizeof (struct sigevent))
  alignment _ = 1
  poke p_Sigevent (Sigevent sigevVal sigevSigno sigevNotify sigevFunction sigevAttribute) = do
    (#poke struct sigevent, sigev_value) p_Sigevent sigevVal
    (#poke struct sigevent, sigev_signo) p_Sigevent sigevSigno
    (#poke struct sigevent, sigev_notify) p_Sigevent sigevNotify
    (#poke struct sigevent, _sigev_un._sigev_thread._function) p_Sigevent sigevFunction
    (#poke struct sigevent, _sigev_un._sigev_thread._attribute) p_Sigevent sigevAttribute
  peek p_Sigevent = do
    sigevVal <- (#peek struct sigevent, sigev_value) p_Sigevent
    sigevSigno <- (#peek struct sigevent, sigev_signo) p_Sigevent
    sigevNotify <- (#peek struct sigevent, sigev_notify) p_Sigevent
    sigevFunction <- (#peek struct sigevent, _sigev_un._sigev_thread._function) p_Sigevent
    sigevAttribute <- (#peek struct sigevent, _sigev_un._sigev_thread._attribute) p_Sigevent
    return (Sigevent sigevVal sigevSigno sigevNotify sigevFunction sigevAttribute)

data TimeSpec = TimeSpec {
  tvSec :: Int,     -- Seconds
  tvNsec :: Int     -- Nano-seconds
  } deriving Show

instance Storable TimeSpec where
  sizeOf _ = (#const sizeof (struct timespec))
  alignment _ = 1
  poke p_timespec (TimeSpec tvSec tvNsec) = do
    (#poke struct timespec, tv_sec) p_timespec tvSec
    (#poke struct timespec, tv_nsec) p_timespec tvNsec
  peek p_timespec = do
    tvSec <- (#peek struct timespec, tv_sec) p_timespec
    tvNsec <- (#peek struct timespec, tv_nsec) p_timespec
    return (TimeSpec  tvSec tvNsec)

data ItimerSpec = ItimerSpec {
  itInterval :: TimeSpec,   -- ^ Timer period
  itValue :: TimeSpec       -- ^ Timer expiration
} deriving(Show)

instance Storable ItimerSpec where
  sizeOf _ = (#const sizeof (struct itimerspec))
  alignment _ = 1
  poke p_itimerspec (ItimerSpec itInterval itValue) = do
    (#poke struct itimerspec, it_interval) p_itimerspec itInterval
    (#poke struct itimerspec, it_value) p_itimerspec itValue 
  peek p_itimerspec = do
    itInterval <- (#peek struct itimerspec, it_interval) p_itimerspec
    itValue <- (#peek struct itimerspec, it_value) p_itimerspec
    return (ItimerSpec itInterval itValue)
