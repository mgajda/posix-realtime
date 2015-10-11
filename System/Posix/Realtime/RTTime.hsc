{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Realtime.RTTimer
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  William N. Halchin (vigalchin@gmail.com)
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX Realtime Timer and Clock support.  See
-- <http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/time.h.html>.
--
-----------------------------------------------------------------------------

module System.Posix.Realtime.RTTime (
  TimerId,
  ClockId(..),
  SetTimeFlag(..),
  timerCreate,
  timerDelete,
  timerSetTime,
  timerGetTime,
  timerGetOverrun,
  clockGetRes,
  clockGetTime,
  clockSetTime
  ) where

#include <HsUnix.h>
#include <time.h>

import System.Posix.Realtime.RTDataTypes
import System.Posix.Types
import System.Posix.Error
import System.Posix.Internals
import Foreign
import Foreign.C


type TimerId = Int

data ClockId = Clock_Realtime
             | Clock_Monotonic
             | Clock_Process_CPUTime_ID
             | Clock_Thread_CPUTime_ID

type CClockId = Int

data SetTimeFlag = Timer_Abstime


-- | Create a realtime timer.
timerCreate :: ClockId -> Maybe Sigevent -> IO TimerId
timerCreate clockId (Just sigEvent) =
  allocaBytes (#const sizeof(struct sigevent)) $ \ p_sigevent -> do
    allocaBytes (#const sizeof(int)) $ \ p_timerId -> do
      poke p_sigevent sigEvent 
      throwErrnoIfMinus1 "timerCreate" (c_timer_create (fromIntegral (mapClockId clockId)) p_sigevent p_timerId)
      timerId <- peek p_timerId
      return  timerId

timerCreate clockId Nothing =
  allocaBytes (#const sizeof(int)) $ \ p_timerId -> do
    throwErrnoIfMinus1 "timerCreate" (c_timer_create (fromIntegral (mapClockId clockId)) nullPtr p_timerId)
    timerId <- peek p_timerId
    return  (timerId)

foreign import ccall safe "time.h timer_create"
  c_timer_create :: CInt -> Ptr Sigevent -> Ptr TimerId -> IO CInt


-- | Delete a timer.
timerDelete :: TimerId -> IO ()
timerDelete timerId = do
  throwErrnoIfMinus1 "timerDelete" (c_timer_delete (fromIntegral timerId))
  return ()

foreign import ccall safe "time.h timer_delete"
  c_timer_delete :: CInt -> IO CInt


-- | Get the timer state.
timerGetTime :: TimerId -> IO ItimerSpec
timerGetTime timerId =
  allocaBytes (#const sizeof(struct itimerspec)) $ \ p_itimerSpec -> do
    throwErrnoIfMinus1 "timerGettime" (c_timer_gettime (fromIntegral timerId) p_itimerSpec) 
    itimerSpec <- peek p_itimerSpec
    return itimerSpec

foreign import ccall safe "time.h timer_gettime"
  c_timer_gettime :: CInt -> Ptr ItimerSpec-> IO CInt 


-- | Set the timer state.
timerSetTime :: TimerId -> SetTimeFlag -> ItimerSpec -> IO ItimerSpec
timerSetTime timerId setTimeFlag itimerSpec = do 
  allocaBytes (#const sizeof(struct itimerspec)) $ \ p_itimerSpec -> do
    poke p_itimerSpec itimerSpec
    allocaBytes (#const sizeof(struct itimerspec)) $ \ p_olditimerSpec -> do
      throwErrnoIfMinus1 "timerSettime" (c_timer_settime (fromIntegral timerId) (cSetTimeFlag) p_itimerSpec p_olditimerSpec) 
      olditimerSpec <- peek p_olditimerSpec
      return olditimerSpec

        where
          cSetTimeFlag = case setTimeFlag of
            Timer_Abstime -> (#const TIMER_ABSTIME) 

foreign import ccall safe "time.h timer_settime"
  c_timer_settime :: CInt -> CInt -> Ptr ItimerSpec -> Ptr ItimerSpec-> IO CInt 


-- | Get the timer overrun count.
timerGetOverrun :: TimerId -> IO Int
timerGetOverrun timerId = do
  rc <- throwErrnoIfMinus1 "timerGetoverrun" (c_timer_getoverrun (fromIntegral timerId))
  return (fromIntegral rc)

foreign import ccall safe "time.h timer_getoverrun"
  c_timer_getoverrun :: CInt -> IO CInt


-- | Get clock resolution.
clockGetRes :: ClockId -> IO TimeSpec
clockGetRes clockId =
  allocaBytes (#const sizeof(struct timespec)) $ \ p_timeSpec -> do
    throwErrnoIfMinus1 "clockGetres" (c_clock_getres (fromIntegral (mapClockId clockId)) p_timeSpec) 
    timeSpec <- peek p_timeSpec
    return timeSpec

foreign import ccall safe "time.h clock_getres"
  c_clock_getres :: CInt -> Ptr TimeSpec -> IO CInt


-- | Get clock time.
clockGetTime :: ClockId -> IO TimeSpec
clockGetTime clockId =
  allocaBytes (#const sizeof(struct timespec)) $ \ p_timeSpec -> do
    throwErrnoIfMinus1 "clockGettime" (c_clock_gettime (fromIntegral (mapClockId clockId)) p_timeSpec) 
    timeSpec <- peek p_timeSpec
    return (timeSpec)

foreign import ccall safe "time.h clock_gettime"
  c_clock_gettime :: CInt -> Ptr TimeSpec -> IO CInt


-- | Set clock time.
clockSetTime :: ClockId -> TimeSpec -> IO ()
clockSetTime clockId timeSpec =
  allocaBytes (#const sizeof(struct timespec)) $ \ p_timeSpec -> do
    throwErrnoIfMinus1 "clockSettime" (c_clock_settime (fromIntegral (mapClockId clockId)) p_timeSpec) 
    return ()

foreign import ccall safe "time.h clock_settime"
  c_clock_settime :: CInt -> Ptr TimeSpec -> IO CInt


-- | Helper function that maps a clockid to it's C representation.
mapClockId :: ClockId -> CClockId
mapClockId clockId = case clockId of
  Clock_Realtime             -> (#const CLOCK_REALTIME)
  Clock_Monotonic            -> (#const CLOCK_MONOTONIC)
  Clock_Process_CPUTime_ID   -> (#const CLOCK_PROCESS_CPUTIME_ID)
  Clock_Thread_CPUTime_ID    -> (#const CLOCK_THREAD_CPUTIME_ID)
