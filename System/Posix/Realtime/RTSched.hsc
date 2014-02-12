{-# OPTIONS -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Realtime.RTSched
-- Copyright   :  (c) The University of Glasgow 2002   
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  William N. Halchin (vigalchin@gmail.com)
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX 1003.1b realtime scheduling support.  
--
-----------------------------------------------------------------------------

module System.Posix.Realtime.RTSched (

  Pid,

  Policy,

  SchedParam(..),

  schedYield,                     --  IO ()

  schedGetScheduler,              --  Pid -> IO (Int)

  schedSetScheduler,              --  Pid -> Policy -> SchedParam -> IO ()

  schedGetParam,                  --  Pid -> IO SchedParam

  schedSetParam,                  --  Pid -> SchedParam -> IO ()

  schedGetPriorityMin,            --  Policy -> IO (Int)

  schedGetPriorityMax,            --  Policy -> IO (Int)

  schedRRGetInterval              --  Pid -> IO (TimeSpec)

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
import GHC.IOBase
import GHC.Handle hiding (fdToHandle)
import qualified GHC.Handle
#endif

#ifdef __HUGS__
import Hugs.Prelude (IOException(..), IOErrorType(..))
import qualified Hugs.IO (handleToFd, openFd)
#endif

#include "HsUnix.h"
#include <sched.h>

-- -----------------------------------------------------------------------------

type Pid = Int
type Policy = Int

data SchedParam =
   SchedParam {
     schedPriority :: Int
}


instance Storable SchedParam  where
    sizeOf (SchedParam schedPriority) = (#const sizeof(struct sched_param))
    alignment _ = 1
    poke p_schedParam (SchedParam schedPriority) = do
      (#poke struct sched_param, __sched_priority) p_schedParam schedPriority 
    peek p_schedParam = do
      schedPriority <- (#peek struct sched_param, __sched_priority) p_schedParam 
      return (SchedParam schedPriority)



-- | Running thread relinquishes control of its processor
schedYield :: IO ()
schedYield = do
   throwErrnoIfMinus1 "schedYield" (c_sched_yield)
   return ()

foreign import ccall unsafe "sched.h sched_yield"
    c_sched_yield :: IO CInt



-- | Returns the scheduling policy of the process specified by Pid.
schedGetScheduler :: Pid -> IO (Int)
schedGetScheduler pid = do
   rc <- throwErrnoIfMinus1 "schedGetScheduler" (c_sched_getscheduler (fromIntegral pid))
   return (fromIntegral rc)

foreign import ccall unsafe "sched.h sched_getscheduler"
    c_sched_getscheduler :: CInt -> IO CInt



-- | Sets the scheduling policy and scheduling parameters of the process 
-- | specified by Pid to policy and the parameters specified schedParam.
-- | The value of the schedPriority member in schedParam is any integer 
-- | within the inclusive priority range for the scheduling policy specified 
-- | by policy.   
schedSetScheduler :: Pid -> Policy -> SchedParam -> IO ()
schedSetScheduler pid policy param = do
   allocaBytes (#const sizeof(struct sched_param)) $ \ p_schedParam -> do
      poke p_schedParam param
      throwErrnoIfMinus1 "schedSetScheduler" (c_sched_setscheduler (fromIntegral pid) (fromIntegral policy) p_schedParam)
      return ()

foreign import ccall unsafe "sched.h sched_setscheduler"
    c_sched_setscheduler :: CInt -> CInt -> Ptr SchedParam -> IO CInt



-- | Returns the scheduling parameters of a process specified by Pid 
-- | in schedParam.
schedGetParam :: Pid -> IO SchedParam
schedGetParam pid = do
   allocaBytes (#const sizeof(struct sched_param)) $ \ p_schedParam -> do
      throwErrnoIfMinus1 "schedGetParam" (c_sched_getparam (fromIntegral pid) p_schedParam)
      schedParam <- peek p_schedParam
      return (schedParam)

foreign import ccall unsafe "sched.h sched_getparam"
    c_sched_getparam :: CInt -> Ptr SchedParam -> IO CInt 



-- | Sets the scheduling parameters of the process specified by Pid to 
-- | the values specified by schedParam.
schedSetParam :: Pid -> SchedParam -> IO ()
schedSetParam pid param = do
   allocaBytes (#const sizeof(struct sched_param)) $ \ p_schedParam -> do
      poke p_schedParam param
      throwErrnoIfMinus1 "schedSetParam" (c_sched_setparam (fromIntegral pid) p_schedParam)
      return ()

foreign import ccall unsafe "sched.h sched_setparam"
    c_sched_setparam :: CInt -> Ptr SchedParam -> IO CInt 



-- | Returns the appropriate minimum for the scheduling policy specified 
-- | by Policy.  
schedGetPriorityMin :: Policy -> IO (Int)
schedGetPriorityMin  policy = do
      rc <- throwErrnoIfMinus1 "schedGetPriorityMin" (c_sched_get_priority_min (fromIntegral policy))
      return (fromIntegral rc)


foreign import ccall unsafe "sched.h sched_get_priority_min"
    c_sched_get_priority_min :: CInt -> IO CInt


-- | Returns the appropriate maximum for the scheduling policy specified 
-- | by Policy.  
schedGetPriorityMax :: Policy -> IO (Int)
schedGetPriorityMax  policy = do
      rc <- throwErrnoIfMinus1 "schedGetPriorityMax" (c_sched_get_priority_max (fromIntegral policy))
      return (fromIntegral rc)


foreign import ccall unsafe "sched.h sched_get_priority_max"
    c_sched_get_priority_max :: CInt -> IO CInt



-- | Get Round Robin scheduling interval
schedRRGetInterval :: Pid -> IO (TimeSpec)
schedRRGetInterval pid = do
   allocaBytes (#const sizeof(struct timespec )) $ \ p_timeSpec -> do
     throwErrnoIfMinus1 "schedRRGetInterval" (c_sched_rr_get_interval (fromIntegral pid) p_timeSpec)
     timeSpec <- peek p_timeSpec
     return (timeSpec)


foreign import ccall unsafe "sched.h sched_rr_get_interval"
     c_sched_rr_get_interval :: CInt -> Ptr TimeSpec -> IO CInt
