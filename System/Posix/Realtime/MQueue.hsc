{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Realtime.MQueue
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  William N. Halchin (vigalchin@gmail.com)
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX 1003.1b message queue support.  See
-- <http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/mqueue.h.html>.
--
-----------------------------------------------------------------------------

module System.Posix.Realtime.MQueue (
  -- ** Message queue attributes data type
  MQAttributes(..),

  -- ** Opening\/closing\/unlinking mqueues
  Name,
  mqOpen,
  mqClose,
  mqUnlink,

  -- ** Sending\/receiving data
  -- |Programmers using the 'mqSend' and 'mqReceive' API should be aware that
  -- EAGAIN exceptions may occur for non-blocking IO!
  mqSend,
  mqReceive,

  -- **  Getting\/Setting mqueue attributes
  mqGetAttributes,
  mqSetAttributes,

  -- ** Notify receipt of message
  mqNotify
  ) where


import System.IO
import System.IO.Error
import System.Posix.Realtime.RTDataTypes
import System.Posix.IO
import System.Posix.Types
import System.Posix.Error
import System.Posix.Internals

import Foreign
import Foreign.C
import Data.Bits
import Data.ByteString

#ifdef __GLASGOW_HASKELL__
import GHC.IO
import GHC.IO.Exception
import GHC.IO.Handle hiding (fdToHandle)
import qualified GHC.IO.Handle
#endif

#ifdef __HUGS__
import Hugs.Prelude (IOException(..), IOErrorType(..))
import qualified Hugs.IO (handleToFd, openFd)
#endif

#include <mqueue.h>


type Name = String

data MQAttributes =
  MQAttributes {
    flags      :: Int,
    maxMsgNum  :: Int,
    maxMsgSize :: Int,
    curNumMsgs :: Int
    } deriving Show


-- | MQAttributes Storable used to marshall and unmarshall to ANSI C
instance Storable MQAttributes where
  sizeOf (MQAttributes flags maxMsgNum maxMsgSize curNumMsgs) = (#const sizeof(struct mq_attr))
  alignment _ = 1
  poke p_attrs (MQAttributes flags maxMsgNum maxMsgSize curNumMsgs) = do
    (#poke struct mq_attr, mq_flags)    p_attrs  flags
    (#poke struct mq_attr, mq_maxmsg)   p_attrs  maxMsgNum
    (#poke struct mq_attr, mq_msgsize)  p_attrs  maxMsgSize
    (#poke struct mq_attr, mq_curmsgs)  p_attrs  curNumMsgs
  peek p_attrs = do
    flags <- (#peek struct mq_attr, mq_flags) p_attrs
    maxMsgNum <- (#peek struct mq_attr, mq_maxmsg) p_attrs
    maxMsgSize <- (#peek struct mq_attr, mq_msgsize) p_attrs
    curNumMsgs <- (#peek struct mq_attr, mq_curmsgs) p_attrs
    return (MQAttributes flags maxMsgNum maxMsgSize curNumMsgs)


-- | Open and optionally create a message queue
--
-- /Note/: The POSIX standard puts some constraints on 'Name' but leaves much as
-- "implementation-defined", meaning that one needs to read the system-specific
-- details on what a conforming name should look like.
mqOpen :: Name
       -> OpenMode
       -> Maybe FileMode -- ^ @Just x@ creates the queue with the given modes, @Nothing@ then the file must exist.
       -> Maybe MQAttributes -- ^ @Just x@ creates the queue with given attributes, @Nothing@ with default attributes.
       -> IO Fd
mqOpen name how maybe_mode (Just attrs) = do
  withCString name $ \ p_name -> do
    allocaBytes (#const sizeof(struct mq_attr)) $ \ p_attrs -> do
      poke p_attrs attrs
      mqd <- throwErrnoPathIfMinus1 "mqOpen" name (c_mq_open p_name all_flags mode_w p_attrs)
      return (Fd mqd)
        where
          all_flags  = creat .|. open_mode

          (creat, mode_w) = case maybe_mode of
            Nothing -> (0,0)
            Just x  -> ((#const O_CREAT), x)

          open_mode = case how of
            ReadOnly  -> (#const O_RDONLY)
            WriteOnly -> (#const O_WRONLY)
            ReadWrite -> (#const O_RDWR)

mqOpen name how maybe_mode Nothing = do
  withCString name $ \ p_name -> do
    mqd <- throwErrnoPathIfMinus1 "mqOpen" name (c_mq_open p_name all_flags mode_w nullPtr)
    return (Fd mqd)
      where
        all_flags  = creat .|. open_mode 

        (creat, mode_w) = case maybe_mode of 
          Nothing -> (0,0)
          Just x  -> ((#const O_CREAT), x)

        open_mode = case how of
          ReadOnly  -> (#const O_RDONLY)
          WriteOnly -> (#const O_WRONLY)
          ReadWrite -> (#const O_RDWR)

foreign import ccall unsafe "bits/mqueue.h mq_open"
  c_mq_open :: CString -> CInt -> CMode -> Ptr MQAttributes -> IO CInt


-- | Close a message queue
mqClose :: Fd -> IO ()
mqClose (Fd mqd) = throwErrnoIfMinus1_ "mqClose" (c_mq_close mqd)

foreign import ccall unsafe "mqueue.h mq_close"
  c_mq_close :: CInt -> IO CInt


-- | Unlink (destroy) an existing message queue
mqUnlink :: String -> IO ()
mqUnlink name = do
  withCString name $ \ p_name -> do
    throwErrnoPathIfMinus1 "mqUnlink" name (c_mq_unlink p_name)
    return ()

foreign import ccall unsafe "mqueue.h mq_unlink"
  c_mq_unlink :: CString -> IO CInt


-- | Get the attributes for an existing message queue
mqGetAttributes :: Fd -> IO MQAttributes
mqGetAttributes (Fd mqd) = do
  allocaBytes (#const sizeof(struct mq_attr)) $ \ p_attrs -> do
    throwErrnoIfMinus1 "mqGetAttributes" (c_mq_getattr mqd p_attrs)
    mq_attrs <- peek p_attrs
    return (mq_attrs)

foreign import ccall unsafe "mqueue.h mq_getattr"
  c_mq_getattr :: CInt ->  Ptr MQAttributes -> IO CInt


-- | Set the attributes for an existing message queue and its retrieve old attributes
mqSetAttributes :: Fd -> MQAttributes -> IO (MQAttributes)
mqSetAttributes (Fd mqd) newAttrs = do
  allocaBytes (#const sizeof(struct mq_attr)) $ \ p_attrs -> do
    allocaBytes (#const sizeof(struct mq_attr)) $ \ p_oldattrs -> do
      poke p_attrs newAttrs
      throwErrnoIfMinus1 "mqSetAttributes" (c_mq_setattr mqd p_attrs p_oldattrs)
      oldAttrs <- peek p_oldattrs
      return (oldAttrs)

foreign import ccall unsafe "mqueue.h mq_setattr"
  c_mq_setattr :: CInt ->  Ptr MQAttributes -> Ptr MQAttributes -> IO CInt


-- | Retrieve a message from a message queue
--
-- /Note/: @mq_timedreceive@ is not exposed, wrap 'mqReceive' in 'System.Timeout.timeout'
-- to get a timed receive.
mqReceive :: Fd -> ByteCount -> Maybe Int -> IO (ByteString, Int)
mqReceive (Fd mqd) len (Just prio) = do
  allocaBytes (fromIntegral len) $ \ p_buffer -> do
    with (fromIntegral prio) $ \ p_prio -> do
      rc <- throwErrnoIfMinus1 "mqReceive" (c_mq_receive mqd p_buffer (fromIntegral len) p_prio)
      case fromIntegral rc of
        0 -> ioError (IOError Nothing EOF "mqReceive" "EOF" Nothing Nothing)
        n -> do
          s <- packCStringLen (p_buffer, fromIntegral n)
          return (s, n)

mqReceive (Fd mqd) len Nothing = do
  allocaBytes (fromIntegral len) $ \ p_buffer -> do
    rc <- throwErrnoIfMinus1 "mqReceive" (c_mq_receive mqd p_buffer (fromIntegral len) nullPtr)
    case fromIntegral rc of
      0 -> ioError (IOError Nothing EOF "mqReceive" "EOF" Nothing Nothing)
      n -> do
        s <- packCStringLen (p_buffer, fromIntegral n)
        return (s, n)

foreign import ccall unsafe "mqueue.h mq_receive"
  c_mq_receive :: CInt -> Ptr CChar -> CSize -> Ptr CInt -> IO CInt


-- | Send a message on a message queue
mqSend :: Fd -> ByteString -> ByteCount -> Int -> IO ()
mqSend (Fd mqd) msg len prio = do
  useAsCString msg $ \ p_msg -> do
    throwErrnoIfMinus1 "mqSend" (c_mq_send mqd p_msg (fromIntegral len) (fromIntegral prio))
    return ()

foreign import ccall unsafe "mqueue.h mq_send"
  c_mq_send :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt


-- | Notify a registered process of the new-message-in-empty-queue event
mqNotify :: Fd -> Maybe Sigevent -> IO ()
mqNotify (Fd mqd) Nothing = do
  throwErrnoIfMinus1 "mqNotify" (c_mq_notify mqd nullPtr)
  return ()

mqNotify (Fd mqd) (Just sigEvent) = do
  allocaBytes (#const sizeof(struct sigevent)) $ \ p_sigevent -> do
    poke p_sigevent sigEvent
    throwErrnoIfMinus1 "mqNotify" (c_mq_notify mqd p_sigevent)
    return ()

foreign import ccall unsafe "mqueue.h mq_notify"
  c_mq_notify :: CInt -> Ptr Sigevent -> IO CInt
