-- |
-- Module      : MTP.Handle
-- Copyright   : (c) Joachim Fasting 2010
-- License     : LGPL
--
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : unstable
-- Portability : not portable
--
-- Bindings to libmtp. Device handle.

module MTP.Handle (MTPHandle, isClosed, open, close, withMTPHandle) where

import MTP.Foreign (MTPDevice)

import Control.Concurrent.MVar
import Control.Monad
import Foreign

data HandleState = Closed | Open deriving Eq

data MTPHandle_ = MTPHandle_
    { haDev :: !(ForeignPtr MTPDevice)
    , haState :: HandleState
    }

-- | A handle to an MTP device connection.
data MTPHandle = MTPHandle (MVar MTPHandle_)

-- Open handle, given a device pointer.
open :: Ptr MTPDevice -> IO MTPHandle
open dptr = do
    -- XXX: newForeignPtr finalizerFree devptr causes a double free
    -- error when the finalizer is run.
    dev <- newForeignPtr_ dptr
    h_ <- newMVar $ MTPHandle_ dev Open
    return (MTPHandle h_)

-- Close handle.
close :: MTPHandle -> IO ()
close (MTPHandle hv) = modifyMVar_ hv (\x -> return x { haState = Closed })

-- Test whether the handle is closed.
isClosed :: MTPHandle -> IO Bool
isClosed (MTPHandle hv) = withMVar hv (\x -> return $ haState x == Closed)

-- A helper that lifts operations on MTPDevice into MTPHandle.
-- Throws error if the handle is unusable.
withMTPHandle :: MTPHandle -> (Ptr MTPDevice -> IO a) -> IO a
withMTPHandle h@(MTPHandle hv) f = do
    closed <- isClosed h
    when closed (fail "MTPHandle is closed")
    withMVar hv (\x -> withForeignPtr (haDev x) f)
