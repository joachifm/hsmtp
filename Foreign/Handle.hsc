{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.Handle (
    fdopen, fflush, fclose,
    handleToCFile
    ) where

import GHC.IO.Handle
import Foreign
import Foreign.C
import System.Posix.IO
import System.Posix.Types

foreign import ccall unsafe "stdio.h fdopen" fdopen :: Fd -> CString -> IO (Ptr CFile)

foreign import ccall unsafe "stdio.h fflush" fflush :: Ptr CFile -> IO ()

foreign import ccall unsafe "stdio.h fclose" fclose :: Ptr CFile -> IO ()

-- | Convert a Handle to a CFile.
-- Source <http://haskell.org/haskellwiki/The_Monad.Reader/Issue2/Bzlib2Binding>
handleToCFile :: Handle -> String -> IO (Ptr CFile)
handleToCFile h m = withCAString m $ \iomode -> do
    -- Create a duplicate so the original handle is kept open
    h' <- hDuplicate h
    fd <- handleToFd h'
    fdopen fd iomode
