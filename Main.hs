-- Transfer files to and from the device.

module Main (main) where

import MTP

import System.Environment
import Text.Printf

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("list":_) -> do
            mtp <- getFirstDevice
            files <- getFileListing mtp
            mapM_ (\f -> printf "%d - %s - %d - %s\n" (fileID f) (fileName f)
                                 (fileSize f) (show (fileType f)))
                  files
        ("get":fid:fname:_) -> do
            mtp <- getFirstDevice
            getFileToFile mtp (read fid) fname
        ("send":fname:_) -> do
            mtp <- getFirstDevice
            sendFileFromFile mtp fname
        _ -> putStrLn "Usage: Main <list|get|send> [ARGUMENT..]"
