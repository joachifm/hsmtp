-- Transfer files to and from the device.

module Main (main) where

import MTP

import System.Environment
import Text.Printf

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("list":_) -> withFirstDevice $ \mtp -> do
            files <- getFileListing mtp
            mapM_ (\f -> printf "%d - %s - %d - %s\n" (fileID f) (fileName f)
                                 (fileSize f) (show (fileType f)))
                  files
        ("get":fid:fname:_) -> withFirstDevice $ \mtp -> do
            getFileToFile mtp (read fid) fname
        ("send":fname:_) -> withFirstDevice $ \mtp -> do
            sendFileFromFile mtp fname
        _ -> putStrLn "Usage: Main <list|get|send> [ARGUMENT..]"
