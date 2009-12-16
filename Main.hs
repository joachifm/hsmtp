-- Transfer files to and from the device.

module Main (main) where

import MTP

import System.Environment
import Text.Printf

dumpInfo h = do
    manufacturer <- getManufacturerName h
    serial <- getSerialNumber h
    friendly <- getFriendlyName h
    devversion <- getDeviceVersion h
    model <- getModelName h
    charge <- getBatteryLevel h
    putStr $ unlines ["Model: " ++ model
                     ,"Manufacturer: " ++ manufacturer
                     ,"Serial: " ++ serial
                     ,"Firmware version: " ++ devversion
                     ,"Battery: " ++ show charge
                     ,"Owner: " ++ friendly
                     ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("list-files":_) -> withFirstDevice $ \mtp -> do
            files <- getFileListing mtp
            mapM_ (\f -> printf "%d - %s - %d - %s\n" (fileID f) (fileName f)
                                 (fileSize f) (show (fileType f)))
                  files
        ("get-file":fid:fname:_) -> withFirstDevice $ \mtp -> do
            getFileToFile mtp (read fid) fname
        ("send-file":fname:_) -> withFirstDevice $ \mtp -> do
            sendFileFromFile mtp fname
        ("list-tracks":_) -> withFirstDevice $ \mtp -> do
            tracks <- getTrackListing mtp
            print tracks
        ("list-playlists":_) -> withFirstDevice $ \mtp -> do
            pls <- getPlaylistList mtp
            print pls
        ("info":_) -> withFirstDevice dumpInfo
        _ -> do
            putStrLn "Usage: Main COMMAND [ARGUMENT..]"
            putStrLn "Commands:"
            putStr $ unlines ["\tlist-files"
                             ,"\tget-file"
                             ,"\tsend-file"
                             ,"\tlist-tracks"
                             ,"\tget-track"
                             ,"\tsend-track"
                             ,"\tinfo"]
