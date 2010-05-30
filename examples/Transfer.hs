-- | An example program that transfers files specified on the command line to
-- the first available MTP device, using TagLib to obtain track metadata.

module Main (main) where

import MTP

import qualified Sound.TagLib as TagLib

import Control.Exception (bracket)
import Data.Maybe
import Prelude hiding (init)
import System.Environment
import System.FilePath
import System.IO

main :: IO ()
main = do
    init
    withFirstDevice $ \dev -> do
        args <- getArgs
        mapM_ (\fn -> sendTrack dev fn =<< getTrackmeta fn) args

-- | Create track metadata from a file.
getTrackmeta :: FilePath -> IO Track
getTrackmeta filePath = do
    file <- fromJust `fmap` TagLib.open filePath
    tag <- fromJust `fmap` TagLib.tag file
    prop <- fromJust `fmap` TagLib.audioProperties file
    title <- TagLib.title tag
    artist <- TagLib.artist tag
    genre <- TagLib.genre tag
    album <- TagLib.album tag
    year <- TagLib.year tag
    track <- TagLib.track tag
    duration <- TagLib.duration prop
    srate <- TagLib.sampleRate prop
    brate <- TagLib.bitRate prop
    chans <- TagLib.channels prop
    fsize <- fromJust `fmap` getFileSize filePath
    return emptyTrack { trackComposer = ""
                      , trackTitle = title
                      , trackArtist = artist
                      , trackGenre = genre
                      , trackAlbum = album
                      , trackDate = show year
                      , trackFileName = takeBaseName filePath
                      , trackNumber = fromIntegral track
                      , trackDuration = fromIntegral (duration * 1000)
                      , trackSamplerate = fromIntegral srate
                      , trackBitrate = fromIntegral brate
                      , trackChannels = fromIntegral chans
                      , trackFileSize = fsize
                      , trackFileType = findFileType filePath
                 }

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize fname =
    maybeIO $ bracket (openFile fname ReadMode) hClose hFileSize

maybeIO :: IO a -> IO (Maybe a)
maybeIO f = (Just `fmap` f) `catch` (\_ -> return Nothing)
