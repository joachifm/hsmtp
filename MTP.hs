{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : MTP
-- Copyright   : (c) Joachim Fasting 2010
-- License     : LGPL
--
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : unstable
-- Portability : not portable
--
-- Bindings to libmtp. High-level interface.
--
-- Usage:
--
-- > import qualified MTP
--
-- > main = do
--
-- > MTP.init
--
-- > MTP.withFirstDevice MTP.getDeviceVersion

module MTP (
    -- * Types
    MTPHandle, Track(..), File(..), Folder(..), Playlist(..), FileType,
    StorageOrder, MTPException(..),
    -- * Constants
    version,
    notsorted, freespace, maxspace,
    wav, mp3, wma, ogg, audible, mp4, undef_audio, wmv, avi, mpeg, asf, qt,
    undef_video, jpeg, jfif, tiff, bmp, gif, pict, png, vcalendar1,
    vcalendar2, vcard2, vcard3, windowsimageformat, winexec, text, html,
    firmware, aac, mediacard, flac, mp2, m4a, doc, xml, xls, ppt, mht, jp2,
    unknown,
    -- * Device management
    init, getFirstDevice, releaseDevice, resetDevice,
    withFirstDevice,
    getDeviceVersion, getManufacturerName, getModelName, getSerialNumber,
    getFriendlyName, getBatteryLevel, getSupportedFileTypes,
    -- * File management
    getFileListing,
    getFile, sendFile,
    hGetFile, hSendFile,
    setFileName,
    -- * Track management
    emptyTrack,
    doesTrackExist,
    getTrackListing,
    getTrack,
    sendTrack,
    hGetTrack, hSendTrack,
    updateTrack,
    getTrackMetadata,
    setTrackName,
    -- * Folder management
    createFolder, getFolderList, setFolderName,
    -- * Audio\/video playlist management
    getPlaylistList, getPlaylist, createPlaylist, updatePlaylist,
    setPlaylistName,
    -- * Object management
    deleteObject, setObjectName,
    -- * Extras
    findFileType
    ) where

import Foreign.Handle
import MTP.Foreign
import MTP.Handle

import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Typeable
import Foreign
import Foreign.C
import Prelude hiding (init)
import System.FilePath
import System.IO

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- | MTP exceptions.
data MTPException
    = NoDevice
    | StorageFull
    | ConnectionFailed
    | Cancelled
    | General String
    deriving (Eq, Show, Typeable)

instance Exception MTPException where

-- | File metadata.
data File = File
    { fileID :: Int
    , fileParentID :: Int
    , fileStorageID :: Int
    , fileName :: String
    , fileSize :: Integer
    , fileType :: FileType
    } deriving (Eq, Show)

-- | Track metadata.
data Track = Track
    { trackID :: Int
    , trackParentID :: Int
    , trackStorageID :: Int
    , trackTitle :: String
    , trackArtist :: String
    , trackComposer :: String
    , trackGenre :: String
    , trackAlbum :: String
    , trackDate :: String
    , trackFileName :: String
    , trackNumber :: Int
    , trackDuration :: Int
    , trackSamplerate :: Int
    , trackChannels :: Int
    , trackWavecodec :: Int
    , trackBitrate :: Int
    , trackBitrateType :: Int
    , trackRating :: Int
    , trackUseCount :: Int
    , trackFileSize :: Integer
    , trackFileType :: FileType
    } deriving (Eq, Show)

-- | Folder metadata.
data Folder = Folder
    { folderID :: Int
    , folderParentID :: Int
    , folderStorageID :: Int
    , folderName :: String
    , folderChild :: Maybe Folder
    }

-- | Playlist metadata.
data Playlist = Playlist
    { playlistID :: Int
    , playlistParentID :: Int
    , playlistStorageID :: Int
    , playlistName :: String
    , playlistTracks :: [Int]
    , playlistNoTracks :: Int
    } deriving (Eq, Show)

------------------------------------------------------------------------------
-- Device management
------------------------------------------------------------------------------

-- | Initialize MTP.
init :: IO ()
init = c_init

-- | Open a connection to the first available MTP device and run an
-- action, closing the connection afterwards.
withFirstDevice :: (MTPHandle -> IO a) -> IO a
withFirstDevice = bracket getFirstDevice releaseDevice

-- A helper that checks the error stack and throws an exception if there is an error.
checkError :: MTPHandle -> IO ()
checkError h = withMTPHandle h $ \devptr -> do
    e_ptr <- c_get_errorstack devptr
    unless (e_ptr == nullPtr) $ do
        et <- peek e_ptr
        es <- peekCString (et_errortext et)
        c_clear_errorstack devptr
        case ErrorCode (et_errornumber et) of
            x | x == general -> throw $ General es
              | x == noDevice -> throw NoDevice
              | x == storageFull -> throw StorageFull
              | x == connectionFailed -> throw ConnectionFailed
              | x == cancelled -> throw Cancelled
              | otherwise -> error $ "checkError: unhandled error number: " ++
                                     show x

-- | Connect to the first available MTP device.
getFirstDevice :: IO MTPHandle
getFirstDevice = do
    devptr <- c_get_first_device
    if devptr == nullPtr
       then throw NoDevice
       else open devptr

-- XXX: we should free the ptr, but it causes a double free error, WHY, OH WHY
-- | Close connection to a MTP device. The handle is unusable after this.
releaseDevice :: MTPHandle -> IO ()
releaseDevice h = withMTPHandle h c_release_device >> close h

-- | Reset device.
resetDevice :: MTPHandle -> IO ()
resetDevice h = withMTPHandle h $ \devptr -> do
    r <- c_reset_device devptr
    unless (r == 0) (checkError h)

-- | Get the device manufacturer name.
getManufacturerName :: MTPHandle -> IO String
getManufacturerName h = withMTPHandle h $ \devptr ->
    peekCString =<< c_get_manufacturername devptr

-- | Get the device model name.
getModelName :: MTPHandle -> IO String
getModelName h = withMTPHandle h $ \devptr ->
   peekCString =<< c_get_modelname devptr

-- | Get the device serial number.
getSerialNumber :: MTPHandle -> IO String
getSerialNumber h = withMTPHandle h $ \devptr ->
  peekCString =<< c_get_serialnumber devptr

-- | Get the owner string aka. the \"friendly name\".
getFriendlyName :: MTPHandle -> IO String
getFriendlyName h = withMTPHandle h $ \devptr ->
   peekCString =<< c_get_friendlyname devptr

-- | Get device hardware and firmware version.
getDeviceVersion :: MTPHandle -> IO String
getDeviceVersion h = withMTPHandle h $ \ptr ->
    peekCString =<< c_get_deviceversion ptr

-- | Get battery level, maximum and current.
getBatteryLevel :: MTPHandle -> IO (Int, Int)
getBatteryLevel h = withMTPHandle h $ \devptr ->
    alloca $ \maxptr ->
    alloca $ \curptr -> do
        ret <- c_get_batterylevel devptr maxptr curptr
        unless (ret == 0) (checkError h)
        maxv <- peek maxptr
        curv <- peek curptr
        return (fromIntegral maxv, fromIntegral curv)

-- | Get a list of supported file types.
getSupportedFileTypes :: MTPHandle -> IO [FileType]
getSupportedFileTypes h = withMTPHandle h $ \devptr ->
    alloca $ \ft_ptr ->
    alloca $ \len_ptr -> do
        r <- c_get_supported_filetypes devptr ft_ptr len_ptr
        unless (r == 0) (checkError h)
        len <- peek len_ptr
        map FileType `fmap` peekArray (fromIntegral len) ft_ptr

------------------------------------------------------------------------------
-- File management
------------------------------------------------------------------------------

-- Marshall a Track into a Track_t pointer using temporary storage.
withFilePtr :: File -> (Ptr File_t -> IO a) -> IO a
withFilePtr f = bracket alloc free
    where
        alloc = do
            ptr <- malloc :: IO (Ptr File_t)
            ft <- marshall
            poke ptr ft
            return ptr
        marshall =
            withCAString (fileName f) $ \name_ptr ->
                return File_t { ft_item_id = fromIntegral (fileID f)
                              , ft_parent_id = fromIntegral (fileParentID f)
                              , ft_storage_id = fromIntegral (fileStorageID f)
                              , ft_filename = name_ptr
                              , ft_filesize = fromIntegral (fileSize f)
                              , ft_filetype = unFileType (fileType f)
                              , ft_next = nullPtr
                              }

-- | Get a list of all files stored on the device.
getFileListing :: MTPHandle -> IO [File]
getFileListing h = withMTPHandle h $ \ptr ->
    toList [] =<< c_get_filelisting ptr nullPtr nullPtr
    where
        toList acc p =
            if p == nullPtr
               then return acc
               else do
                   ft <- peek p
                   fn <- convert ft
                   free p
                   toList (fn : acc) (ft_next ft)
        convert ft = do
            n <- peekCString (ft_filename ft)
            return File { fileID = fromIntegral (ft_item_id ft)
                        , fileParentID = fromIntegral (ft_parent_id ft)
                        , fileStorageID = fromIntegral (ft_storage_id ft)
                        , fileName = n
                        , fileSize = fromIntegral (ft_filesize ft)
                        , fileType = FileType (ft_filetype ft)
                        }

-- | Copy a file from the device to a local file.
getFile :: MTPHandle -> Int -> FilePath -> IO ()
getFile h i n =
    withMTPHandle h $ \devptr ->
    withCAString n $ \str_ptr -> do
        r <- c_get_file_to_file devptr (fromIntegral i) str_ptr nullPtr nullPtr
        unless (r == 0) (checkError h)

-- | Send a local file to the device.
sendFile :: MTPHandle -> FilePath -> IO ()
sendFile h n =
    withMTPHandle h $ \devptr ->
    withCAString n $ \str_ptr -> do
        r <- c_send_file_from_file devptr str_ptr nullPtr nullPtr
        unless (r == 0) (checkError h)

-- | Get a file from the device to a file handle.
hGetFile :: MTPHandle -> Int -> Handle -> IO ()
hGetFile h i fd = withMTPHandle h $ \devptr -> do
    oh <- handleToCFile fd "w"
    r <- c_get_file_to_file_descriptor devptr (fromIntegral i) oh nullPtr
                                       nullPtr
    fflush oh
    fclose oh
    unless (r == 0) (checkError h)

-- | Send a file to the device from a file handle.
hSendFile :: MTPHandle -> Handle -> File -> IO ()
hSendFile h fd f = withMTPHandle h $ \devptr ->
    withFilePtr f $ \file_ptr -> do
        ih <- handleToCFile fd "r"
        r <- c_send_file_from_file_descriptor devptr ih file_ptr nullPtr
                                              nullPtr
        fclose ih
        unless (r == 0) (checkError h)

-- | Rename a file on the device.
setFileName :: MTPHandle -> File -> String -> IO ()
setFileName h f n =
    withMTPHandle h $ \devptr ->
    withFilePtr f $ \file_ptr ->
    withCAString n $ \str_ptr -> do
        r <- c_set_file_name devptr file_ptr str_ptr
        unless (r == 0) (checkError h)

------------------------------------------------------------------------------
-- Track management
------------------------------------------------------------------------------

-- Marshall a Track into a Track_t pointer using temporary storage.
withTrackPtr :: Track -> (Ptr Track_t -> IO a) -> IO a
withTrackPtr t = bracket alloc free
    where
        alloc = do
            ptr <- malloc :: IO (Ptr Track_t)
            tt <- marshall
            poke ptr tt
            return ptr
        marshall =
            withCAString (trackTitle t) $ \title_ptr ->
            withCAString (trackArtist t) $ \artist_ptr ->
            withCAString (trackComposer t) $ \composer_ptr ->
            withCAString (trackGenre t) $ \genre_ptr ->
            withCAString (trackAlbum t) $ \album_ptr ->
            withCAString (trackDate t) $ \date_ptr ->
            withCAString (trackFileName t) $ \filename_ptr ->
                return Track_t
                           { tt_item_id = fromIntegral (trackID t)
                           , tt_parent_id = fromIntegral (trackParentID t)
                           , tt_storage_id = fromIntegral (trackStorageID t)
                           , tt_title = title_ptr
                           , tt_artist = artist_ptr
                           , tt_composer = composer_ptr
                           , tt_genre = genre_ptr
                           , tt_album = album_ptr
                           , tt_date = date_ptr
                           , tt_filename = filename_ptr
                           , tt_tracknumber = fromIntegral (trackNumber t)
                           , tt_duration = fromIntegral (trackDuration t)
                           , tt_samplerate = fromIntegral (trackSamplerate t)
                           , tt_nochannels = fromIntegral (trackChannels t)
                           , tt_wavecodec = fromIntegral (trackWavecodec t)
                           , tt_bitrate = fromIntegral (trackBitrate t)
                           , tt_bitratetype = fromIntegral (trackBitrateType t)
                           , tt_rating = fromIntegral (trackRating t)
                           , tt_usecount = fromIntegral (trackUseCount t)
                           , tt_filesize = fromIntegral (trackFileSize t)
                           , tt_filetype = unFileType (trackFileType t)
                           , tt_next = nullPtr
                           }

peekTrack :: Ptr Track_t -> IO [Track]
peekTrack = go []
    where
        go acc p =
            if p == nullPtr
               then return acc
               else do
                   tt <- peek p
                   tn <- convert tt
                   free p
                   go (tn : acc) (tt_next tt)
        convert tt = do
            ti <- peekCString (tt_title tt)
            ar <- peekCString (tt_artist tt)
            cm <- peekCString (tt_composer tt)
            ge <- peekCString (tt_genre tt)
            al <- peekCString (tt_album tt)
            dt <- peekCString (tt_date tt)
            fn <- peekCString (tt_filename tt)
            return $! Track { trackID = fromIntegral (tt_item_id tt)
                            , trackParentID = fromIntegral (tt_parent_id tt)
                            , trackStorageID = fromIntegral (tt_storage_id tt)
                            , trackTitle = ti
                            , trackArtist = ar
                            , trackComposer = cm
                            , trackGenre = ge
                            , trackAlbum = al
                            , trackDate = dt
                            , trackFileName = fn
                            , trackNumber = fromIntegral (tt_tracknumber tt)
                            , trackDuration = fromIntegral (tt_duration tt)
                            , trackSamplerate = fromIntegral (tt_samplerate tt)
                            , trackChannels = fromIntegral (tt_nochannels tt)
                            , trackWavecodec = fromIntegral (tt_wavecodec tt)
                            , trackBitrate = fromIntegral (tt_bitrate tt)
                            , trackBitrateType = fromIntegral (tt_bitratetype tt)
                            , trackRating = fromIntegral (tt_rating tt)
                            , trackUseCount = fromIntegral (tt_usecount tt)
                            , trackFileSize = fromIntegral (tt_filesize tt)
                            , trackFileType = FileType (tt_filetype tt)
                            }

-- | An empty track.
emptyTrack :: Track
emptyTrack =
    Track { trackAlbum = ""
          , trackArtist = ""
          , trackBitrate = 0
          , trackBitrateType = 0
          , trackComposer = ""
          , trackDate = ""
          , trackDuration = 0
          , trackFileName = ""
          , trackFileSize = 0
          , trackGenre = ""
          , trackFileType = unknown
          , trackID = 0
          , trackChannels = 0
          , trackParentID = 0
          , trackRating = 0
          , trackSamplerate = 0
          , trackStorageID = 0
          , trackTitle = ""
          , trackNumber = 0
          , trackUseCount = 0
          , trackWavecodec = 0
          }

-- | Test whether a track exists on the device.
doesTrackExist :: MTPHandle -> Int -> IO Bool
doesTrackExist h i = withMTPHandle h $ \devptr -> do
    exists <- c_track_exists devptr (fromIntegral i)
    return $ exists /= 0

-- | Get a list of all tracks stored on the device.
getTrackListing :: MTPHandle -> IO [Track]
getTrackListing h = withMTPHandle h $ \ptr ->
    peekTrack =<< c_get_tracklisting ptr nullPtr nullPtr

-- | Copy a track from the device to a local file.
getTrack :: MTPHandle -> Int -> FilePath -> IO ()
getTrack h i n = withMTPHandle h $ \devptr ->
    withCAString n $ \strptr -> do
        r <- c_get_track_to_file devptr (fromIntegral i) strptr nullPtr nullPtr
        unless (r == 0) (checkError h)

-- | Send a local track to the device, using the supplied metadata.
sendTrack :: MTPHandle -> FilePath -> Track -> IO ()
sendTrack h n t = withMTPHandle h $ \devptr ->
    withCAString n $ \strptr -> withTrackPtr t $ \tt_ptr -> do
        r <- c_send_track_from_file devptr strptr tt_ptr nullPtr nullPtr
        unless (r == 0) (checkError h)

-- | Copy a track from the device to a file handle.
hGetTrack :: MTPHandle -> Int -> Handle -> IO ()
hGetTrack h i fd = withMTPHandle h $ \devptr -> do
    oh <- handleToCFile fd "w"
    r <- c_get_track_to_file_descriptor devptr (fromIntegral i) oh nullPtr
                                        nullPtr
    fflush oh
    fclose oh
    unless (r == 0) (checkError h)

-- | Send a track to the device from a file handle.
hSendTrack :: MTPHandle -> Handle -> Track -> IO ()
hSendTrack h fd t = withMTPHandle h $ \devptr ->
    withTrackPtr t $ \track_ptr -> do
        ih <- handleToCFile fd "r"
        r <- c_send_track_from_file_descriptor devptr ih track_ptr nullPtr
                                               nullPtr
        fclose ih
        unless (r == 0) (checkError h)

-- | Update track metadata.
updateTrack :: MTPHandle -> Track -> IO ()
updateTrack h t = withMTPHandle h $ \devptr ->
    withTrackPtr t $ \tt_ptr -> do
        r <- c_update_track_metadata devptr tt_ptr
        unless (r == 0) (checkError h)

-- | Get metadata for a single track.
getTrackMetadata :: MTPHandle -> Int -> IO (Maybe Track)
getTrackMetadata h i = withMTPHandle h $ \devptr -> do
    r <- peekTrack =<< c_get_trackmetadata devptr (fromIntegral i)
    return $ listToMaybe r

-- | Rename a single track.
setTrackName :: MTPHandle -> Track -> String -> IO ()
setTrackName h t n = withMTPHandle h $ \devptr ->
    withTrackPtr t $ \track_ptr ->
    withCAString n $ \name_ptr -> do
        r <- c_set_track_name devptr track_ptr name_ptr
        unless (r == 0) (checkError h)

------------------------------------------------------------------------------
-- Folder management
------------------------------------------------------------------------------

peekFolder :: Ptr Folder_t -> IO [Folder]
peekFolder = go []
    where
        go acc p =
            if p == nullPtr
               then return acc
               else do
                   fdt <- peek p
                   fdn <- convert fdt
                   free p
                   go (fdn : acc) (fdt_sibling fdt)
        convert fdt = do
            name <- peekCString (fdt_name fdt)
            child <- if fdt_child fdt == nullPtr
                      then return Nothing
                      else peek (fdt_child fdt) >>= convert >>= return . Just
            return $! Folder
                       { folderID = fromIntegral (fdt_folder_id fdt)
                       , folderParentID = fromIntegral (fdt_parent_id fdt)
                       , folderStorageID = fromIntegral (fdt_storage_id fdt)
                       , folderName = name
                       , folderChild = child
                       }

-- XXX: should handle siblings and children
withFolderPtr :: Folder -> (Ptr Folder_t -> IO a) -> IO a
withFolderPtr f = bracket alloc free
    where
        alloc = do
            ptr <- malloc :: IO (Ptr Folder_t)
            fdt <- marshall
            poke ptr fdt
            return ptr
        marshall =
            withCAString (folderName f) $ \name_ptr ->
                return Folder_t
                           { fdt_folder_id = fromIntegral (folderID f)
                           , fdt_parent_id = fromIntegral (folderParentID f)
                           , fdt_storage_id = fromIntegral (folderStorageID f)
                           , fdt_name = name_ptr
                           , fdt_sibling = nullPtr
                           , fdt_child = nullPtr
                           }

-- | Create a new folder.
createFolder :: MTPHandle
             -> String -- ^ Folder name
             -> Int    -- ^ Parent ID
             -> Int    -- ^ Storage ID
             -> IO Int -- ^ ID to new folder
createFolder h n pid sid = withMTPHandle h $ \devptr ->
    withCAString n $ \name_ptr -> do
        r <- c_create_folder devptr name_ptr (fromIntegral pid) (fromIntegral sid)
        when (r == 0) (checkError h)
        return $ fromIntegral r

-- | Get a list of all folders on the device.
getFolderList :: MTPHandle -> IO [Folder]
getFolderList h = withMTPHandle h $ \devptr ->
    peekFolder =<< c_folder_list devptr

-- | Rename a folder.
setFolderName :: MTPHandle -> Folder -> String -> IO ()
setFolderName h f n = withMTPHandle h $ \devptr ->
    withFolderPtr f $ \folder_ptr ->
    withCAString n $ \name_ptr -> do
        r <- c_set_folder_name devptr folder_ptr name_ptr
        unless (r == 0) (checkError h)

------------------------------------------------------------------------------
-- Playlist management
------------------------------------------------------------------------------

-- Marshall a Playlist into a Playlist_t pointer using temporary storage.
withPlaylistPtr :: Playlist -> (Ptr Playlist_t -> IO a) -> IO a
withPlaylistPtr pl = bracket alloc free
    where
        alloc = do
            ptr <- malloc :: IO (Ptr Playlist_t)
            pt <- marshall
            poke ptr pt
            return ptr
        marshall =
            withCAString (playlistName pl) $ \name_ptr ->
            withArray (map fromIntegral (playlistTracks pl)) $ \tracks_ptr ->
                return Playlist_t
                           { pt_playlist_id = fromIntegral (playlistID pl)
                           , pt_parent_id = fromIntegral (playlistParentID pl)
                           , pt_storage_id = fromIntegral (playlistStorageID pl)
                           , pt_name = name_ptr
                           , pt_tracks = tracks_ptr
                           , pt_no_tracks = fromIntegral (playlistNoTracks pl)
                           , pt_next = nullPtr
                           }

peekPlaylist :: Ptr Playlist_t -> IO [Playlist]
peekPlaylist = go []
    where
        go acc p =
            if p == nullPtr
               then return acc
               else do
                   pt <- peek p
                   pn <- convert pt
                   free p
                   go (pn : acc) (pt_next pt)
        convert pt = do
            name <- peekCString (pt_name pt)
            let no_tracks = fromIntegral (pt_no_tracks pt)
            tracks <- peekArray no_tracks (pt_tracks pt)
            return $! Playlist { playlistID = fromIntegral (pt_playlist_id pt)
                               , playlistParentID = fromIntegral (pt_parent_id pt)
                               , playlistStorageID = fromIntegral (pt_storage_id pt)
                               , playlistName = name
                               , playlistTracks = map fromIntegral tracks
                               , playlistNoTracks = no_tracks }

-- | Get a list of playlists on the device.
getPlaylistList :: MTPHandle -> IO [Playlist]
getPlaylistList h = withMTPHandle h $ \devptr ->
    peekPlaylist =<< c_get_playlist_list devptr

-- | Get a single playlist by ID.
getPlaylist :: MTPHandle -> Int -> IO (Maybe Playlist)
getPlaylist h plid = withMTPHandle h $ \devptr -> do
    r <- peekPlaylist =<< c_get_playlist devptr (fromIntegral plid)
    return $ listToMaybe r

-- | Create a new playlist using the metadata supplied.
createPlaylist :: MTPHandle -> Playlist -> IO ()
createPlaylist h pl = withMTPHandle h $ \devptr ->
    withPlaylistPtr pl $ \plptr -> do
        r <- c_create_new_playlist devptr plptr
        unless (r == 0) (checkError h)

-- | Update an existing playlist.
updatePlaylist :: MTPHandle -> Playlist -> IO ()
updatePlaylist h pl = withMTPHandle h $ \devptr ->
    withPlaylistPtr pl $ \plptr -> do
        r <- c_update_playlist devptr plptr
        unless (r == 0) (checkError h)

-- | Rename an existing playlist. The expected name suffix is \".pla\".
setPlaylistName :: MTPHandle -> Playlist -> String -> IO ()
setPlaylistName h pl name = withMTPHandle h $ \devptr ->
    withPlaylistPtr pl $ \plptr ->
    withCAString name $ \nameptr -> do
        r <- c_set_playlist_name devptr plptr nameptr
        unless (r == 0) (checkError h)

------------------------------------------------------------------------------
-- Object management
------------------------------------------------------------------------------

-- | Delete a single file, track, playlist, folder or any other object.
deleteObject :: MTPHandle -> Int -> IO ()
deleteObject h i = withMTPHandle h $ \devptr -> do
    r <- c_delete_object devptr (fromIntegral i)
    unless (r == 0) (checkError h)

-- | Set the filename of any object.
setObjectName :: MTPHandle -> Int -> String -> IO ()
setObjectName h i n = withMTPHandle h $ \devptr ->
    withCAString n $ \name_ptr -> do
        r <- c_set_object_filename devptr (fromIntegral i) name_ptr
        unless (r == 0) (checkError h)

------------------------------------------------------------------------------
-- Extras
------------------------------------------------------------------------------

-- | Find the FileType for a given file name.
findFileType :: FilePath -> FileType
findFileType path =
    fromMaybe unknown (lookup (takeExtension path) tbl)
    where
        tbl = [(".wav", wav), (".mp3", mp3), (".wma", wma), (".ogg", ogg)
              ,(".aa", audible), (".mp4", mp4), (".wmv", wmv), (".avi", avi)
              ,(".mpg", mpeg), (".mpeg", mpeg), (".asf", asf), (".qt", qt)
              ,(".mov", qt), (".jpg", jpeg), (".jpeg", jpeg), (".jfif", jfif)
              ,(".tif", tiff), (".tiff", tiff), (".bmp", bmp), (".gif", gif)
              ,(".pict", pict), (".pct", pict), (".pic", pict), (".png", png)
              ,(".ics", vcalendar2), (".ical", vcalendar2), (".ifb", vcalendar2)
              ,(".icalendar", vcalendar2), (".vcard", vcard3), (".vcf", vcard3)
              ,(".wmf", windowsimageformat), (".exe", winexec), (".com", winexec)
              ,(".bat", winexec), (".dll", winexec), (".sys", winexec), (".txt", text)
              ,(".html", html), (".bin", firmware), (".aac", aac), (".flac", flac)
              ,(".mp2", mp2), (".m4a", m4a), (".doc", doc), (".xml", xml), (".xls", xls)
              ,(".ppt", ppt), (".mht", mht), (".jp2", jp2), (".jpx", jpx)]
