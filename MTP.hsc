{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls,
             DeriveDataTypeable #-}

-- |
-- Module      : MTP
-- Copyright   : (c) Joachim Fasting
-- License     : LGPL
--
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : unstable
-- Portability : not portable
--
-- Bindings to libmtp.

module MTP (
    -- * Types
    MTPHandle, Track(..), File(..), Folder(..), Playlist(..), FileType,
    MTPException(..),
    -- * Constants
    version,
    wav, mp3, wma, ogg, audible, mp4, undef_audio, wmv, avi, mpeg, asf, qt,
    undef_video, jpeg, jfif, tiff, bmp, gif, pict, png, vcalendar1,
    vcalendar2, vcard2, vcard3, windowsimageformat, winexec, text, html,
    firmware, aac, mediacard, flac, mp2, m4a, doc, xml, xls, ppt, mht, jp2,
    unknown,
    -- * Device management
    getFirstDevice, releaseDevice, resetDevice,
    withFirstDevice,
    getDeviceVersion, getManufacturerName, getModelName, getSerialNumber,
    getFriendlyName, getBatteryLevel, getSupportedFileTypes,
    -- * File management
    getFileListing,
    getFile, sendFile,
    hGetFile, hSendFile,
    setFileName,
    -- * Track management
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
    deleteObject, setObjectName
    ) where

import Control.Exception
import Control.Monad
import GHC.Handle
import Data.Maybe
import Data.Typeable
import Foreign
import Foreign.C
import Foreign.Marshal.Alloc
import System.IO
import System.Posix.IO
import System.Posix.Types

#include <stdio.h>
#include <libmtp.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t(y__); }, y__)

------------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------------

-- | MTP library version.
version :: Integer
version = #const LIBMTP_VERSION_STRING

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- | Filetype enumeration.
newtype FileType = FileType { unFileType :: CInt }
    deriving (Eq, Ord, Show)

#{enum FileType, FileType
 ,wav = LIBMTP_FILETYPE_WAV
 ,mp3 = LIBMTP_FILETYPE_MP3
 ,wma = LIBMTP_FILETYPE_WMA
 ,ogg = LIBMTP_FILETYPE_OGG
 ,audible = LIBMTP_FILETYPE_AUDIBLE
 ,mp4 = LIBMTP_FILETYPE_MP4
 ,undef_audio = LIBMTP_FILETYPE_UNDEF_AUDIO
 ,wmv = LIBMTP_FILETYPE_WMV
 ,avi = LIBMTP_FILETYPE_AVI
 ,mpeg = LIBMTP_FILETYPE_MPEG
 ,asf = LIBMTP_FILETYPE_ASF
 ,qt = LIBMTP_FILETYPE_QT
 ,undef_video = LIBMTP_FILETYPE_UNDEF_VIDEO
 ,jpeg = LIBMTP_FILETYPE_JPEG
 ,jfif = LIBMTP_FILETYPE_JFIF
 ,tiff = LIBMTP_FILETYPE_TIFF
 ,bmp = LIBMTP_FILETYPE_BMP
 ,gif = LIBMTP_FILETYPE_GIF
 ,pict = LIBMTP_FILETYPE_PICT
 ,png = LIBMTP_FILETYPE_PNG
 ,vcalendar1 = LIBMTP_FILETYPE_VCALENDAR1
 ,vcalendar2 = LIBMTP_FILETYPE_VCALENDAR2
 ,vcard2 = LIBMTP_FILETYPE_VCARD2
 ,vcard3 = LIBMTP_FILETYPE_VCARD3
 ,windowsimageformat = LIBMTP_FILETYPE_WINDOWSIMAGEFORMAT
 ,winexec = LIBMTP_FILETYPE_WINEXEC
 ,text = LIBMTP_FILETYPE_TEXT
 ,html = LIBMTP_FILETYPE_HTML
 ,firmware = LIBMTP_FILETYPE_FIRMWARE
 ,aac = LIBMTP_FILETYPE_AAC
 ,mediacard = LIBMTP_FILETYPE_MEDIACARD
 ,flac = LIBMTP_FILETYPE_FLAC
 ,mp2 = LIBMTP_FILETYPE_MP2
 ,m4a = LIBMTP_FILETYPE_M4A
 ,doc = LIBMTP_FILETYPE_DOC
 ,xml = LIBMTP_FILETYPE_XML
 ,xls = LIBMTP_FILETYPE_XLS
 ,ppt = LIBMTP_FILETYPE_PPT
 ,mht = LIBMTP_FILETYPE_MHT
 ,jp2 = LIBMTP_FILETYPE_JP2
 ,unknown = LIBMTP_FILETYPE_UNKNOWN
 }

-- Opaque types, only passed around between C funcs, but never
-- dereferenced on the Haskell side.
data MTPDevice
data Data
data Callback

-- An intermediate structure for the LIBMTP_error_t struct.
data Error_t = Error_t
    { et_errornumber :: CInt
    , et_errortext :: CString
    , et_next :: Ptr Error_t
    }

instance Storable Error_t where
    sizeOf _ = #{size LIBMTP_error_t}
    alignment _ = #{alignment LIBMTP_error_t}
    peek ptr = do
        en <- #{peek LIBMTP_error_t, errornumber} ptr
        et <- #{peek LIBMTP_error_t, error_text} ptr
        next <- #{peek LIBMTP_error_t, next} ptr
        return $! Error_t { et_errornumber = en
                          , et_errortext = et
                          , et_next = next
                          }

-- An intermediate structure for the LIBMTP_file_t struct.
-- We need this because LIBMPT_file_t is recursive.
data File_t = File_t
    { ft_item_id :: CInt
    , ft_parent_id :: CInt
    , ft_storage_id :: CInt
    , ft_filename :: CString
    , ft_filesize :: CInt
    , ft_filetype :: CInt
    , ft_next :: Ptr File_t
    }

instance Storable File_t where
    sizeOf _ = #{size LIBMTP_file_t}
    alignment _ = #{alignment LIBMTP_file_t}
    peek ptr = do
        fid  <- #{peek LIBMTP_file_t, item_id} ptr
        pid  <- #{peek LIBMTP_file_t, parent_id} ptr
        sid  <- #{peek LIBMTP_file_t, storage_id} ptr
        name <- #{peek LIBMTP_file_t, filename} ptr
        size <- #{peek LIBMTP_file_t, filesize} ptr
        typ  <- #{peek LIBMTP_file_t, filetype} ptr
        next <- #{peek LIBMTP_file_t, next} ptr
        return $! File_t fid pid sid name size typ next
    poke ptr ft = do
        #{poke LIBMTP_file_t, item_id} ptr (ft_item_id ft)
        #{poke LIBMTP_file_t, parent_id} ptr (ft_parent_id ft)
        #{poke LIBMTP_file_t, filename} ptr (ft_filename ft)
        #{poke LIBMTP_file_t, filesize} ptr (ft_filesize ft)
        #{poke LIBMTP_file_t, next} ptr (ft_next ft)

-- An intermediate structure for the LIBMTP_track_t struct.
-- We need this because LIBMTP_track_t is recursive.
data Track_t = Track_t
    { tt_item_id :: CInt
    , tt_parent_id :: CInt
    , tt_storage_id :: CInt
    , tt_title :: CString
    , tt_artist :: CString
    , tt_composer :: CString
    , tt_genre :: CString
    , tt_album :: CString
    , tt_date :: CString
    , tt_filename :: CString
    , tt_tracknumber :: CInt
    , tt_duration :: CInt
    , tt_samplerate :: CInt
    , tt_nochannels :: CInt
    , tt_wavecodec :: CInt
    , tt_bitrate :: CInt
    , tt_bitratetype :: CInt
    , tt_rating :: CInt
    , tt_usecount :: CInt
    , tt_filesize :: CInt
    , tt_filetype :: CInt
    , tt_next :: Ptr Track_t
    }

instance Storable Track_t where
    sizeOf _ = #{size LIBMTP_track_t}
    alignment _ = #{alignment LIBMTP_track_t}
    peek ptr = do
        iid <- #{peek LIBMTP_track_t, item_id} ptr
        pid <- #{peek LIBMTP_track_t, parent_id} ptr
        sid <- #{peek LIBMTP_track_t, storage_id} ptr
        title <- #{peek LIBMTP_track_t, title} ptr
        artist <- #{peek LIBMTP_track_t, artist} ptr
        composer <- #{peek LIBMTP_track_t, composer} ptr
        genre <- #{peek LIBMTP_track_t, genre} ptr
        album <- #{peek LIBMTP_track_t, album} ptr
        date <- #{peek LIBMTP_track_t, date} ptr
        fname <- #{peek LIBMTP_track_t, filename} ptr
        trackno <- #{peek LIBMTP_track_t, tracknumber} ptr
        dur <- #{peek LIBMTP_track_t, duration} ptr
        srate <- #{peek LIBMTP_track_t, samplerate} ptr
        chans <- #{peek LIBMTP_track_t, nochannels} ptr
        codec <- #{peek LIBMTP_track_t, wavecodec} ptr
        brate <- #{peek LIBMTP_track_t, bitrate} ptr
        bratetype <- #{peek LIBMTP_track_t, bitratetype} ptr
        rating <- #{peek LIBMTP_track_t, rating} ptr
        count <- #{peek LIBMTP_track_t, usecount} ptr
        fsize <- #{peek LIBMTP_track_t, filesize} ptr
        ftype <- #{peek LIBMTP_track_t, filetype} ptr
        next <- #{peek LIBMTP_track_t, next} ptr
        return $! Track_t iid pid sid title artist composer genre album
                          date fname trackno dur srate chans codec brate
                          bratetype rating count fsize ftype next
    poke ptr tt = do
        #{poke LIBMTP_track_t, item_id} ptr (tt_item_id tt)
        #{poke LIBMTP_track_t, parent_id} ptr (tt_parent_id tt)
        #{poke LIBMTP_track_t, storage_id} ptr (tt_storage_id tt)
        #{poke LIBMTP_track_t, title} ptr (tt_title tt)
        #{poke LIBMTP_track_t, artist} ptr (tt_artist tt)
        #{poke LIBMTP_track_t, composer} ptr (tt_composer tt)
        #{poke LIBMTP_track_t, genre} ptr (tt_genre tt)
        #{poke LIBMTP_track_t, album} ptr (tt_album tt)
        #{poke LIBMTP_track_t, date} ptr (tt_date tt)
        #{poke LIBMTP_track_t, filename} ptr (tt_filename tt)
        #{poke LIBMTP_track_t, tracknumber} ptr (tt_tracknumber tt)
        #{poke LIBMTP_track_t, duration} ptr (tt_duration tt)
        #{poke LIBMTP_track_t, samplerate} ptr (tt_samplerate tt)
        #{poke LIBMTP_track_t, nochannels} ptr (tt_nochannels tt)
        #{poke LIBMTP_track_t, wavecodec} ptr (tt_wavecodec tt)
        #{poke LIBMTP_track_t, bitrate} ptr (tt_bitrate tt)
        #{poke LIBMTP_track_t, bitratetype} ptr (tt_bitratetype tt)
        #{poke LIBMTP_track_t, rating} ptr (tt_rating tt)
        #{poke LIBMTP_track_t, usecount} ptr (tt_usecount tt)
        #{poke LIBMTP_track_t, filesize} ptr (tt_filesize tt)
        #{poke LIBMTP_track_t, filetype} ptr (tt_filetype tt)
        #{poke LIBMTP_track_t, next} ptr (tt_next tt)

-- An intermediate structure for the LIBMTP_folder_t struct.
-- We need this because LIBMTP_folder_t is recursive.
data Folder_t = Folder_t
    { fdt_folder_id :: CInt
    , fdt_parent_id :: CInt
    , fdt_storage_id :: CInt
    , fdt_name :: CString
    , fdt_sibling :: Ptr Folder_t
    , fdt_child :: Ptr Folder_t
    }

instance Storable Folder_t where
    sizeOf _ = #{size LIBMTP_folder_t}
    alignment _ = #{alignment LIBMTP_folder_t}
    peek ptr = do
        fid <- #{peek LIBMTP_folder_t, folder_id} ptr
        pid <- #{peek LIBMTP_folder_t, parent_id} ptr
        sid <- #{peek LIBMTP_folder_t, storage_id} ptr
        name <- #{peek LIBMTP_folder_t, name} ptr
        sibling <- #{peek LIBMTP_folder_t, sibling} ptr
        child <- #{peek LIBMTP_folder_t, child} ptr
        return $! Folder_t { fdt_folder_id = fid
                           , fdt_parent_id = pid
                           , fdt_storage_id = sid
                           , fdt_name = name
                           , fdt_sibling = sibling
                           , fdt_child = child
                           }
    poke ptr fdt = do
        #{poke LIBMTP_folder_t, folder_id} ptr (fdt_folder_id fdt)
        #{poke LIBMTP_folder_t, parent_id} ptr (fdt_parent_id fdt)
        #{poke LIBMTP_folder_t, storage_id} ptr (fdt_storage_id fdt)
        #{poke LIBMTP_folder_t, name} ptr (fdt_name fdt)
        #{poke LIBMTP_folder_t, sibling} ptr (fdt_sibling fdt)
        #{poke LIBMTP_folder_t, child} ptr (fdt_child fdt)

-- An intermediate structure for the LIBMTP_playlist_t struct.
-- We need this because LIBMTP_playlist_t is recursive.
data Playlist_t = Playlist_t
    { pt_playlist_id :: CInt
    , pt_parent_id :: CInt
    , pt_storage_id :: CInt
    , pt_name :: CString
    , pt_tracks :: Ptr CInt
    , pt_no_tracks :: CInt
    , pt_next :: Ptr Playlist_t
    }

instance Storable Playlist_t where
    sizeOf _ = #{size LIBMTP_playlist_t}
    alignment _ = #{alignment LIBMTP_playlist_t}
    peek ptr = do
        plid <- #{peek LIBMTP_playlist_t, playlist_id} ptr
        pid <- #{peek LIBMTP_playlist_t, parent_id} ptr
        sid <- #{peek LIBMTP_playlist_t, storage_id} ptr
        name <- #{peek LIBMTP_playlist_t, name} ptr
        tracks <- #{peek LIBMTP_playlist_t, tracks} ptr
        ntracks <- #{peek LIBMTP_playlist_t, no_tracks} ptr
        next <- #{peek LIBMTP_playlist_t, next} ptr
        return $ Playlist_t plid pid sid name tracks ntracks next
    poke ptr pt = do
        #{poke LIBMTP_playlist_t, playlist_id} ptr (pt_playlist_id pt)
        #{poke LIBMTP_playlist_t, parent_id} ptr (pt_parent_id pt)
        #{poke LIBMTP_playlist_t, storage_id} ptr (pt_storage_id pt)
        #{poke LIBMTP_playlist_t, name} ptr (pt_name pt)
        #{poke LIBMTP_playlist_t, tracks} ptr (pt_tracks pt)
        #{poke LIBMTP_playlist_t, no_tracks} ptr (pt_no_tracks pt)
        #{poke LIBMTP_playlist_t, next} ptr (pt_next pt)

------------------------------------------------------------------------------
-- Foreign imports
------------------------------------------------------------------------------

foreign import ccall unsafe "fdopen" fdopen :: Fd -> CString -> IO (Ptr CFile)

foreign import ccall unsafe "fflush" fflush :: Ptr CFile -> IO ()

foreign import ccall unsafe "fclose" fclose :: Ptr CFile -> IO ()

foreign import ccall unsafe "LIBMTP_Get_First_Device" c_get_first_device
    :: IO (Ptr MTPDevice)

foreign import ccall unsafe "LIBMTP_Release_Device" c_release_device
    :: Ptr MTPDevice -> IO ()

foreign import ccall unsafe "LIBMTP_Reset_Device" c_reset_device
    :: Ptr MTPDevice
    -> IO CInt

foreign import ccall unsafe "LIBMTP_Get_Errorstack" c_get_errorstack
    :: Ptr MTPDevice
    -> IO (Ptr Error_t)

foreign import ccall unsafe "LIBMTP_Clear_Errorstack" c_clear_errorstack
    :: Ptr MTPDevice
    -> IO ()

foreign import ccall unsafe "LIBMTP_Get_Manufacturername"
    c_get_manufacturername :: Ptr MTPDevice -> IO CString

foreign import ccall unsafe "LIBMTP_Get_Modelname"
    c_get_modelname :: Ptr MTPDevice -> IO CString

foreign import ccall unsafe "LIBMTP_Get_Serialnumber"
    c_get_serialnumber :: Ptr MTPDevice -> IO CString

foreign import ccall unsafe "LIBMTP_Get_Friendlyname"
    c_get_friendlyname :: Ptr MTPDevice -> IO CString

foreign import ccall unsafe "LIBMTP_Get_Deviceversion" c_get_deviceversion
    :: Ptr MTPDevice -> IO CString

foreign import ccall unsafe "LIBMTP_Get_Batterylevel" c_get_batterylevel
    :: Ptr MTPDevice
    -> Ptr CInt
    -> Ptr CInt
    -> IO CInt

foreign import ccall unsafe "LIBMTP_Get_Supported_Filetypes"
    c_get_supported_filetypes :: Ptr MTPDevice
                              -> Ptr CInt
                              -> Ptr CInt
                              -> IO CInt

foreign import ccall unsafe "LIBMTP_Get_Filelisting_With_Callback" c_get_filelisting
    :: Ptr MTPDevice
    -> Ptr Callback
    -> Ptr Data
    -> IO (Ptr File_t)

foreign import ccall unsafe "LIBMTP_Track_Exists" c_track_exists
    :: Ptr MTPDevice
    -> CInt
    -> IO CInt

foreign import ccall unsafe "LIBMTP_Get_File_To_File" c_get_file_to_file
    :: Ptr MTPDevice
    -> CInt
    -> CString
    -> Ptr Callback
    -> Ptr Data
    -> IO CInt

foreign import ccall unsafe "LIBMTP_Send_File_From_File" c_send_file_from_file
    :: Ptr MTPDevice
    -> CString
    -> Ptr Callback
    -> Ptr Data
    -> IO CInt

foreign import ccall unsafe "LIBMTP_Get_File_To_File_Descriptor"
    c_get_file_to_file_descriptor :: Ptr MTPDevice
                                  -> CInt
                                  -> Ptr CFile
                                  -> Ptr Callback
                                  -> Ptr Data
                                  -> IO CInt

foreign import ccall unsafe "LIBMTP_Send_File_From_File_Descriptor"
    c_send_file_from_file_descriptor :: Ptr MTPDevice
                                     -> Ptr CFile
                                     -> Ptr File_t
                                     -> Ptr Callback
                                     -> Ptr Data
                                     -> IO CInt

foreign import ccall unsafe "LIBMTP_Set_File_Name" c_set_file_name
    :: Ptr MTPDevice
    -> Ptr File_t
    -> CString
    -> IO CInt

foreign import ccall unsafe "LIBMTP_Get_Tracklisting_With_Callback" c_get_tracklisting
    :: Ptr MTPDevice
    -> Ptr Callback
    -> Ptr Data
    -> IO (Ptr Track_t)

foreign import ccall unsafe "LIBMTP_Get_Track_To_File" c_get_track_to_file
    :: Ptr MTPDevice
    -> CInt
    -> CString
    -> Ptr Callback
    -> Ptr Data
    -> IO CInt

foreign import ccall unsafe "LIBMTP_Send_Track_From_File" c_send_track_from_file
    :: Ptr MTPDevice
    -> CString
    -> (Ptr Track_t)
    -> Ptr Callback
    -> Ptr Data
    -> IO CInt

foreign import ccall unsafe "LIBMTP_Send_Track_From_File_Descriptor"
   c_send_track_from_file_descriptor :: Ptr MTPDevice
                                     -> Ptr CFile
                                     -> Ptr Track_t
                                     -> Ptr Callback
                                     -> Ptr Data
                                     -> IO CInt

foreign import ccall unsafe "LIBMTP_Get_Track_To_File_Descriptor"
   c_get_track_to_file_descriptor :: Ptr MTPDevice
                                  -> CInt
                                  -> Ptr CFile
                                  -> Ptr Callback
                                  -> Ptr Data
                                  -> IO CInt

foreign import ccall unsafe "LIBMTP_Get_Trackmetadata"
    c_get_trackmetadata :: Ptr MTPDevice
                        -> CInt
                        -> IO (Ptr Track_t)

foreign import ccall unsafe "LIBMTP_Update_Track_Metadata"
    c_update_track_metadata :: Ptr MTPDevice
                            -> Ptr Track_t
                            -> IO CInt

foreign import ccall unsafe "LIBMTP_Set_Track_Name" c_set_track_name
    :: Ptr MTPDevice
    -> Ptr Track_t
    -> CString
    -> IO CInt

foreign import ccall unsafe "LIBMTP_Create_Folder" c_create_folder
    :: Ptr MTPDevice
    -> CString
    -> CInt
    -> CInt
    -> IO CInt

foreign import ccall unsafe "LIBMTP_Get_Folder_List" c_folder_list
    :: Ptr MTPDevice
    -> IO (Ptr Folder_t)

foreign import ccall unsafe "LIBMTP_Set_Folder_Name" c_set_folder_name
    :: Ptr MTPDevice
    -> Ptr Folder_t
    -> CString
    -> IO CInt

foreign import ccall unsafe "LIBMTP_Get_Playlist_List"
    c_get_playlist_list :: Ptr MTPDevice
                        -> IO (Ptr Playlist_t)

foreign import ccall unsafe "LIBMTP_Get_Playlist"
    c_get_playlist :: Ptr MTPDevice
                   -> CInt
                   -> IO (Ptr Playlist_t)

foreign import ccall unsafe "LIBMTP_Create_New_Playlist"
    c_create_new_playlist :: Ptr MTPDevice
                          -> Ptr Playlist_t
                          -> IO CInt

foreign import ccall unsafe "LIBMTP_Update_Playlist"
    c_update_playlist :: Ptr MTPDevice
                      -> Ptr Playlist_t
                      -> IO CInt

foreign import ccall unsafe "LIBMTP_Set_Playlist_Name"
    c_set_playlist_name :: Ptr MTPDevice
                        -> Ptr Playlist_t
                        -> CString
                        -> IO CInt

foreign import ccall unsafe "LIBMTP_Delete_Object"
    c_delete_object :: Ptr MTPDevice
                    -> CInt
                    -> IO CInt

foreign import ccall unsafe "LIBMTP_Set_Object_Filename"
    c_set_object_filename :: Ptr MTPDevice
                          -> CInt
                          -> CString
                          -> IO CInt

------------------------------------------------------------------------------
-- High-level interface
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

-- | A handle to an MTP device connection.
data MTPHandle = MTPHandle !(ForeignPtr MTPDevice)
    deriving (Eq, Show)

-- XXX: should check that the handle is usable
-- A helper that lifts operations on MTPDevice into MTPHandle
withMTPHandle :: MTPHandle -> (Ptr MTPDevice -> IO a) -> IO a
withMTPHandle (MTPHandle h) = withForeignPtr h

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
        case et_errornumber et of
            #{const LIBMTP_ERROR_GENERAL} -> throw $ General es
            #{const LIBMTP_ERROR_NO_DEVICE_ATTACHED} -> throw NoDevice
            #{const LIBMTP_ERROR_STORAGE_FULL} -> throw StorageFull
            #{const LIBMTP_ERROR_CONNECTING} -> throw ConnectionFailed
            #{const LIBMTP_ERROR_CANCELLED} -> throw Cancelled
            x -> error $ "checkError: unhandled error number: " ++ show x

-- Convert a Handle to a CFile.
-- Source <http://haskell.org/haskellwiki/The_Monad.Reader/Issue2/Bzlib2Binding>
handleToCFile :: Handle -> String -> IO (Ptr CFile)
handleToCFile h m = withCAString m $ \iomode -> do
    -- Create a duplicate so the original handle is kept open
    h' <- hDuplicate h
    fd <- handleToFd h'
    fdopen fd iomode

-- | Connect to the first available MTP device.
getFirstDevice :: IO MTPHandle
getFirstDevice = do
    devptr <- c_get_first_device
    if devptr == nullPtr
       then throw NoDevice
       else do
           -- XXX: newForeignPtr finalizerFree devptr causes a double free
           -- error when the finalizer is run.
           dev <- newForeignPtr_ devptr
           return (MTPHandle dev)

-- XXX: using the handle after running this causes a segmentation fault.
-- XXX: we should free the ptr, but it causes a double free error, WHY, OH WHY
-- | Close connection to a MTP device.
releaseDevice :: MTPHandle -> IO ()
releaseDevice h = withMTPHandle h c_release_device

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

-- | Test whether a track exists on the device.
doesTrackExist :: MTPHandle -> Int -> IO Bool
doesTrackExist h i = withMTPHandle h $ \devptr -> do
    exists <- c_track_exists devptr (fromIntegral i)
    return $ exists /= 0

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
