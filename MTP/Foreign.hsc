{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

-- |
-- Module      : MTP.Foreign
-- Copyright   : (c) Joachim Fasting 2010
-- License     : LGPL
--
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : unstable
-- Portability : not portable
--
-- Bindings to libmtp. Foreign interface.

module MTP.Foreign where

import Foreign
import Foreign.C

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
 ,jpx = LIBMTP_FILETYPE_JPX
 ,album = LIBMTP_FILETYPE_ALBUM
 ,playlist = LIBMTP_FILETYPE_PLAYLIST
 ,unknown = LIBMTP_FILETYPE_UNKNOWN
 }

-- | Storage order.
newtype StorageOrder = StorageOrder { unStorageOrder :: CInt }
    deriving (Eq, Show)

notsorted, freespace, maxspace :: StorageOrder
notsorted = StorageOrder #{const LIBMTP_STORAGE_SORTBY_NOTSORTED}
freespace = StorageOrder #{const LIBMTP_STORAGE_SORTBY_FREESPACE}
maxspace  = StorageOrder #{const LIBMTP_STORAGE_SORTBY_MAXSPACE}

-- Error code enumeration.
newtype ErrorCode = ErrorCode { unErrorCode :: CInt }
    deriving (Eq, Show)

#{enum ErrorCode, ErrorCode
 ,general = LIBMTP_ERROR_GENERAL
 ,noDevice = LIBMTP_ERROR_NO_DEVICE_ATTACHED
 ,storageFull = LIBMTP_ERROR_STORAGE_FULL
 ,connectionFailed = LIBMTP_ERROR_CONNECTING
 ,cancelled = LIBMTP_ERROR_CANCELLED
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
        alb <- #{peek LIBMTP_track_t, album} ptr
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
        return $! Track_t iid pid sid title artist composer genre alb
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

foreign import ccall unsafe "LIBMTP_Init" c_init :: IO ()

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
