{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}

module MTP (
    -- * Types
    MTPHandle, Track(..), File(..),
    -- * Constants
    version,
    -- * Device management
    getFirstDevice, releaseDevice,
    withFirstDevice,
    getDeviceVersion,
    getFileListing,
    -- * Track management
    getTrackListing,
    getTrackToFile,
    sendTrackFromFile
    ) where

import Control.Exception
import Data.Typeable
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

#include <libmtp.h>

------------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------------

-- | MTP library version.
version :: Integer
version = #const LIBMTP_VERSION_STRING

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- An opaque type, only passed around between C funcs, but never
-- dereferenced on the Haskell side.
data MTPDevice

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
    alignment = sizeOf
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
    alignment = sizeOf
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

------------------------------------------------------------------------------
-- Foreign imports
------------------------------------------------------------------------------

foreign import ccall unsafe "LIBMTP_Get_First_Device" c_getFirstDevice
    :: IO (Ptr MTPDevice)

foreign import ccall unsafe "LIBMTP_Release_Device" c_releaseDevice
    :: (Ptr MTPDevice) -> IO ()

foreign import ccall unsafe "LIBMTP_Get_Deviceversion" c_getDeviceVersion
    :: (Ptr MTPDevice) -> IO CString

foreign import ccall unsafe "LIBMTP_Get_Filelisting" c_getFileListing
    :: (Ptr MTPDevice) -> IO (Ptr File_t)

foreign import ccall unsafe "LIBMTP_Get_Tracklisting" c_getTrackListing
    :: (Ptr MTPDevice) -> IO (Ptr Track_t)

foreign import ccall unsafe "LIBMTP_Get_Track_To_File" c_getTrackToFile
    :: (Ptr MTPDevice)
    -> CInt
    -> CString
    -> Ptr ()
    -> Ptr ()
    -> IO CInt

foreign import ccall unsafe "LIBMTP_Send_Track_From_File" c_sendTrackFromFile
    :: (Ptr MTPDevice)
    -> CString
    -> (Ptr Track_t)
    -> Ptr ()
    -> Ptr ()
    -> IO CInt

------------------------------------------------------------------------------
-- High-level interface
------------------------------------------------------------------------------

-- | MTP exceptions.
data MTPException = NoDevice
    deriving (Eq, Show, Typeable)

instance Exception MTPException where

-- | File metadata.
data File = File
    { fileID :: Int
    , fileParentID :: Int
    , fileStorageID :: Int
    , fileName :: String
    , fileSize :: Integer
    , fileType :: Int
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
    , trackFileType :: Int
    } deriving (Eq, Show)

-- | A handle to an MTP device connection.
data MTPHandle = MTPHandle !(ForeignPtr MTPDevice)
    deriving (Eq, Ord, Show)

-- A helper that lifts operations on MTPDevice into MTPHandle
withMTPHandle :: MTPHandle -> (Ptr MTPDevice -> IO a) -> IO a
withMTPHandle (MTPHandle h) f = withForeignPtr h $ \devptr -> f devptr

-- Marshall a Track into a Track_t pointer using temporary storage.
withTrackPtr :: Track -> (Ptr Track_t -> IO a) -> IO a
withTrackPtr t = bracket alloc free
    where
        alloc = do
            ptr <- malloc :: IO (Ptr Track_t)
            tt <- conv
            poke ptr tt
            return ptr
        conv = withCAString (trackTitle t) $ \title_ptr ->
               withCAString (trackArtist t) $ \artist_ptr ->
               withCAString (trackComposer t) $ \composer_ptr ->
               withCAString (trackGenre t) $ \genre_ptr ->
               withCAString (trackAlbum t) $ \album_ptr ->
               withCAString (trackDate t) $ \date_ptr ->
               withCAString (trackFileName t) $ \filename_ptr -> do
                   return $ Track_t
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
                              , tt_filetype = fromIntegral (trackFileType t)
                              , tt_next = nullPtr
                              }

-- | Open a connection to the first available MTP device and run an
-- action, closing the connection afterwards.
withFirstDevice :: (MTPHandle -> IO a) -> IO a
withFirstDevice = bracket getFirstDevice releaseDevice

-- | Connect to the first available MTP device.
getFirstDevice :: IO MTPHandle
getFirstDevice = do
    devptr <- c_getFirstDevice
    if devptr == nullPtr
       then throw NoDevice
       else do
           dev <- newForeignPtr finalizerFree devptr
           return (MTPHandle dev)

-- | Close connection to a MTP device.
releaseDevice :: MTPHandle -> IO ()
releaseDevice h = withMTPHandle h c_releaseDevice

-- | Get device hardware and firmware version.
getDeviceVersion :: MTPHandle -> IO String
getDeviceVersion h = withMTPHandle h $ \ptr -> do
   v <- c_getDeviceVersion ptr
   peekCString v

-- | Get a list of all files stored on the device.
getFileListing :: MTPHandle -> IO [File]
getFileListing h = withMTPHandle h $ \ptr -> do
    toList [] =<< c_getFileListing ptr
    where
        toList acc p = do
            if p == nullPtr
               then return acc
               else do
                   ft <- peek p
                   fn <- convert ft
                   free p
                   toList (fn : acc) (ft_next ft)
        convert ft = do
            n <- peekCString (ft_filename ft)
            return $ File { fileID = fromIntegral (ft_item_id ft)
                          , fileParentID = fromIntegral (ft_parent_id ft)
                          , fileStorageID = fromIntegral (ft_storage_id ft)
                          , fileName = n
                          , fileSize = fromIntegral (ft_filesize ft)
                          , fileType = fromIntegral (ft_filetype ft)
                          }

-- | Get a list of all tracks stored on the device.
getTrackListing :: MTPHandle -> IO [Track]
getTrackListing h = withMTPHandle h $ \ptr -> do
    toList [] =<< c_getTrackListing ptr
    where
        toList acc p = do
            if p == nullPtr
               then return acc
               else do
                   tt <- peek p
                   tn <- convert tt
                   free p
                   toList (tn : acc) (tt_next tt)
        convert :: Track_t -> IO Track
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
                            , trackFileType = fromIntegral (tt_filetype tt)
                            }

-- | Copy a track from the device to a local file.
getTrackToFile :: MTPHandle -> Int -> FilePath -> IO CInt
getTrackToFile h i n = withMTPHandle h $ \devptr -> do
    withCAString n $ \strptr -> do
        r <- c_getTrackToFile devptr (fromIntegral i) strptr nullPtr nullPtr
        return r

-- | Send a local file to the device, using the supplied metadata.
sendTrackFromFile :: MTPHandle -> FilePath -> Track -> IO CInt
sendTrackFromFile h n t = withMTPHandle h $ \devptr -> do
    withCAString n $ \strptr -> withTrackPtr t $ \tt_ptr ->
        c_sendTrackFromFile devptr strptr tt_ptr nullPtr nullPtr
