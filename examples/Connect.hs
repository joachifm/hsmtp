-- | An example program that simply connects to the first available
-- MTP device and printing the device's firmware version.

import MTP
import MTP.Handle

main = do
    h <- getFirstDevice
    print =<< getDeviceVersion h
    releaseDevice h
    closed <- isClosed h
    print closed
    print =<< getDeviceVersion h
