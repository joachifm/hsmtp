# About
A FFI binding to [libmtp], an implementation of the Media Transfer Protocol.

[libmtp]: http://libmtp.sourceforge.net

# Getting
* [Latest release]
* `git clone git://github.com/joachifm/hsmtp.git`

[Latest release]: http://hackage.haskell.org/package/mtp

# Building
The preferred method of building is using [cabal-install]:

    $ cd hsmtp
    $ cabal configure
    $ cabal build
    $ cabal install

[cabal-install]: http://hackage.haskell.org/package/cabal-install

# Usage

    > import qualified MTP
    > main = do
    >     MTP.init
    >     MTP.withFirstDevice MTP.getDeviceVersion

# Author
Joachim Fasting \<joachim.fasting@gmail.com\>

# Licence
LGPL version 2.1 (see COPYING)
