#!/bin/bash

# You have to download wxhaskell-doc-0.10.3.zip from
# http://sourceforge.net/project/showfiles.php?group_id=73133&package_id=73173&release_id=582361
# and place it in wxhaskell/ . This is neccesarry as we cannot currently
# build the Haddock documentation on Debian.

# This script must be run as root :(

PREFIX=/usr/local

case "$1" in
   6.6) 
        chmod 755 configure
        ./configure --enable-split-objs --hcprof --prefix=$PREFIX
        make
        make install
        make wx
        make wx-install

        make debdist
   ;;

   6.8)
        chmod 755 configure
        runhaskell Setup configure --enable-split-objs --hcprof --prefix=$PREFIX
        runhaskell Setup build
        runhaskell Setup install

        cd wx
        chmod 755 configure
        runhaskell Setup configure --enable-split-objs --enable-library-profiling --prefix=$PREFIX
        runhaskell Setup build
        runhaskell Setup install

        cd ..
        make debdist
   ;;

   *)
        echo "You must specify either 6.6 or 6.8 as parameter"
   ;;

esac


