#!/bin/sh

LIBDIR=$(ghc6 --print-libdir)

/usr/bin/ghc-pkg6 update - < ${LIBDIR}/package.conflets/wxcore.pkg
/usr/bin/ghc-pkg6 update - < ${LIBDIR}/package.conflets/wx.pkg
