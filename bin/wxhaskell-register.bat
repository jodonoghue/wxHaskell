@ECHO OFF
IF "%1"==""       GOTO noarg
IF "%1"=="--help" GOTO help
IF "%1"=="-?"     GOTO help
IF "%1"=="/?"     GOTO help
GOTO installarg

:installarg
SET installdir=%1
ghc-pkg -u -i "%1\wxcore.pkg"
ghc-pkg -u -i "%1\wx.pkg"
PAUSE
GOTO end

:noarg
SET installdir=%CD%
ghc-pkg -u -i wxcore.pkg
ghc-pkg -u -i wx.pkg
PAUSE
GOTO end

:help
ECHO usage:
ECHO   wxhaskell-register [^<installdir^>]
ECHO.
GOTO end

:end