@ECHO OFF

IF "%1"==""       GOTO noarg
IF "%1"=="--help" GOTO help
IF "%1"=="-?"     GOTO help
IF "%1"=="/?"     GOTO help
GOTO installarg

:installarg
SET installdir=%1
GOTO check

:noarg
SET installdir=%CD%\..
GOTO check

:check
IF NOT EXIST %installdir%\bin\wx.pkg GOTO notfound

ECHO uninstall wxhaskell:

ECHO - unregister packages
ghc-pkg -r wx
ghc-pkg -r wxcore

ECHO.
ECHO - removing wxhaskell libraries from the windows system directory

SET wxclibpath=%WINDIR%\system\%wxclibname%.dll
IF "%OS%"=="Windows_NT" SET wxclibpath=%WINDIR%\system32\%wxclibname%.dll

IF EXIST %wxclibpath% GOTO removelib 
ECHO   (no libraries found)
GOTO done

:removelib
DEL /F %wxclibpath%
ECHO   (%wxclibpath% removed)
GOTO done

:done
ECHO.
ECHO - done! You can now safely remove the wxhaskell install directory.
ECHO.
GOTO end

:notfound
ECHO error:
ECHO   Unable to find the wxHaskell packages (wx.pkg). You have already
ECHO   uninstalled wxHaskell or are trying to uninstall from another
ECHO   directory than the install directory.
ECHO.
GOTO help

:help
ECHO usage:
ECHO   wxhaskell-unregister [^<installdir^>]
ECHO.
ECHO   (or just double click on the batch file from the explorer)
ECHO.
GOTO end

:end
PAUSE