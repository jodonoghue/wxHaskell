@ECHO OFF

IF "%1"==""       GOTO noarg
IF "%1"=="--help" GOTO help
IF "%1"=="-?"     GOTO help
IF "%1"=="/?"     GOTO help

:noarg
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

:help
ECHO usage:
ECHO   wxhaskell-unregister [^<installdir^>]
ECHO.
ECHO   (or just double click on the batch file from the explorer)
ECHO.
GOTO end

:end
PAUSE