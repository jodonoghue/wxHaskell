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

:install
ECHO - register packages
ghc-pkg -u -i "%installdir%\bin\wxcore.pkg"
ghc-pkg -u -i "%installdir%\bin\wx.pkg"

ECHO.
IF "%OS%"=="Windows_NT" GOTO copyNT
ECHO - copy %wxclibname%.dll to the windows system directory (%WINDIR%\system)
COPY %installdir%\lib\%wxclibname%.dll %WINDIR%\system\%wxclibname%.dll
GOTO checkcopy

:copyNT
ECHO - copy %wxclibname%.dll to the windows system directory (%WINDIR%\system32)
COPY %installdir%\lib\%wxclibname%.dll %WINDIR%\system32\%wxclibname%.dll

:checkcopy
IF ERRORLEVEL 1 GOTO copyerror

ECHO - done!
GOTO end

:copyerror
ECHO error:
ECHO   Unable to copy dynamic link libraries to the system folder. 
ECHO   This is not a real problem but you will have to add the wxHaskell 
ECHO   library directory to your PATH environment variable. 
ECHO.
ECHO   The library directory is: %installdir%\lib
ECHO.
goto end

:notfound
ECHO error:
ECHO   unable to find the wxHaskell packages (wx.pkg)
ECHO.
GOTO help

:help
ECHO usage:
ECHO   wxhaskell-register [^<installdir^>]
ECHO.
ECHO (or just double click on the batch file from the explorer)
ECHO.
GOTO end

:end
PAUSE