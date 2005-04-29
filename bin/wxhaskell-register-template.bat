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
COPY /B /Y setcd setcd.bat
CD ..
CD >> bin\setcd.bat
CD bin
CALL setcd.bat
DEL setcd.bat
SET installdir=%CURDIR%
GOTO check

:check
IF "%generate%"=="yes" GOTO generate
GOTO checkfile

:generate
IF NOT EXIST %installdir%\bin\wx-partial.pkg GOTO notfound
COPY /Y %installdir%\bin\wx-partial.pkg %installdir%\bin\wx.pkg
ECHO import-dirs:%installdir%\lib\imports >> %installdir%\bin\wx.pkg
ECHO library-dirs:%installdir%\lib        >> %installdir%\bin\wx.pkg
COPY /Y %installdir%\bin\wxcore-partial.pkg %installdir%\bin\wxcore.pkg
ECHO import-dirs:%installdir%\lib\imports >> %installdir%\bin\wxcore.pkg
ECHO library-dirs:%installdir%\lib        >> %installdir%\bin\wxcore.pkg
GOTO checkfile

:checkfile
IF NOT EXIST "%installdir%\bin\wx.pkg" GOTO notfound

:install
ECHO - register packages
SET wxhlibdir=%installdir%\lib
${hcregister} "%installdir%\bin\wxcore.pkg"
${hcregister} "%installdir%\bin\wx.pkg"

IF ERRORLEVEL 1 GOTO regerror

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

:regerror
ECHO error:
ECHO   Unable to register the package using "ghc-pkg". 
ECHO   Maybe you have an incompatible version of ghc installed?
ECHO.
goto end

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
ECHO   Unable to find the wxHaskell packages (bin\wx.pkg)
ECHO   Maybe the installation directory is not properly specified?
ECHO.
GOTO help

:help
ECHO usage:
ECHO   wxhaskell-register [installdir]
ECHO.
ECHO (or just double click on the batch file from the explorer)
ECHO.
GOTO end

:end
PAUSE
