@ECHO OFF
IF EXIST wx.pkg GOTO foundwx

ECHO error: already uninstalled or trying to uninstall from another 
ECHO        directory than the ghc install directory.
ECHO for example: c:\program files\ghc\ghc-6.0.1
ECHO.
PAUSE
goto end

:foundwx
ECHO uninstall wxhaskell:

ECHO - unregister packages
ghc-pkg -r wx
ghc-pkg -r wxcore

ECHO - deleting binary libraries
DEL wx.pkg wxcore.pkg
DEL libwx.a libwxcore.a libwxcore0.a 
DEL wx.o wxcore.o wxcore0.o 
DEL /Q libwxc*.a 
DEL /Q bin\wxc*.dll

ECHO - deleting haskell interface files
DEL imports\Graphics\UI\WX.hi
DEL imports\Graphics\UI\WXCore.hi
RMDIR /S /Q imports\Graphics\UI\WX
RMDIR /S /Q imports\Graphics\UI\WXCore

ECHO - deleting scripts
DEL wxhaskell-register wxhaskell-register.bat
ECHO done.
ECHO.
PAUSE
DEL wxhaskell-uninstall.bat

:end
