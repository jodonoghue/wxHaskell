# Microsoft Developer Studio Project File - Name="wxc" - Package Owner=<4>r
# Microsoft Developer Studio Generated Build File, Format Version 6.00r
# ** DO NOT EDIT **r
r
# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102r
r
CFG=wxc - Win32 Debugr
!MESSAGE This is not a valid makefile. To build this project using NMAKE,r
!MESSAGE use the Export Makefile command and runr
!MESSAGE r
!MESSAGE NMAKE /f "wxc.mak".r
!MESSAGE r
!MESSAGE You can specify a configuration when running NMAKEr
!MESSAGE by defining the macro CFG on the command line. For example:r
!MESSAGE r
!MESSAGE NMAKE /f "wxc.mak" CFG="wxc - Win32 Debug"r
!MESSAGE r
!MESSAGE Possible choices for configuration are:r
!MESSAGE r
!MESSAGE "wxc - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")r
!MESSAGE "wxc - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")r
!MESSAGE r
r
# Begin Projectr
# PROP AllowPerConfigDependencies 0r
# PROP Scc_ProjName ""r
# PROP Scc_LocalPath ""r
CPP=cl.exer
MTL=midl.exer
RSC=rc.exer
r
!IF  "$(CFG)" == "wxc - Win32 Release"r
r
# PROP BASE Use_MFC 0r
# PROP BASE Use_Debug_Libraries 0r
# PROP BASE Output_Dir "Release"r
# PROP BASE Intermediate_Dir "Release"r
# PROP BASE Target_Dir ""r
# PROP Use_MFC 0r
# PROP Use_Debug_Libraries 0r
# PROP Output_Dir "..\out\wxc"r
# PROP Intermediate_Dir "..\out\wxc\release"r
# PROP Ignore_Export_Lib 0r
# PROP Target_Dir ""r
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "WXC_EXPORTS" /YX /FD /cr
# ADD CPP /nologo /MT /W3 /GX /O2 /I "..\..\wxWindows-2.4.2\lib\msw" /I "include" /I "src\ewxw" /I "..\..\wxWindows-2.4.2\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "WXC_EXPORTS" /D WINVER=0x400 /D "_MT" /D wxUSE_GUI=1 /D BUILD_WXC=1 /D "_DLL" /YX /FD /cr
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32r
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32r
# ADD BASE RSC /l 0x413 /d "NDEBUG"r
# ADD RSC /l 0x409 /i "..\..\wxWindows-2.4.2\include" /d "NDEBUG"r
BSC32=bscmake.exer
# ADD BASE BSC32 /nologor
# ADD BSC32 /nologor
LINK32=link.exer
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386r
# ADD LINK32 zlib.lib regex.lib png.lib jpeg.lib tiff.lib wxmsw.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib comctl32.lib rpcrt4.lib wsock32.lib opengl32.lib winmm.lib /nologo /dll /machine:I386 /nodefaultlib:"LIBCMT" /out:"..\out\wxc\wxc-0.6.dll" /libpath:"..\..\wxWindows-2.4.2\lib"r
# Begin Special Build Toolr
SOURCE="$(InputPath)"r
PostBuild_Cmds=echo Generating mingw32 import library ...	..\bin\reimp ..\out\wxc\wxc-0.6.lib	move libwxc-0.6.a ..\out\wxc	move wxc-0.6.def ..\out\wxc	echo Done.r
# End Special Build Toolr
r
!ELSEIF  "$(CFG)" == "wxc - Win32 Debug"r
r
# PROP BASE Use_MFC 0r
# PROP BASE Use_Debug_Libraries 1r
# PROP BASE Output_Dir "Debug"r
# PROP BASE Intermediate_Dir "Debug"r
# PROP BASE Target_Dir ""r
# PROP Use_MFC 0r
# PROP Use_Debug_Libraries 1r
# PROP Output_Dir "..\out\wxc"r
# PROP Intermediate_Dir "..\out\wxc\debug"r
# PROP Ignore_Export_Lib 0r
# PROP Target_Dir ""r
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "WXC_EXPORTS" /YX /FD /GZ /cr
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I "..\..\wxWindows-2.4.2\lib\mswd" /I "include" /I "src\ewxw" /I "..\..\wxWindows-2.4.2\include" /D "_DEBUG" /D "__WXDEBUG__" /D WXDEBUG=1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "WXC_EXPORTS" /D WINVER=0x400 /D "_MT" /D wxUSE_GUI=1 /D BUILD_WXC=1 /D "_DLL" /YX /FD /GZ /cr
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32r
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32r
# ADD BASE RSC /l 0x413 /d "_DEBUG"r
# ADD RSC /l 0x409 /i "..\..\wxWindows-2.4.2\include" /d "_DEBUG"r
BSC32=bscmake.exer
# ADD BASE BSC32 /nologor
# ADD BSC32 /nologor
LINK32=link.exer
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:septr
# ADD LINK32 zlibd.lib regexd.lib pngd.lib jpegd.lib tiffd.lib wxmswd.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib comctl32.lib rpcrt4.lib wsock32.lib opengl32.lib winmm.lib /nologo /dll /debug /machine:I386 /nodefaultlib:"LIBCMTD" /out:"..\out\wxc\wxcd-0.6.dll" /pdbtype:sept /libpath:"..\..\wxWindows-2.4.2\lib"r
# Begin Special Build Toolr
SOURCE="$(InputPath)"r
PostBuild_Cmds=echo Generating mingw32 import library ...	..\bin\reimp ..\out\wxc\wxcd-0.6.lib	move libwxcd-0.6.a ..\out\wxc	move wxcd-0.6.def ..\out\wxc	echo Done.r
# End Special Build Toolr
r
!ENDIF r
r
# Begin Targetr
r
# Name "wxc - Win32 Release"r
# Name "wxc - Win32 Debug"r
# Begin Group "Header Files"r
r
# PROP Default_Filter "h;hpp;hxx;hm;inl"r
# Begin Source Filer
r
SOURCE=.\include\db.hr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\include\ewxw_def.hr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\include\glcanvas.hr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\include\wave.hr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\include\wrapper.hr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\include\wxc.hr
# End Source Filer
# End Groupr
# Begin Group "Resource Files"r
r
# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"r
# Begin Source Filer
r
SOURCE=.\src\wxc.rcr
# End Source Filer
# End Groupr
# Begin Group "Source Files"r
r
# PROP Default_Filter "*.cpp"r
# Begin Source Filer
r
SOURCE=.\src\apppath.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\db.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\eljevent.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\eljmime.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw_main.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\extra.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\glcanvas.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\image.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\treectrl.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\wave.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\wrapper.cppr
# End Source Filer
# End Groupr
# Begin Group "ewxw"r
r
# PROP Default_Filter ""r
# Begin Group "ewxw sources"r
r
# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"r
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljaccelerator.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljartprov.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljbitmap.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljbrush.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljbusyinfo.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljbutton.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljcalendarctrl.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljcaret.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljcheckbox.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljchecklistbox.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljchoice.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljclipboard.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljcoldata.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljcolour.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljcolourdlg.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljcombobox.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljconfigbase.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljcontrol.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljctxhelp.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljcursor.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljdataformat.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljdatetime.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljdc.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljdialog.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljdialup.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljdirdlg.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljdnd.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljdrawing.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljfiledialog.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljfilehist.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljfindrepldlg.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljfont.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljfontdata.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljfontdlg.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljframe.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljgauge.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljgrid.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljhelpcontroller.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljicnbndl.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljicon.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljimage.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljimagelist.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljipc.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljlayoutconstraints.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljlistbox.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljlistctrl.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljlocale.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljlog.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljmask.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljmdi.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljmenu.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljmenubar.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljmessagedialog.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljminiframe.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljnotebook.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljpalette.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljpanel.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljpen.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljprintdlg.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljprinting.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljprocess.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljradiobox.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljradiobutton.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljregion.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljregioniter.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljsash.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljscintilla.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljscrollbar.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljscrolledwindow.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljsingleinst.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljsizer.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljslider.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljspinctrl.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljsplitterwindow.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljstaticbox.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljstaticline.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljstatictext.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljstatusbar.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljsystemsettings.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljtextctrl.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljtglbtn.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljthread.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljtimer.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljtipwnd.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljtoolbar.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljvalidator.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljwindow.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\eljwizard.cppr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\src\ewxw\Scintilla.hr
# End Source Filer
# End Groupr
# Begin Group "ewxw headers"r
r
# PROP Default_Filter "*.h"r
# Begin Source Filer
r
SOURCE=.\include\ewxw\wxc_glue.hr
# End Source Filer
# Begin Source Filer
r
SOURCE=.\include\ewxw\wxc_types.hr
# End Source Filer
# End Groupr
# End Groupr
# End Targetr
# End Projectr
