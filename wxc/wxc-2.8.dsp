# Microsoft Developer Studio Project File - Name="wxc" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=wxc - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "wxc-2.8.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wxc-2.8.mak" CFG="wxc - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wxc - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "wxc - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "wxc - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\dist\wxc"
# PROP Intermediate_Dir "..\dist\wxc\release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "WXC_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "..\..\wxMSW-2.8.7\lib\vc_lib\mswu" /I "..\..\wxMSW-2.8.7\contrib\include" /I "include" /I "src\ewxw" /I "..\..\wxMSW-2.8.7\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "WXC_EXPORTS" /D WINVER=0x400 /D "_MT" /D wxUSE_GUI=1 /D wxUSE_UNICODE=1 /D wxUSE_STC=1 /D BUILD_WXC=1 /D "_DLL" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x413 /d "NDEBUG"
# ADD RSC /l 0x409 /i "..\..\wxMSW-2.8.7\include" /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386

# ADD LINK32 wxzlib.lib wxregexu.lib wxpng.lib wxjpeg.lib wxtiff.lib wxexpat.lib wxbase28u.lib wxbase28u_net.lib wxbase28u_odbc.lib wxbase28u_xml.lib wxmsw28u_core.lib wxmsw28u_adv.lib wxmsw28u_dbgrid.lib wxmsw28u_gl.lib wxmsw28u_html.lib wxmsw28u_media.lib wxmsw28u_stc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib comctl32.lib rpcrt4.lib wsock32.lib opengl32.lib winmm.lib /nologo /dll /machine:I386 /nodefaultlib:"LIBCMT" /out:"..\dist\wxc\wxc-msw2.8.7-0.10.2.dll" /libpath:"..\..\wxMSW-2.8.7\lib\vc_lib"
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Cmds=echo Generating mingw32 import library ...	..\bin\reimp ..\dist\wxc\wxc-msw2.8.7-0.10.2.lib	move libwxc-msw2.8.7-0.10.2.a ..\dist\wxc	move wxc-msw2.8.7-0.10.2.def ..\dist\wxc	echo Done.
# End Special Build Tool

!ELSEIF  "$(CFG)" == "wxc - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\dist\wxc"
# PROP Intermediate_Dir "..\dist\wxc\debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "WXC_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I "..\..\wxMSW-2.8.7\lib\vc_lib\mswud" /I "include" /I "src\ewxw" /I "..\..\wxMSW-2.8.7\include" /I "..\..\wxMSW-2.8.7\contrib\include" /D "_DEBUG" /D "__WXDEBUG__" /D WXDEBUG=1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "WXC_EXPORTS" /D WINVER=0x400 /D "_MT" /D wxUSE_GUI=1 /D wxUSE_UNICODE=1 /D wxUSE_STC=1 /D BUILD_WXC=1 /D "_DLL" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x413 /d "_DEBUG"
# ADD RSC /l 0x409 /i "..\..\wxMSW-2.8.7\include" /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept

# ADD LINK32 wxzlibd.lib wxregexud.lib wxpngd.lib wxjpegd.lib wxtiffd.lib wxexpatd.lib wxbase28ud.lib wxbase28ud_net.lib wxbase28ud_odbc.lib wxbase28ud_xml.lib wxmsw28ud_core.lib wxmsw28ud_adv.lib wxmsw28ud_dbgrid.lib wxmsw28ud_gl.lib wxmsw28ud_html.lib wxmsw28ud_media.lib wxmsw28ud_stc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib comctl32.lib rpcrt4.lib wsock32.lib opengl32.lib winmm.lib /nologo /dll /debug /machine:I386 /nodefaultlib:"LIBCMTD" /out:"..\dist\wxc\wxcd-msw2.8.7-0.10.2.dll" /pdbtype:sept /libpath:"..\..\wxMSW-2.8.7\lib\vc_lib"
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Cmds=echo Generating mingw32 import library ...	..\bin\reimp ..\dist\wxc\wxcd-msw2.8.7-0.10.2.lib	move libwxcd-msw2.8.7-0.10.2.a ..\dist\wxc	move wxcd-msw2.8.7-0.10.2.def ..\dist\wxc	echo Done.
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "wxc - Win32 Release"
# Name "wxc - Win32 Debug"
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\include\db.h
# End Source File
# Begin Source File

SOURCE=.\include\dragimage.h
# End Source File
# Begin Source File

SOURCE=.\include\ewxw_def.h
# End Source File
# Begin Source File

SOURCE=.\include\glcanvas.h
# End Source File
# Begin Source File

SOURCE=.\include\managed.h
# End Source File
# Begin Source File

SOURCE=.\include\mediactrl.h
# End Source File
# Begin Source File

SOURCE=.\include\sound.h
# End Source File
# Begin Source File

SOURCE=.\include\previewframe.h
# End Source File
# Begin Source File

SOURCE=.\include\printout.h
# End Source File
# Begin Source File

SOURCE=.\include\stc.h
# End Source File
# Begin Source File

SOURCE=.\include\stc_gen.h
# End Source File
# Begin Source File

SOURCE=.\include\textstream.h
# End Source File
# Begin Source File

SOURCE=.\include\wrapper.h
# End Source File
# Begin Source File

SOURCE=.\include\wxc.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\src\wxc.rc
# End Source File
# End Group
# Begin Group "Source Files"

# PROP Default_Filter "*.cpp"
# Begin Source File

SOURCE=.\src\apppath.cpp
# End Source File
# Begin Source File

SOURCE=.\src\db.cpp
# End Source File
# Begin Source File

SOURCE=.\src\dragimage.cpp
# End Source File
# Begin Source File

SOURCE=.\src\eljevent.cpp
# End Source File
# Begin Source File

SOURCE=.\src\eljmime.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw_main.cpp
# End Source File
# Begin Source File

SOURCE=.\src\extra.cpp
# End Source File
# Begin Source File

SOURCE=.\src\glcanvas.cpp
# End Source File
# Begin Source File

SOURCE=.\src\image.cpp
# End Source File
# Begin Source File

SOURCE=.\src\managed.cpp
# End Source File
# Begin Source File

SOURCE=.\src\mediactrl.cpp
# End Source File
# Begin Source File

SOURCE=.\src\previewframe.cpp
# End Source File
# Begin Source File

SOURCE=.\src\printout.cpp
# End Source File
# Begin Source File

SOURCE=.\src\stc.cpp
# End Source File
# Begin Source File

SOURCE=.\src\taskbaricon.cpp
# End Source File
# Begin Source File

SOURCE=.\src\textstream.cpp
# End Source File
# Begin Source File

SOURCE=.\src\treectrl.cpp
# End Source File
# Begin Source File

SOURCE=.\src\sound.cpp
# End Source File
# Begin Source File

SOURCE=.\src\wrapper.cpp
# End Source File
# End Group
# Begin Group "ewxw"

# PROP Default_Filter ""
# Begin Group "ewxw sources"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\src\ewxw\eljaccelerator.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljartprov.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljbitmap.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljbrush.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljbusyinfo.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljbutton.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljcalendarctrl.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljcaret.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljcheckbox.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljchecklistbox.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljchoice.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljclipboard.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljcoldata.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljcolour.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljcolourdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljcombobox.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljconfigbase.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljcontrol.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljctxhelp.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljcursor.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljdataformat.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljdatetime.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljdc.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljdialog.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljdialup.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljdirdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljdnd.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljdrawing.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljfiledialog.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljfilehist.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljfindrepldlg.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljfont.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljfontdata.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljfontdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljframe.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljgauge.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljgrid.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljhelpcontroller.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljicnbndl.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljicon.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljimage.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljimagelist.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljipc.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljlayoutconstraints.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljlistbox.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljlistctrl.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljlocale.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljlog.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljmask.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljmdi.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljmenu.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljmenubar.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljmessagedialog.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljminiframe.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljnotebook.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljpalette.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljpanel.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljpen.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljprintdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljprinting.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljprocess.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljradiobox.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljradiobutton.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljregion.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljregioniter.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljsash.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljscintilla.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljscrollbar.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljscrolledwindow.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljsingleinst.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljsizer.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljslider.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljspinctrl.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljsplitterwindow.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljstaticbox.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljstaticline.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljstatictext.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljstatusbar.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljsystemsettings.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljtextctrl.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljtglbtn.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljthread.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljtimer.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljtipwnd.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljtoolbar.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljvalidator.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljwindow.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\eljwizard.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ewxw\Scintilla.h
# End Source File
# End Group
# Begin Group "ewxw headers"

# PROP Default_Filter "*.h"
# Begin Source File

SOURCE=.\include\ewxw\wxc_glue.h
# End Source File
# Begin Source File

SOURCE=.\include\ewxw\wxc_types.h
# End Source File
# End Group
# End Group
# End Target
# End Project
