#-----------------------------------------------------------------------
#  Copyright (c) 2003 2004 Daan Leijen.
#  This file is distributed under the wxWindows library license.
#  See "license.txt" for more details.
#-----------------------------------------------------------------------
ifeq ($(RPM-SOURCE-DIR),)
RPM-SOURCE-DIR=$(HOME)/rpm/SOURCES
endif

# main target
all:	wxcore

#--------------------------------------------------------------------------
# Help
#--------------------------------------------------------------------------
help:
	@echo "usage:"
	@echo " make [macros] [target]"
	@echo ""
	@echo "build:"
	@echo " all             build the library (default target)"
	@echo " doc             generate haddock documentation"
	@echo " install         build and install the library"
	@echo " uninstall       uninstall the library"
	@echo " install-files   install, but do not register with the Haskell compiler"
	@echo ""
	@echo "distribution:"
	@echo " fulldist        make a source, documentation, and binary distribution"
	@echo " srcdist         make a source distribution (as a .zip file)"
	@echo " docdist         make a documentation and samples zip file"
	@echo " bindist         binaries, docs, and samples zip (windows distribution)"
	@echo " macdist         installer for MacOS X (.dmg file) (includes docs and samples)"
	@echo " rpmdist         unix RPM installer (no docs and samples included)"
	@echo ""
	@echo "maintenance:"
	@echo " clean           remove object files and binaries"
	@echo " realclean       remove all generated files (including documentation)"
	@echo " wx-clean        clean the wx library only"
	@echo " wxcore-clean    clean the wxcore library only"
	@echo " wxc-clean       clean the wxc wrapper only"
	@echo ""
	@echo "macros:"
	@echo " DESTDIR=<dir>        prefix the installation directories with <dir>"
	@echo " RPM-SOURCE-DIR=<dir> rpm sources directory [$(RPM-SOURCE-DIR)]"
	@echo ""

depend:
	@echo "You do not need 'make depend' for wxHaskell ;-)"


# system dependent stuff
include config/config.mk

# helper functions, see also for makefile implementation notes.
include makefile.lib

#--------------------------------------------------------------------------
# directories
#--------------------------------------------------------------------------
WX-SOURCES= \
	Graphics/UI/WX/Types \
	Graphics/UI/WX/Attributes \
	Graphics/UI/WX/Layout \
	Graphics/UI/WX/Classes \
	Graphics/UI/WX/Variable \
	Graphics/UI/WX/Events \
	Graphics/UI/WX/Window \
	Graphics/UI/WX/TopLevelWindow \
	Graphics/UI/WX/Frame \
	Graphics/UI/WX/Media \
	Graphics/UI/WX/Menu \
	Graphics/UI/WX/Timer \
	Graphics/UI/WX/Draw \
	Graphics/UI/WX/Controls \
	Graphics/UI/WX/Dialogs \
	Graphics/UI/WX

#--------------------------------------------------------------------------
# wxcore
#--------------------------------------------------------------------------
WXCORE-SOURCES = \
	Graphics/UI/WXCore/WxcClasses \
	Graphics/UI/WXCore/WxcClassInfo \
	Graphics/UI/WXCore/WxcDefs \
	Graphics/UI/WXCore/Types \
	Graphics/UI/WXCore/Defines \
	Graphics/UI/WXCore/Draw \
	Graphics/UI/WXCore/Image \
	Graphics/UI/WXCore/Events \
	Graphics/UI/WXCore/DragAndDrop \
	Graphics/UI/WXCore/Frame \
	Graphics/UI/WXCore/Layout \
	Graphics/UI/WXCore/Process \
	Graphics/UI/WXCore/Print \
	Graphics/UI/WXCore/Dialogs \
	Graphics/UI/WXCore/Controls \
	Graphics/UI/WXCore/Db \
	Graphics/UI/WXCore/OpenGL \
	Graphics/UI/WXCore

WXCORE-CORE-A-SOURCES = \
	Graphics/UI/WXCore/WxcObject \
	Graphics/UI/WXCore/WxcClassTypes \
	Graphics/UI/WXCore/WxcTypes \
	
WXCORE-CORE-B-SOURCES = \
	Graphics/UI/WXCore/WxcClassesAL

WXCORE-CORE-C-SOURCES = \
	Graphics/UI/WXCore/WxcClassesMZ

WXCORE-GEN-SOURCES = \
	Graphics/UI/WXCore/WxcClassTypes \
	Graphics/UI/WXCore/WxcClassesAL \
	Graphics/UI/WXCore/WxcClassesMZ \
	Graphics/UI/WXCore/WxcClasses \
	Graphics/UI/WXCore/WxcClassInfo \
	Graphics/UI/WXCore/WxcDefs 
	
# all sources that generate stub files (ie. containing: foreign import "wrapper")
WXCORE-STUBS = \
	Graphics/UI/WXCore/Events

#--------------------------------------------------------------------------
# wxdirect
#--------------------------------------------------------------------------
WXD-SOURCES = \
	MultiSet \
	HaskellNames Types \
	ParseEiffel ParseC \
	Classes \
	DeriveTypes \
	CompileHeader \
	CompileClassTypes \
	CompileClassInfo \
	CompileClasses \
	CompileDefs \
	CompileSTC \
	Main


#--------------------------------------------------------------------------
# wxc
#--------------------------------------------------------------------------
WXC-CORE= \
	ewxw_main extra wrapper \
	eljevent eljmime \
	treectrl image apppath db dragimage glcanvas graphicscontext sound managed \
	mediactrl printout previewframe taskbaricon textstream stc

WXC-EWXW= \
	accelerator bitmap brush busyinfo button calendarctrl \
	caret checkbox checklistbox choice clipboard coldata \
	colour colourdlg combobox configbase control cursor \
	dataformat datetime dc dialog dirdlg dnd drawing \
	filedialog filehist font fontdata fontdlg frame gauge \
	grid helpcontroller icon image imagelist layoutconstraints \
	listbox listctrl locale log mask mdi menu menubar \
	messagedialog miniframe notebook palette panel pen \
	printdlg printing process radiobox radiobutton region \
	regioniter sash scrollbar scrolledwindow sizer \
	slider spinctrl splitterwindow staticbox staticline \
	statictext statusbar systemsettings textctrl timer \
	toolbar validator window wizard \
	findrepldlg artprov tipwnd icnbndl ctxhelp singleinst

# unused:
# treectrl

WXC-SOURCES = \
	$(WXC-CORE) $(patsubst %,elj%,$(WXC-EWXW))

WXC-SPECS-EIFFEL = \
	wxc/eiffel/wxc_defs.e  wxc/eiffel/wx_defs.e \
	wxc/eiffel/stc.e

WXC-SPECS-HEADER = \
	wxc/include/wxc.h \
	wxc/include/db.h wxc/include/glcanvas.h \
	wxc/include/graphicscontext.h \
	wxc/include/sound.h wxc/include/managed.h \
	wxc/include/printout.h wxc/include/previewframe.h \
	wxc/include/textstream.h \
	wxc/include/wxc_glue.h


# distributed in a source distribution
WXC-SRCS=$(wildcard wxc/src/*.cpp) \
	 $(wildcard wxc/src/*.h)\
	 $(wildcard wxc/include/*.h)\
	 $(wildcard wxc/eiffel/*.e)\
	 wxc/src/wxc.rc \
	 $(wildcard wxc/wxc*.dsp) $(wildcard wxc/wxc*.dsw)

#--------------------------------------------------------------------------
# wxhaskell project itself
#--------------------------------------------------------------------------
WXHASKELL-SOURCES= \
	Setup.lhs wxcore.cabal\
	wx/Setup.lhs wx/wx.cabal wx/license.txt\
       	configure makefile makefile.lib \
	bin/prologue-template.txt license.txt changes.txt \
	bin/wxhaskell-spec-template \
	bin/macosx-app-template bin/reimp.exe \
	bin/macosx-builddmg bin/macosx-package  \
	bin/wxhaskell-register bin/wxhaskell-unregister \
	bin/wxhaskell-register-template.bat bin/wxhaskell-unregister-template.bat bin/setcd

SAMPLE-SOURCES= \
	samples/wx/BouncingBalls.hs \
	samples/wx/ByeDemo.hs \
	samples/wx/Controls.hs \
	samples/wx/CustomControl.hs \
	samples/wx/DbBrowse.hs \
	samples/wx/DbConsole.hs \
	samples/wx/FileBrowse.hs \
	samples/wx/Grid.hs \
	samples/wx/HelloWorld.hs \
	samples/wx/ImageViewer.hs \
	samples/wx/Layout.hs \
	samples/wx/ListCtrl.hs \
	samples/wx/Minimal.hs \
	samples/wx/Process.hs \
	samples/wx/Paint.hs \
	samples/wx/Print.hs \
	samples/wx/TimeFlows.hs \
	samples/wx/TimeFlowsEx.hs \
	\
	samples/wxcore/BouncingBalls.hs \
	samples/wxcore/ByeDemo.hs \
	samples/wxcore/HelloWorld.hs \
	samples/wxcore/Minimal.hs \
	samples/wxcore/Paint.hs \
	samples/wxcore/ImageViewer.hs \
	\
	samples/bitmaps/computer.ico \
	samples/bitmaps/disk.ico \
	samples/bitmaps/eye.ico \
	samples/bitmaps/f_closed.ico \
	samples/bitmaps/f_open.ico \
	samples/bitmaps/file.ico \
	samples/bitmaps/fileopen16.png \
	samples/bitmaps/hsicon.ico \
	samples/bitmaps/wxwin16.png \
	samples/bitmaps/wxwin.ico \
	samples/bitmaps/desert.bmp \
	\
	samples/contrib/Camels.hs \
	samples/contrib/PaintDirect.hs \
	samples/contrib/NotebookRight.hs \
	samples/contrib/GLCanvas.hs \
	samples/contrib/GLMultiCanvas.hs\
	\
	bugs/FontFixed.hs\
	bugs/SubMenu.hs\
	bugs/StartStart.hs\
	bugs/NonModalDialog.hs\
	bugs/TextColor.hs\
	
#--------------------------------------------------------------------------
# The main targets.
#--------------------------------------------------------------------------
.SUFFIXES: .hs .hi .o .p_hi .p_o .c .cpp
.PHONY: all install uninstall install-files uninstall-files
.PHONY: help doc webdoc clean realclean

# global variables
OUTDIR	= dist

# clean
clean:	wxc-clean wxd-clean wxcore-clean wx-clean 

realclean: wxcore-realclean 
	-@$(call full-remove-dir,$(OUTDIR)) 



#--------------------------------------------------------------------------
# Install (unfortunately with extra clauses for the mac)
#--------------------------------------------------------------------------
register:  wxcore-register

install:	wxcore-install-files wxcore-register
	@echo ------------------------------------------
	@echo Done with wxcore...
	@echo
	@echo Now please
	@echo  cd wx
	@echo  runhaskell Setup configure
	@echo  runhaskell Setup build
	@echo  runhaskell Setup install
	@echo ------------------------------------------

ifeq ($(TOOLKIT),mac)
	@$(call install-files,config,$(BINDIR),config/macosx-app)
endif
	
uninstall: wx-unregister wxcore-unregister uninstall-files

uninstall-files: wx-uninstall-files wxcore-uninstall-files wxc-uninstall-files 
ifeq ($(TOOLKIT),mac)
	-@$(call uninstall-files,config,$(BINDIR),config/macosx-app)
endif


#--------------------------------------------------------------------------
# Distribution
#--------------------------------------------------------------------------
.PHONY: dist srcdist bindist docdist dist-dirs macdist
#.PHONY: wxc-dist wxd-dist wxcore-dist wx-dist
#.PHONY: wxc-bindist wxcore-bindist wx-bindist
WXHASKELLVER    =wxhaskell-$(VERSION)
WXCOREVER	=wxcore-$(VERSION)
BIN-VERSION	=$(TOOLKIT)$(WXWIN-VERSION)-$(VERSION)
REL-VERSION	=$(TOOLKIT)$(WXWIN-VERSION)-$(HCBASENAME)$(HCVERSION)-$(VERSION)-$(RELEASE)

DIST-OUTDIR	=$(OUTDIR)
DIST-DOC	=$(DIST-OUTDIR)/wxhaskell-doc-$(VERSION).zip
DIST-SRC	=$(DIST-OUTDIR)/wxhaskell-src-$(VERSION).zip
DIST-HACKAGE	=$(DIST-OUTDIR)/wxcore-$(VERSION).tar.gz
DIST-BIN	=$(DIST-OUTDIR)/wxhaskell-bin-$(REL-VERSION).zip
DISTS		=$(DIST-DOC) $(DIST-SRC) $(DIST-BIN) $(DIST-HACKAGE)

SRCDIST-OUTDIR  =$(DIST-OUTDIR)/srcdist
SRCDIST-SRCDIR  =$(SRCDIST-OUTDIR)/$(WXHASKELLVER)
HACKAGEDIST-OUTDIR  =$(DIST-OUTDIR)/hackagedist
HACKAGEDIST-SRCDIR  =$(HACKAGEDIST-OUTDIR)/$(WXCOREVER)

DOCDIST-OUTDIR  =$(DIST-OUTDIR)/docdist
DOCDIST-SRCDIR  =$(DOCDIST-OUTDIR)/$(WXHASKELLVER)


BINDIST-OUTDIR  =$(DIST-OUTDIR)/bindist
BINDIST-LIBDIR  =$(BINDIST-OUTDIR)/$(WXHASKELLVER)/lib
BINDIST-DLLDIR  =$(BINDIST-OUTDIR)/$(WXHASKELLVER)/lib
BINDIST-BINDIR  =$(BINDIST-OUTDIR)/$(WXHASKELLVER)/bin

# full distribution
fulldist: dist-dirs all srcdist bindist docdist

dist-dirs:
	@$(call ensure-dirs-of-files,$(DISTS))

dist-clean: srcdist-clean bindist-clean
	-@$(call safe-remove-files,$(DISTS))

# source distribution
srcdist: srcdist-clean dist-dirs wxc-dist wxd-dist wxcore-dist wx-dist
	@$(call cp-srcdist, $(WXHASKELL-SOURCES))
	@$(call cp-srcdist, $(SAMPLE-SOURCES))
	@echo zipping: $(DIST-SRC)
	@$(CD) $(SRCDIST-OUTDIR) && $(call zip-add-rec,$(DIST-SRC),$(WXHASKELLVER))
	@$(CD) $(SRCDIST-OUTDIR) && $(call tgz-add-rec,$(basename $(DIST-SRC)).tar.gz,$(WXHASKELLVER))


srcdist-clean:
	-@$(call full-remove-dir,$(SRCDIST-OUTDIR))
	-@$(call safe-remove-file,$(DIST-SRC))

# generic binary distribution as a zip
bindist: bindist-clean dist-dirs wxc-bindist wxcore-bindist wx-bindist docdist
	@$(call cp-bindist,config,$(BINDIST-BINDIR),config/wxcore.pkg config/wx.pkg)
ifeq ($(TOOLKIT),msw)
	@$(call cp-bindist,config,$(BINDIST-BINDIR),config/wxcore-partial.pkg config/wx-partial.pkg)
	@$(call cp-bindist,config,$(BINDIST-BINDIR),config/wxhaskell-register.bat config/wxhaskell-unregister.bat config/setcd)
	@$(call cp-bindist,,$(BINDIST-BINDIR),README.txt)
else
	@$(call cp-bindist,bin,$(BINDIST-BINDIR),bin/wxhaskell-register bin/wxhaskell-unregister)
endif
ifeq ($(TOOLKIT),mac)
	@$(call cp-bindist,config,$(BINDIST-BINDIR),config/macosx-app)
endif
	@$(RM) $(DIST-BIN)
	@$(CP) $(DIST-DOC) $(DIST-BIN)
	@$(CD) $(BINDIST-OUTDIR) && $(call zip-add-rec,$(DIST-BIN),*)

bindist-clean:
	-@$(call full-remove-dir,$(BINDIST-OUTDIR))
	-@$(call safe-remove-file,$(DIST-BIN))


# specific binary distributions

# RPM dist
rpmdist: srcdist
	$(CP) -f $(DIST-SRC) $(RPM-SOURCE-DIR)
	rpmbuild -ba config/wxhaskell.spec

# DEB dist
DEBIAN_DIST=dist/debian
DEBIAN_INSTALL_LOCACTION=$(DEBIAN_DIST)/usr/local
DEB_NAME=$(DIST-OUTDIR)/wxhaskell$(ARCHITECTURE)-bin-$(REL-VERSION).deb
DEB_DOC=$(DEBIAN_DIST)/usr/local/share

# MAC dist
WXHASKELLINS=wxhaskell
WXHASKELLDMG=$(DIST-OUTDIR)/wxhaskell-bin-$(ARCHITECTURE)-$(REL-VERSION).dmg
RESOURCEDIR=$(OUTDIR)/macdist/recources
PACKAGEDIR=$(OUTDIR)/macdist/$(WXHASKELLINS)
INFOFILE=$(PACKAGEDIR).info

macdist: docdist bindist
	@$(call ensure-dir,$(RESOURCEDIR))
	@$(call ensure-dir,$(PACKAGEDIR))
	# copy packages
	@$(call cp-echo,$(BINDIST-BINDIR)/wxcore.pkg,$(RESOURCEDIR)/wxcore.pkg)
	@$(call cp-echo,$(BINDIST-BINDIR)/wx.pkg,$(RESOURCEDIR)/wx.pkg)
	# copy post install scripts
	@$(call cp-echo,config/macosx-postinstall,$(RESOURCEDIR)/$(WXHASKELLINS).post_install)
	@$(call cp-echo,config/macosx-postinstall,$(RESOURCEDIR)/$(WXHASKELLINS).post_upgrade)
	# copy info file for installer
	@$(call cp-echo,config/macosx-install.info,$(INFOFILE))
	# license and readme
	@$(call cp-echo,license.txt,$(RESOURCEDIR)/License.txt)
	@echo "See <http://wxhaskell.sourceforge.net> for more information." > $(RESOURCEDIR)/Readme.txt
	# create package
	chmod u+x bin/macosx-package
	bin/macosx-package $(BINDIST-OUTDIR)/$(WXHASKELLVER) $(INFOFILE) -d $(PACKAGEDIR) -r $(RESOURCEDIR)
	$(CP) -R $(DOCDIST-SRCDIR)/doc $(PACKAGEDIR)
	$(CP) -R $(DOCDIST-SRCDIR)/samples $(PACKAGEDIR)
	$(CP) -R $(DOCDIST-SRCDIR)/bugs $(PACKAGEDIR)
	chmod u+x bin/macosx-builddmg
	bin/macosx-builddmg $(PACKAGEDIR) $(OUTDIR)
	@mv -f $(OUTDIR)/$(WXHASKELLINS).dmg $(WXHASKELLDMG)
	echo "created: $(WXHASKELLDMG)"
#--------------------------------------------------------------------------
# wxdirect: generates haskell marshall modules
#--------------------------------------------------------------------------
WXD		= wxdirect
WXD-SRCDIR	= $(WXD)/src
WXD-OUTDIR	= $(OUTDIR)/$(WXD)
WXD-EXE		= $(WXD-OUTDIR)/$(WXD)$(EXE)
WXD-OBJS	=$(call make-objs, $(WXD-OUTDIR), $(WXD-SOURCES))
WXD-DEPS	=$(call make-deps, $(WXD-OUTDIR), $(WXD-SOURCES))
WXD-HS		=$(call make-hs,   $(WXD-SRCDIR), $(WXD-SOURCES))
WXD-FLAGS	= --wxc $(WXC) -o $(WXCORE-SRCDIR)/$(WXCORE-HPATH)

# Build executable
wxd: wxd-dirs $(WXD-EXE)

wxd-dirs:
	@$(call ensure-dirs-of-files,$(WXD-OBJS))

wxd-clean:
	-@$(call full-remove-dir,$(WXD-OUTDIR))


# source dist
wxd-dist: $(WXD-HS)
	@$(call cp-srcdist, $^)

# build executable
$(WXD-EXE): $(WXD-OBJS)
	$(HC) $(HCFLAGS) $(PKG-CONTAINERS) $(PKG-PARSEC) $(PKG-TIME) -o $@ $^

# create an object file from source files.
$(WXD-OBJS): $(WXD-OUTDIR)/%.o: $(WXD-SRCDIR)/%.hs
	@$(call compile-hs,$@,$<,$(HCFLAGS) $(PKG-PARSEC) $(PKG-TIME),$(WXD-OUTDIR),-i$(WXD-SRCDIR))

# automatically include all dependency information.
-include $(WXD-DEPS)

#--------------------------------------------------------------------------
# WXCORE: the direct haskell wrapper of the wxc api
#--------------------------------------------------------------------------
WXCORE		=wxcore
WXCORE-PKG	=config/$(WXCORE).pkg
WXCORE-SRCDIR	=$(WXCORE)/src
WXCORE-HPATH	=Graphics/UI/WXCore
WXCORE-OUTDIR	=$(OUTDIR)/wxcore
WXCORE-IMPORTSDIR=$(WXCORE-OUTDIR)/imports

WXCORE-HSDIRS	=-i$(WXCORE-SRCDIR) -i$(WXD-SRCDIR)

WXCORE-OBJ	=$(WXCORE-OUTDIR)/$(WXCORE).o
WXCORE-LIB	=$(WXCORE-OUTDIR)/lib$(WXCORE).a
WXCORE-CORE-A-OBJ =$(WXCORE-OUTDIR)/$(WXCORE)0.o
WXCORE-CORE-A-LIB =$(WXCORE-OUTDIR)/lib$(WXCORE)0.a
WXCORE-CORE-B-OBJ =$(WXCORE-OUTDIR)/$(WXCORE)1.o
WXCORE-CORE-B-LIB =$(WXCORE-OUTDIR)/lib$(WXCORE)1.a
WXCORE-CORE-C-OBJ =$(WXCORE-OUTDIR)/$(WXCORE)2.o
WXCORE-CORE-C-LIB =$(WXCORE-OUTDIR)/lib$(WXCORE)2.a
WXCORE-LIBS	=$(WXCORE-CORE-A-LIB) $(WXCORE-CORE-A-OBJ) \
                 $(WXCORE-CORE-B-LIB) $(WXCORE-CORE-B-OBJ) \
                 $(WXCORE-CORE-C-LIB) $(WXCORE-CORE-C-OBJ) \
		 $(WXCORE-LIB) $(WXCORE-OBJ)

WXCORE-OBJS	=$(call make-objs, $(WXCORE-IMPORTSDIR), $(WXCORE-SOURCES))
WXCORE-CORE-A-OBJS=$(call make-objs, $(WXCORE-IMPORTSDIR), $(WXCORE-CORE-A-SOURCES))
WXCORE-CORE-B-OBJS=$(call make-objs, $(WXCORE-IMPORTSDIR), $(WXCORE-CORE-B-SOURCES))
WXCORE-CORE-C-OBJS=$(call make-objs, $(WXCORE-IMPORTSDIR), $(WXCORE-CORE-C-SOURCES))
WXCORE-STUB-OBJS=$(call make-objs, $(WXCORE-IMPORTSDIR), $(patsubst %,%_stub,$(WXCORE-STUBS)))

WXCORE-PROF-OBJ	=$(WXCORE-OUTDIR)/$(WXCORE).p_o
WXCORE-PROF-LIB	=$(WXCORE-OUTDIR)/lib$(WXCORE)_p.a
WXCORE-CORE-A-PROF-OBJ =$(WXCORE-OUTDIR)/$(WXCORE)0.p_o
WXCORE-CORE-A-PROF-LIB =$(WXCORE-OUTDIR)/lib$(WXCORE)0_p.a
WXCORE-CORE-B-PROF-OBJ =$(WXCORE-OUTDIR)/$(WXCORE)1.p_o
WXCORE-CORE-B-PROF-LIB =$(WXCORE-OUTDIR)/lib$(WXCORE)1_p.a
WXCORE-CORE-C-PROF-OBJ =$(WXCORE-OUTDIR)/$(WXCORE)2.p_o
WXCORE-CORE-C-PROF-LIB =$(WXCORE-OUTDIR)/lib$(WXCORE)2_p.a
WXCORE-PROF-LIBS	=$(WXCORE-CORE-A-PROF-LIB) $(WXCORE-CORE-A-PROF-OBJ) \
                 $(WXCORE-CORE-B-PROF-LIB) $(WXCORE-CORE-B-PROF-OBJ) \
                 $(WXCORE-CORE-C-PROF-LIB) $(WXCORE-CORE-C-PROF-OBJ) \
		 $(WXCORE-PROF-LIB) $(WXCORE-PROF-OBJ)
WXCORE-PROF-OBJS	=$(call make-prof-objs, $(WXCORE-IMPORTSDIR), $(WXCORE-SOURCES))
WXCORE-CORE-A-PROF-OBJS=$(call make-prof-objs, $(WXCORE-IMPORTSDIR), $(WXCORE-CORE-A-SOURCES))
WXCORE-CORE-B-PROF-OBJS=$(call make-prof-objs, $(WXCORE-IMPORTSDIR), $(WXCORE-CORE-B-SOURCES))
WXCORE-CORE-C-PROF-OBJS=$(call make-prof-objs, $(WXCORE-IMPORTSDIR), $(WXCORE-CORE-C-SOURCES))
WXCORE-STUB-PROF-OBJS=$(call make-prof-objs, $(WXCORE-IMPORTSDIR), $(patsubst %,%_stub,$(WXCORE-STUBS)))

WXCORE-CORE-SOURCES=$(WXCORE-CORE-A-SOURCES) $(WXCORE-CORE-B-SOURCES) $(WXCORE-CORE-C-SOURCES)
WXCORE-CORE-OBJS   =$(WXCORE-CORE-A-OBJS) $(WXCORE-CORE-B-OBJS) $(WXCORE-CORE-C-OBJS)

WXCORE-DEPS	=$(call make-deps, $(WXCORE-IMPORTSDIR), $(WXCORE-CORE-SOURCES) $(WXCORE-SOURCES))
WXCORE-HIS	=$(call make-his,  $(WXCORE-IMPORTSDIR), $(WXCORE-CORE-SOURCES) $(WXCORE-SOURCES))
WXCORE-PROF-HIS	=$(call make-prof-his,  $(WXCORE-IMPORTSDIR), $(WXCORE-CORE-SOURCES) $(WXCORE-SOURCES))
WXCORE-HS	=$(call make-hs,   $(WXCORE-SRCDIR),     $(WXCORE-CORE-SOURCES) $(WXCORE-SOURCES))
WXCORE-GEN-HS   =$(call make-hs,   $(WXCORE-SRCDIR),     $(WXCORE-GEN-SOURCES))
WXCORE-NONGEN-HS=$(filter-out $(WXCORE-GEN-HS),$(WXCORE-HS))

WXCORE-BINS	=$(WXCORE-HIS) $(WXCORE-LIBS)
WXCORE-PROF-BINS	=$(WXCORE-PROF-HIS) $(WXCORE-PROF-LIBS)
WXCORE-DOCS	=$(filter-out $(WXCORE-SRCDIR)/$(WXCORE-HPATH)/IntMap.hs,$(WXCORE-HS))
WXCORE-HCFLAGS	=$(HCFLAGS) $(PKG-CONTAINERS) $(PKG-STM) -fvia-C -package-name $(WXCORE)-$(VERSION)


# build main library
ifdef ENABLE-PROF
wxcore: wxcore-only wxcore-prof
else
wxcore: wxcore-only
endif

wxcore-only: wxd wxc wxcore-dirs $(WXCORE-LIBS)

wxcore-dirs:
	@$(call ensure-dirs-of-files,$(WXCORE-OBJS))

wxcore-prof: wxd wxc wxcore-prof-dirs $(WXCORE-PROF-LIBS)

wxcore-prof-dirs:
	@$(call ensure-dirs-of-files,$(WXCORE-PROF-OBJS))

wxcore-clean:
	-@$(call full-remove-dir,$(WXCORE-OUTDIR))

wxcore-realclean: wxcore-clean
	-@$(call safe-remove-files,$(WXCORE-GEN-HS))

# source dist
wxcore-dist: $(WXCORE-NONGEN-HS)
	@$(call cp-srcdist, $^)

# bindist
wxcore-bindist: wxcore
	@$(call cp-bindist,$(WXCORE-OUTDIR),$(BINDIST-LIBDIR),$(WXCORE-BINS))
ifdef ENABLE-PROF
	@$(call cp-bindist,$(WXCORE-OUTDIR),$(BINDIST-LIBDIR),$(WXCORE-PROF-BINS))
endif

# install
wxcore-register: 
	$(call install-pkg  ,$(LIBDIR),$(WXCORE-PKG))

wxcore-install-files: wxcore wxc-install-files 
	@$(call install-files,$(WXCORE-OUTDIR),$(LIBDIR),$(WXCORE-BINS))
ifdef ENABLE-PROF
	@$(call install-files,$(WXCORE-OUTDIR),$(LIBDIR),$(WXCORE-PROF-BINS))
endif
	@$(call install-files,$(dir $(WXCORE-PKG)),$(LIBDIR),$(WXCORE-PKG))

wxcore-unregister: 
	-@$(call uninstall-pkg  ,$(WXCORE))

wxcore-uninstall-files:	
	-@$(call uninstall-files,$(WXCORE-OUTDIR),$(LIBDIR),$(WXCORE-BINS))
ifdef ENABLE-PROF
	-@$(call uninstall-files,$(WXCORE-OUTDIR),$(LIBDIR),$(WXCORE-PROF-BINS))
endif
	-@$(call uninstall-files,$(dir $(WXCORE-PKG)),$(LIBDIR),$(WXCORE-PKG))

# build marshall modules
$(WXCORE-SRCDIR)/$(WXCORE-HPATH)/WxcClassTypes.hs: $(WXD-EXE) $(WXC-SPECS-HEADER)
	$(WXD-EXE) -t $(WXD-FLAGS) $(word 1,$(WXC-SPECS-HEADER))

$(WXCORE-SRCDIR)/$(WXCORE-HPATH)/WxcDefs.hs: $(WXD-EXE) $(WXC-SPECS-EIFFEL)
	$(WXD-EXE) -d $(WXD-FLAGS) $(WXC-SPECS-EIFFEL)

$(WXCORE-SRCDIR)/$(WXCORE-HPATH)/WxcClassInfo.hs: $(WXD-EXE) $(WXC-SPECS-HEADER)
	$(WXD-EXE) -i $(WXD-FLAGS) $(word 1,$(WXC-SPECS-HEADER))

$(WXCORE-SRCDIR)/$(WXCORE-HPATH)/WxcClassesAL.hs: $(WXCORE-SRCDIR)/$(WXCORE-HPATH)/WxcClasses.hs
$(WXCORE-SRCDIR)/$(WXCORE-HPATH)/WxcClassesMZ.hs: $(WXCORE-SRCDIR)/$(WXCORE-HPATH)/WxcClasses.hs

$(WXCORE-SRCDIR)/$(WXCORE-HPATH)/WxcClasses.hs: $(WXD-EXE) $(WXC-SPECS-HEADER)
	$(WXD-EXE) -c $(WXD-FLAGS) $(word 1,$(WXC-SPECS-HEADER))

# build ghci object files
$(WXCORE-OBJ): $(WXCORE-OBJS)  $(WXCORE-STUB-OBJS)
	  $(call combine-objs,$@,$^)

$(WXCORE-CORE-A-OBJ): $(WXCORE-CORE-A-OBJS)
	  $(call combine-objs,$@,$^)

$(WXCORE-CORE-B-OBJ): $(WXCORE-CORE-B-OBJS)
	  $(call combine-objs,$@,$^)

$(WXCORE-CORE-C-OBJ): $(WXCORE-CORE-C-OBJS)
	  $(call combine-objs,$@,$^)

ifdef ENABLE-SPLITOBJS
# build a library
$(WXCORE-LIB): $(WXCORE-OBJS)  $(WXCORE-STUB-OBJS)
	  $(call make-archive-of-splitobjs,$@,$(WXCORE-OBJS))
	  $(call make-archive,$@,$(WXCORE-STUB-OBJS))

$(WXCORE-CORE-A-LIB): $(WXCORE-CORE-A-OBJS)
	  $(call make-archive,$@,$(filter %WxcTypes.o,$^))
	  $(call make-archive-of-splitobjs,$@,$(filter-out %WxcTypes.o,$^))

$(WXCORE-CORE-B-LIB): $(WXCORE-CORE-B-OBJS)
	  $(call make-archive-of-splitobjs,$@,$^)

$(WXCORE-CORE-C-LIB): $(WXCORE-CORE-C-OBJS)
	  $(call make-archive-of-splitobjs,$@,$^)
else
$(WXCORE-LIB): $(WXCORE-OBJS)  $(WXCORE-STUB-OBJS)
	  $(call make-archive,$@,$^)

$(WXCORE-CORE-A-LIB): $(WXCORE-CORE-A-OBJS)
	  $(call make-archive,$@,$^)

$(WXCORE-CORE-B-LIB): $(WXCORE-CORE-B-OBJS)
	  $(call make-archive,$@,$^)

$(WXCORE-CORE-C-LIB): $(WXCORE-CORE-C-OBJS)
	  $(call make-archive,$@,$^)
endif

# create an object file from source files.
$(WXCORE-CORE-A-OBJS) $(WXCORE-CORE-B-OBJS) $(WXCORE-CORE-C-OBJS) $(WXCORE-OBJS): $(WXCORE-IMPORTSDIR)/%.o: $(WXCORE-SRCDIR)/%.hs
	@$(call compile-hs,$@,$<,$(WXCORE-HCFLAGS) -Iwxc/include,$(WXCORE-IMPORTSDIR),$(WXCORE-HSDIRS) )

$(WXCORE-STUB-OBJS): $(WXCORE-IMPORTSDIR)/%_stub.o: $(WXCORE-SRCDIR)/%.hs
	$(HC) -c $(basename $@).c

# profiling version of above targets
$(WXCORE-PROF-OBJ): $(WXCORE-PROF-OBJS)  $(WXCORE-STUB-PROF-OBJS)
	  $(call combine-objs,$@,$^)
$(WXCORE-CORE-A-PROF-OBJ): $(WXCORE-CORE-A-PROF-OBJS)
	  $(call combine-objs,$@,$^)
$(WXCORE-CORE-B-PROF-OBJ): $(WXCORE-CORE-B-PROF-OBJS)
	  $(call combine-objs,$@,$^)
$(WXCORE-CORE-C-PROF-OBJ): $(WXCORE-CORE-C-PROF-OBJS)
	  $(call combine-objs,$@,$^)

ifdef ENABLE-SPLITOBJS
$(WXCORE-PROF-LIB): $(WXCORE-PROF-OBJS)  $(WXCORE-STUB-PROF-OBJS)
	  $(call make-archive-of-splitobjs,$@,$(WXCORE-PROF-OBJS))
	  $(call make-archive,$@,$(WXCORE-STUB-PROF-OBJS))
$(WXCORE-CORE-A-PROF-LIB): $(WXCORE-CORE-A-PROF-OBJS)
	  $(call make-archive,$@,$(filter %WxcTypes.p_o,$^))
	  $(call make-archive-of-splitobjs,$@,$(filter-out %WxcTypes.p_o,$^))
$(WXCORE-CORE-B-PROF-LIB): $(WXCORE-CORE-B-PROF-OBJS)
	  $(call make-archive-of-splitobjs,$@,$^)
$(WXCORE-CORE-C-PROF-LIB): $(WXCORE-CORE-C-PROF-OBJS)
	  $(call make-archive-of-splitobjs,$@,$^)
else
$(WXCORE-PROF-LIB): $(WXCORE-PROF-OBJS)  $(WXCORE-STUB-PROF-OBJS)
	  $(call make-archive,$@,$^)
$(WXCORE-CORE-A-PROF-LIB): $(WXCORE-CORE-A-PROF-OBJS)
	  $(call make-archive,$@,$^)
$(WXCORE-CORE-B-PROF-LIB): $(WXCORE-CORE-B-PROF-OBJS)
	  $(call make-archive,$@,$^)
$(WXCORE-CORE-C-PROF-LIB): $(WXCORE-CORE-C-PROF-OBJS)
	  $(call make-archive,$@,$^)
endif

$(WXCORE-CORE-A-PROF-OBJS) $(WXCORE-CORE-B-PROF-OBJS) $(WXCORE-CORE-C-PROF-OBJS) $(WXCORE-PROF-OBJS): $(WXCORE-IMPORTSDIR)/%.p_o: $(WXCORE-SRCDIR)/%.hs
	@$(call compile-prof-hs,$@,$<,$(WXCORE-HCFLAGS) $(HC-PROF-FLAGS) -Iwxc/include,$(WXCORE-IMPORTSDIR),$(WXCORE-HSDIRS) )
$(WXCORE-STUB-PROF-OBJS): $(WXCORE-IMPORTSDIR)/%_stub.p_o: $(WXCORE-SRCDIR)/%.hs
	$(HC) -c $(basename $@).c

# automatically include all dependency information.
-include $(WXCORE-DEPS)


#--------------------------------------------------------------------------
# WX: the medium level abstraction on wxcore
#--------------------------------------------------------------------------
WX		=wx
WX-SRCDIR	=$(WX)/src
WX-PKG		=config/$(WX).pkg
WX-OUTDIR	=$(OUTDIR)/$(WX)
WX-IMPORTSDIR	=$(WX-OUTDIR)/imports
WX-OBJ		=$(WX-OUTDIR)/$(WX).o
WX-LIB		=$(WX-OUTDIR)/lib$(WX).a
WX-LIBS		=$(WX-LIB) $(WX-OBJ)

WX-PROF=BJ		=$(WX-OUTDIR)/$(WX).p_o
WX-PROF-LIB		=$(WX-OUTDIR)/lib$(WX)_p.a
WX-PROF-LIBS		=$(WX-PROF-LIB) $(WX-PROF-OBJ)

WX-OBJS		=$(call make-objs, $(WX-IMPORTSDIR), $(WX-SOURCES))
WX-PROF-OBJS		=$(call make-prof-objs, $(WX-IMPORTSDIR), $(WX-SOURCES))
WX-DEPS		=$(call make-deps, $(WX-IMPORTSDIR), $(WX-SOURCES))
WX-HIS		=$(call make-his,  $(WX-IMPORTSDIR), $(WX-SOURCES))
WX-PROF-HIS		=$(call make-prof-his,  $(WX-IMPORTSDIR), $(WX-SOURCES))
WX-HS		=$(call make-hs,   $(WX-SRCDIR),     $(WX-SOURCES))
WX-DOCS		=$(WX-HS)
WX-BINS		=$(WX-HIS) $(WX-LIBS)
WX-PROF-BINS		=$(WX-PROF-HIS) $(WX-PROF-LIBS)
WX-HCFLAGS	=$(HCFLAGS) $(PKG-STM) -fvia-C -package-name $(WX)-$(VERSION) -package $(WXCORE)-$(VERSION)

WX-HSDIRS	=-i$(WX-SRCDIR)

# build main library
ifdef ENABLE-PROF
wx: wx-only wx-prof
else
wx: wx-only
endif

wx-only: wx-dirs $(WX-LIBS)

wx-install: wx-install-files wx-register

wx-dirs:
	@$(call ensure-dirs-of-files,$(WX-OBJS))

wx-prof: wxd wxc wx-prof-dirs $(WX-PROF-LIBS)

wx-prof-dirs:
	@$(call ensure-dirs-of-files,$(WX-PROF-OBJS))

wx-clean:
	-@$(call full-remove-dir,$(WX-OUTDIR))

# source dist
wx-dist: $(WX-HS)
	@$(call cp-srcdist, $^)

# bindist
wx-bindist: wx
	@$(call cp-bindist,$(WX-OUTDIR),$(BINDIST-LIBDIR),$(WX-BINS))
ifdef ENABLE-PROF
	@$(call cp-bindist,$(WX-OUTDIR),$(BINDIST-LIBDIR),$(WX-PROF-BINS))
endif

# install
wx-register:
	@$(call install-pkg  ,$(LIBDIR),$(WX-PKG))

wx-install-files: wx-only
	@$(call install-files,$(WX-OUTDIR),$(LIBDIR),$(WX-BINS))
ifdef ENABLE-PROF
	@$(call install-files,$(WX-OUTDIR),$(LIBDIR),$(WX-PROF-BINS))
endif
	@$(call install-files,$(dir $(WX-PKG)),$(LIBDIR),$(WX-PKG))

wx-unregister:
	-@$(call uninstall-pkg  ,$(WX))

wx-uninstall-files: 
	-@$(call uninstall-files,$(WX-OUTDIR),$(LIBDIR),$(WX-BINS))
ifdef ENABLE-PROF
	-@$(call uninstall-files,$(WX-OUTDIR),$(LIBDIR),$(WX-PROF-BINS))
endif
	-@$(call uninstall-files,$(dir $(WX-PKG)),$(LIBDIR),$(WX-PKG))

# build ghci object files
$(WX-OBJ): $(WX-OBJS)
	$(call combine-objs,$@,$^)

# build a library
$(WX-LIB): $(WX-OBJS)
ifdef ENABLE-SPLITOBJS
	$(call make-archive-of-splitobjs,$@,$^)
else
	$(call make-archive,$@,$^)
endif

# create an object file from source files.
$(WX-OBJS): $(WX-IMPORTSDIR)/%.o: $(WX-SRCDIR)/%.hs
	@$(call compile-hs,$@,$<,$(WX-HCFLAGS),$(WX-IMPORTSDIR),$(WX-HSDIRS))

# profiling version of above targets
$(WX-PROF-OBJ): $(WX-PROF-OBJS)
	$(call combine-objs,$@,$^)
$(WX-PROF-LIB): $(WX-PROF-OBJS)
ifdef ENABLE-SPLITOBJS
	$(call make-archive-of-splitobjs,$@,$^)
else
	$(call make-archive,$@,$^)
endif
$(WX-PROF-OBJS): $(WX-IMPORTSDIR)/%.p_o: $(WX-SRCDIR)/%.hs
	@$(call compile-prof-hs,$@,$<,$(WX-HCFLAGS) $(HC-PROF-FLAGS),$(WX-IMPORTSDIR),$(WX-HSDIRS))

# automatically include all dependency information.
-include $(WX-DEPS)



#--------------------------------------------------------------------------
# WXC: the C wrapper of the (C++) wxWindows API
#
# $(WXC-LIBNAME) is set in config/config.mk as it can be
# either "wxc" or "wxcd" for a debug version.
#--------------------------------------------------------------------------
WXC		=wxc
WXC-OUTDIR	=$(OUTDIR)/$(WXC)
WXC-SRCDIR	=$(WXC)/src
WXC-INCDIR	=$(WXC)/include

WXC-ARCHIVE	=$(WXC-OUTDIR)/lib$(WXC-LIBNAME)-$(BIN-VERSION).a
WXC-LIB		=$(WXC-OUTDIR)/$(LIB)$(WXC-LIBNAME)-$(BIN-VERSION)$(DLL)

WXC-OBJS	=$(call make-objs, $(WXC-OUTDIR), $(WXC-SOURCES))
WXC-DEPS	=$(call make-deps, $(WXC-OUTDIR), $(WXC-SOURCES))
WXC-LIBS	=$(WXWIN-LIBS)
WXC-CXXFLAGS	=$(WXWIN-CXXFLAGS) $(WXC-CONTRIB) -fPIC -I$(WXC-INCDIR)


wxc: wxc-dirs $(WXC-LIB)

wxc-dirs:
	@$(call ensure-dirs-of-files,$(WXC-OBJS))

wxc-clean:
	-@$(call full-remove-dir,$(WXC-OUTDIR))

wxc-compress: wxc
	@$(call run-compress,$(WXC-LIB))

# source dist
wxc-dist: $(WXC-SRCS)
	@$(call cp-srcdist, $(WXC-SRCS))

# binary distribution. A complication is that sometimes wxWindows is in a separate dll
# and sometimes it is statically linked into wxc.dll (as with microsoft visual c++).
wxc-bindist: wxc-compress
	@$(call cp-bindist,$(dir $(WXC-LIB)),$(BINDIST-DLLDIR),$(WXC-LIB))
ifeq ($(DLL),.dll)
	@$(call cp-bindist,$(dir $(WXC-ARCHIVE)),$(BINDIST-LIBDIR),$(WXC-ARCHIVE))
endif
ifneq ($(WXWIN-REZFILE),)
	@$(call cp-bindist,$(dir $(WXWIN-REZFILE)),$(BINDIST-DLLDIR),$(basename $(WXWIN-REZFILE)).rsrc)
	@$(call cp-bindist,$(dir $(WXWIN-REZFILE)),$(BINDIST-DLLDIR),$(basename $(WXWIN-REZFILE)).r)
endif


# install
wxc-install-files: wxc-compress
	@$(call install-files,$(WXC-OUTDIR),$(LIBDIR),$(WXC-LIB))
ifeq ($(DLL),.dll)
	@$(call install-files,$(WXC-OUTDIR),$(LIBDIR),$(WXC-ARCHIVE))
endif
ifneq ($(WXWIN-REZFILE),)
	@$(call install-files,$(dir $(WXWIN-REZFILE)),$(LIBDIR),$(basename $(WXWIN-REZFILE)).rsrc)
	@$(call install-files,$(dir $(WXWIN-REZFILE)),$(LIBDIR),$(basename $(WXWIN-REZFILE)).r)
endif

wxc-uninstall-files: 
	-@$(call uninstall-files,$(WXC-OUTDIR),$(LIBDIR),$(WXC-LIB) $(WXC-ARCHIVE))
ifneq ($(WXWIN-REZFILE),)
	-@$(call uninstall-files,$(dir $(WXWIN-REZFILE)),$(LIBDIR),$(basename $(WXWIN-REZFILE)).rsrc)
	-@$(call uninstall-files,$(dir $(WXWIN-REZFILE)),$(LIBDIR),$(basename $(WXWIN-REZFILE)).r)
endif

# dynamic link library on mingw32/cygwin: generates wxc.dll and a libwxc.a import library
ifeq ($(WITHMSC),yes)
 # it must be generated already
else
$(basename $(WXC-LIB)).dll: $(WXC-OBJS)
	$(CXX) -shared -o $@ $^ $(WXC-LIBS) -Wl,--output-def,$(WXC-OUTDIR)/$(WXC).def,--out-implib,$(WXC-ARCHIVE)
endif

# dynamic link library on unix: generates single .so file
$(basename $(WXC-LIB)).so: $(WXC-OBJS)
	$(CXX) -shared -o $@ $^ $(WXC-LIBS) -Wl --soname=$(SHARED-PREFIX)$(notdir $@)

# dynamic link library on macOSX: generates single .so file
$(basename $(WXC-LIB)).dylib: $(WXC-OBJS)
	$(CXX) -dynamiclib -install_name $(SHARED-PREFIX)$(notdir $@) -undefined suppress -flat_namespace -o $@ $^ $(filter-out %.a,$(WXC-LIBS))
	
# create an object file from source files
$(WXC-OBJS): $(WXC-OUTDIR)/%.o: $(WXC-SRCDIR)/%.cpp
	@$(call compile-c,$@,$<,$(WXC-CXXFLAGS))

# automatically include dependencies
-include $(WXC-DEPS)


# Currently, wxstc always generates same files. So we comment out this part.
#--------------------------------------------------------------------------
# wxSTC: the C wrapper of wxSTC
#--------------------------------------------------------------------------
#
# WXSTC-WRAPPER = \
#	$(WXC-SRCDIR)/stc_gen.cpp \
#	$(WXC-INCDIR)/stc_gen.h
#
# wxstc: $(WXSTC-WRAPPER)
#
# $(WXSTC-WRAPPER): wxd
#	$(WXD-EXE) -s wxSTC-D3/stc.h --wxc $(WXC) -o $(WXC)
#	$(WXD-EXE) -s ../wxWidgets-$(WXWIN-VERSION)/contrib/include/wx/stc/stc.h --wxc $(WXC) -o $(WXC)

#--------------------------------------------------------------------------
# Documentation
#--------------------------------------------------------------------------
DOC-OUTDIR  =$(OUTDIR)/doc/html
WXCORE-DOC-OUTDIR  =$(DOC-OUTDIR)/wxcore
WX-DOC-OUTDIR  	   =$(DOC-OUTDIR)/wx
WXBOTH-DOC-OUTDIR  =$(DOC-OUTDIR)
WXCORE-DOCFILE     =$(WXCORE-DOC-OUTDIR)/wxcore.haddock
WX-DOCFILE     	   =$(WX-DOC-OUTDIR)/wx.haddock
WXBOTH-DOCFILE     =$(WXBOTH-DOC-OUTDIR)/wxhaskell.haddock
WXCORE-HDOCFLAGS   = --prologue=config/prologue.txt --html $(HDOCBASES)
WX-HDOCFLAGS   = $(WXCORE-HDOCFLAGS) -i$(WXCORE-DOC-OUTDIR)/wxcore.haddock
WXCORE-DOCSOURCES  = $(WXCORE-DOCS)
WX-DOCSOURCES      = $(WX-DOCS)


doc: doc-dirs $(WXCORE-DOCFILE) $(WX-DOCFILE)

doc-dirs:
	@$(call ensure-dir,$(WXCORE-DOC-OUTDIR))
	@$(call ensure-dir,$(WX-DOC-OUTDIR))
	@$(call ensure-dir,$(WXBOTH-DOC-OUTDIR))

doc-clean:
	-@$(call full-remove-dir,$(WXCORE-DOC-OUTDIR))
	-@$(call full-remove-dir,$(WX-DOC-OUTDIR))
	-@$(call full-remove-dir,$(WXBOTH-DOC-OUTDIR))

combined-doc: doc-dirs $(WXBOTH-DOCFILE)

# copy documentation to the wxhaskell website
webdoc: doc
	cd $(DOC-OUTDIR); scp *.* $(USERNAME)@shell.sourceforge.net:/home/groups/w/wx/wxhaskell/htdocs/doc

# documentation distribution
ifeq ($(HDOCFOUND),yes)
docdist: docdist-clean combined-doc
	@echo "-- adding documentation"
	@echo $(wildcard $(DOC-OUTDIR)/*)
	@$(call cp-docdist,$(OUTDIR),$(wildcard $(DOC-OUTDIR)/*))
else
docdist:
	@echo "-- haddock not available: documentation can not be added"
endif
	@echo "-- adding samples"
	@$(call cp-docdist,.,$(SAMPLE-SOURCES))
	@$(CD) $(DOCDIST-OUTDIR) && $(call zip-add-rec,$(DIST-DOC),*)
	
docdist-clean:
	-@$(call full-remove-dir,$(DOCDIST-OUTDIR))
	-@$(call safe-remove-file,$(DIST-DOC))
	

# generate documentation with haddock
$(WXCORE-DOCFILE): config/prologue.txt $(WXCORE-DOCSOURCES)
	$(HDOC) --odir $(WXCORE-DOC-OUTDIR) --dump-interface=$(WXCORE-DOCFILE)  $(WXCORE-HDOCFLAGS) $(WXCORE-DOCSOURCES)

$(WX-DOCFILE): config/prologue.txt $(WX-DOCSOURCES)
	$(HDOC) --odir $(WX-DOC-OUTDIR) --dump-interface=$(WX-DOCFILE) $(WX-HDOCFLAGS) $(WX-DOCSOURCES)

$(WXBOTH-DOCFILE): config/prologue.txt $(WXCORE-DOCSOURCES) $(WX-DOCSOURCES)
	$(HDOC) --odir $(WXBOTH-DOC-OUTDIR) --dump-interface=$(WXBOTH-DOCFILE) $(WXCORE-HDOCFLAGS) $(WXCORE-DOCSOURCES) $(WX-DOCSOURCES)

#--------------------------------------------------------------------------
# Cabal / Hackage stuff
#--------------------------------------------------------------------------

# for hackage
dist: hackagedist

hackagedist: hackagedist-clean dist-dirs
	@$(call cp-hackagedist, $(WXC))
	@$(call cp-hackagedist, $(WXD))
	@$(call cp-hackagedist, $(WXCORE))
	@$(call cp-hackagedist, $(WXHASKELL-SOURCES))
	@$(call cp-hackagedist, $(SAMPLE-SOURCES))
	@echo tarring: $(DIST-HACKAGE)
	@$(CD) $(HACKAGEDIST-OUTDIR) && $(call tgz-add-rec,$(DIST-HACKAGE),$(WXCOREVER))

hackagedist-clean:
	-@$(call full-remove-dir,$(HACKAGEDIST-OUTDIR))
	-@$(call safe-remove-file,$(DIST-HACKAGE))

copy:
	$(MAKE) install prefix=$(destdir)/$(prefix) \
                        bindir=$(destdir)/$(bindir) \
                        libdir=$(destdir)/$(libdir) \
                        datadir=$(destdir)/$(datadir) \
                        libexecdir=$(destdir)/$(libexecdir)

# this is the same as the doc target below (no 's')
docs: wxd-dirs doc-dirs $(WXCORE-DOCFILE)


