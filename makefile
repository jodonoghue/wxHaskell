#-----------------------------------------------------------------------
#  Copyright (c) 2003, Daan Leijen.
#  This file is distributed under the wxWindows library license.
#  See "license.txt" for more details.
#-----------------------------------------------------------------------

# $Id: makefile,v 1.51 2003/10/27 23:31:20 dleijen Exp $

#--------------------------------------------------------------------------
# make [all]	 - build the libraries (in "lib").
# make install	 - install the libraries as packages.
# make uninstall - uninstall the libraries
# make doc	 - generate documentation (in "doc")
# make clean	 - remove generated object files and binaries.
#       wx-clean
#       wxcore-clean
#       wxc-clean
#	doc-clean
#	dist-clean
# make realclean - remove all generated files (including documentation)
# make dist	 - create distribution files
#       srcdist
#       docdist
#       bindist
#
# Dependencies are handled fully automatic: no need for "make depend" :-)
#
# Makefile implementation notes:
#
# The dependency (.d) files are generated together with object files using
# the compiler -M switch. Such dependency file is later processed
# by sed to prepend the proper directory to the target and to move it
# into the proper (imports) directory. The way dependency files are handled
# was 'discovered' by Tom Tromey, and described by Paul Smith,
# see "advanced auto-dependency generation" at:
# "http://make.paulandlesley.org/autodep.html"
#
# We use a single makefile in order to correctly resolve dependencies
# between the different projects -- a recursive make fails to do that,
# see "recursive make considered harmfull" at:
# "http://www.tip.net.au/~millerp/rmch/recu-make-cons-harm.html"
# (We might use include files in the future to split this file in smaller chunks.)
#
# We don't use implicit rules (i.e. "%.o: %.c") as the VPATH mechanism can't
# deal with modules with the same name in different directories :-(
#
# We edit (sed) the haskell dependency files to change dependencies on .hi
# files to .o files. We just disregard .hi files and assume they are always
# generated together with the .o file. This allows us to leave out the implicit
# rule for interface files ("%.hi: %.o").
#--------------------------------------------------------------------------

# system dependent stuff
include config/config.mk

#--------------------------------------------------------------------------
# directories
#--------------------------------------------------------------------------
WX-SOURCES= \
	Graphics/UI/WX/Types \
	Graphics/UI/WX/Attributes \
	Graphics/UI/WX/Layout \
	Graphics/UI/WX/Classes \
	Graphics/UI/WX/Events \
	Graphics/UI/WX/Window \
	Graphics/UI/WX/Frame \
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
	Graphics/UI/WXCore/WxcDefs \
	Graphics/UI/WXCore/WxcClassTypes \
	Graphics/UI/WXCore/IntMap \
	Graphics/UI/WXCore/Types \
	Graphics/UI/WXCore/Defines \
	Graphics/UI/WXCore/Draw \
	Graphics/UI/WXCore/Events \
	Graphics/UI/WXCore/Frame \
	Graphics/UI/WXCore/Layout \
	Graphics/UI/WXCore/Process \
	Graphics/UI/WXCore/Dialogs \
	Graphics/UI/WXCore/Image \
	Graphics/UI/WXCore/Controls \
	Graphics/UI/WXCore/Db \
	Graphics/UI/WXCore/OpenGL \
	Graphics/UI/WXCore

WXCORE-CORE-SOURCES = \
	Graphics/UI/WXCore/WxcTypes \
	Graphics/UI/WXCore/WxcClasses


WXCORE-GEN-SOURCES = \
	Graphics/UI/WXCore/WxcClasses \
	Graphics/UI/WXCore/WxcClassTypes \
	Graphics/UI/WXCore/WxcDefs 
	
# all sources that generate stub files (ie. containing: foreign import "wrapper")
WXCORE-STUBS = \
	Graphics/UI/WXCore/Events

#--------------------------------------------------------------------------
# wxdirect
#--------------------------------------------------------------------------
WXD-SOURCES = \
	Map Set MultiSet \
	HaskellNames Types \
	ParseEiffel ParseC \
	Classes \
	DeriveTypes \
	CompileHeader \
	CompileClassTypes \
	CompileClasses \
	CompileDefs \
	Main


#--------------------------------------------------------------------------
# wxc
#--------------------------------------------------------------------------
WXC-CORE= \
	ewxw_main extra wrapper \
	eljevent eljmime \
	treectrl image apppath db glcanvas wave

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
	$(WXC-CORE) $(patsubst %,ewxw/elj%,$(WXC-EWXW))

WXC-SPECS-EIFFEL = \
	wxc/eiffel/wxc_defs.e  wxc/eiffel/ewxw/wx_defs.e

WXC-SPECS-HEADER = \
	wxc/include/wxc.h \
	wxc/include/db.h wxc/include/glcanvas.h wxc/include/wave.h \
	wxc/include/ewxw/wxc_glue.h 


# distributed in a source distribution
WXC-SRCS=$(wildcard wxc/src/*.cpp)   $(wildcard wxc/src/ewxw/*.cpp) $(wildcard wxc/src/ewxw/*.h)\
	 $(wildcard wxc/include/*.h) $(wildcard wxc/include/ewxw/*.h) \
	 $(wildcard wxc/eiffel/*.e)  $(wildcard wxc/eiffel/ewxw/*.e) \
	 wxc/src/wxc.rc wxc/wxc.dsp wxc/wxc.dsw

#--------------------------------------------------------------------------
# wxhaskell project itself
#--------------------------------------------------------------------------
WXHASKELL-SOURCES= \
	config.search configure makefile \
	prologue.txt license.txt \
	bin/macosx-app-template bin/reimp.exe \
	bin/macosx-builddmg bin/macosx-package  \
	bin/wxhaskell-register bin/wxhaskell-unregister \
	bin/wxhaskell-register-template.bat bin/wxhaskell-unregister-template.bat

SAMPLE-SOURCES= \
	samples/wx/BouncingBalls.hs \
	samples/wx/ByeDemo.hs \
	samples/wx/Controls.hs \
	samples/wx/HelloWorld.hs \
	samples/wx/Layout.hs \
	samples/wx/Minimal.hs \
	samples/wx/Process.hs \
	samples/wx/Paint.hs \
	samples/wx/ImageViewer.hs \
	samples/wx/TimeFlows.hs \
	samples/wx/TimeFlowsEx.hs \
	samples/wx/FileBrowse.hs \
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
	
	
#--------------------------------------------------------------------------
# Functions  ($(1) means first argument etc.)
#--------------------------------------------------------------------------
# create derived file from base name
# usage: $(call make-hs,<source root path>,<file base names>)
# usage: $(call make-objs,<object root path>,<file base names>)
make-hs		=$(patsubst %,$(1)/%.hs,$(2))
make-objs	=$(patsubst %,$(1)/%.o,$(2))
make-deps	=$(patsubst %,$(1)/%.d,$(2))
make-his	=$(patsubst %,$(1)/%.hi,$(2))

# usage: $(call run-silent,<command>)
run-silent	=$(1) 1> /dev/null 2> /dev/null
run-with-echo   =echo "$(1)" && $(1)

# usage: $(call relative-to,<root-dir>,<relative files>)
relative-to	=$(patsubst $(1)/%,%,$(2))

# usage: $(call relative-fromto,<old-root-dir>,<new-root-dir>,<files>)
relative-fromto	=$(patsubst $(1)%,$(2)%,$(3))

# get directories of files (using 'sort' to get rid of duplicates)
dirs-of-files   =$(sort $(foreach file,$(1),$(dir $(file))))

# usage: $(call ensure-dir,<directory>)
# usage: $(call ensure-dirs-of-files,<files>)
ensure-dir	=if test -d "$(1)" -o "$(1)" = "./"; then :; else $(MKDIR) $(1); fi
ensure-dirs	=$(foreach dir,$(1),$(call ensure-dir,$(dir)) &&) :
ensure-dirs-of-files=$(call ensure-dirs,$(call dirs-of-files,$(1)))

# full-remove-dir
# safe-remove-dir
safe-remove-dir	=if test -d $(1); then $(call run-with-echo,$(RMDIR) $(1)); fi
safe-remove-dir-contents = if test -d $(1); then $(call run-with-echo,$(RM) -r $(1)/*); fi
full-remove-dir	=$(call safe-remove-dir-contents,$(1)); $(call safe-remove-dir,$(1))

# safe-move-file(<source>,<destination (directory)>)
# safe-remove-file(<file>)
safe-move-file	=if test -f $(1); then $(call run-with-echo,$(MV) $(1) $(2)); fi
safe-remove-file=if test -f $(1); then $(call run-with-echo,$(RM) $(1)); fi
safe-remove-files=$(foreach file,$(1),$(call safe-remove-file,$(file)) &&) :

# silent-move-file
silent-move-file=if test -f $(1); then $(MV) $(1) $(2); fi
silent-remove-file=if test -f $(1); then $(RM) $(1); fi

# make-c-obj(<output .o>,<input .c>,<compile flags>)
make-c-obj	=$(call run-with-echo,$(CXX) -c $(2) -o $(1) $(3))

# compile-c(<output .o>,<input .c>,<compile flags>)
compile-c	=$(call make-c-obj,$(1),$(2),-MD $(3)) && \
		 $(call silent-move-file,$(notdir $(basename $(1))).d,$(dir $(1)))

# silent-move-stubs(<output .o>,<input .c>)
silent-move-stubs =$(call silent-move-file,$(basename $(2))_stub.h,$(dir $(1))) && \
		   $(call silent-move-file,$(basename $(2))_stub.c,$(dir $(1)))	

# make-hs-obj(<output .o>,<input .hs>,<compile flags>)
make-hs-obj     =$(call run-with-echo,$(HC) -c $(2) -o $(1) -ohi $(basename $(1)).hi -odir $(dir $(1)) $(3))

# make-hs-deps(<output .o>,<input .hs>,<compile flags>)
make-hs-deps	=$(HC) $(2) $(3) -M -optdep-f -optdep$(basename $(1)).d.in && \
		 sed -e 's|$(basename $(2))|$(basename $(1))|' -e 's|\.hi|\.o|g' $(basename $(1)).d.in > $(basename $(1)).d && \
		 $(call silent-remove-file,$(basename $(1)).d.in)

# compile-hs(<output .o>,<input .hs>,<compile flags>)
compile-hs      =$(call make-hs-obj,$(1),$(2),$(3)) && \
		 $(call silent-move-stubs,$(1),$(2)) && \
		 $(call make-hs-deps,$(1),$(2),$(3))


# make single-object file
# combine-objs(<output .o>,<input .o files>)
ifeq ($(TOOLKIT),mac)
combine-objs	=$(LD) -x -r -o $(1) $(2)
else
combine-objs	=$(LD) -r -o $(1) $(2)
endif

# create an archive
# make-archive(<archive>,<input .o files>)
make-archive	=$(AR) -sr $(1) $(2)

# update the archive symbol index
# make-archive-index(<archive>)
make-archive-index=$(AR) -s $(1)


# install files, keeping directory structure intact (that is why we use 'foreach').
# we circumvent a 'ld' bug on the mac by also re-indexing archives on installation
# usage: $(call install-files,<local dir>,<install dir>,<files>)
# usage: $(call uninstall-files,<local dir>,<install dir>,<files>)
install-file    =echo "install: $(2)" && $(INSTALL) $(1) $(dir $(2)) \
	         $(if $(filter %.a,$(2)),&& $(call make-archive-index,$(basename $(2)).a))
install-dir     =echo "install directory: $(1)" && $(INSTALLDIR) $(1)
install-files   =$(foreach dir,$(call dirs-of-files,$(call relative-fromto,$(1),$(2),$(3))),$(call install-dir,$(dir)) &&) \
	         $(foreach file,$(3),$(call install-file,$(file),$(call relative-fromto,$(1),$(2),$(file))) &&) \
		 :

uninstall-file  =if test -f "$(1)"; then echo "uninstall: $(1)" && $(RM) $(1); fi
uninstall-dir   =if test -d "$(2)" -a "$(2)" != "./"; then echo "uninstall directory: $(1)/$(2)" && $(call run-silent,$(RMDIR) -p $(2)); fi

# extremely baroque way of reversing a list of (at most 10) items
reverse10	=$(patsubst 9%,%,$(patsubst 8%,%,$(patsubst 7%,%,\
		$(patsubst 6%,%,$(patsubst 5%,%,$(patsubst 4%,%,\
		$(patsubst 3%,%,$(patsubst 2%,%,$(patsubst 1%,%,\
		$(patsubst 0%,%,\
		$(sort $(join $(wordlist 1,$(words $(1)),9 8 7 6 5 4 3 2 1 0),$(1)))\
		))))))))))

uninstall-filesx=$(foreach file,$(2),$(call uninstall-file,$(file)) &&) \
		 $(CD) $(1) && \
		 $(foreach dir,$(call reverse10,$(call dirs-of-files,$(call relative-to,$(1),$(2)))),$(call uninstall-dir,$(1),$(dir)) &&) \
		 :

uninstall-files =$(call uninstall-filesx,$(2),$(call relative-fromto,$(1),$(2),$(3)))

# install packages
# usage: $(call install-pkg,<install dir>,<package file>)
# usage: $(call uninstall-pkg,<package name>)
install-pkg=env wxhlibdir=$(1) $(HCPKG) -u -i $(2)
uninstall-pkg=if $(call run-silent,$(HCPKG) -s $(1)); then echo "unregister package: $(1)" && $(HCPKG) -r $(1); fi

# copy files.
# usage: cp-bindist<dirprefix,target-dir,source files>
# use -R switch to copy symbolic links literally instead of following the links.
# use -p to preserve file dates to avoid linker bug on macosX with .a files.
cp-echo		=echo  "copy $(1) to $(2)" && $(CP) -p -R $(1) $(2)
cp-fromto	=$(call ensure-dirs-of-files,$(call relative-fromto,$(1),$(2),$(3))) && \
		 $(foreach file,$(3),$(call cp-echo,$(file),$(dir $(call relative-fromto,$(1),$(2),$(file)))) && ) :
cp-bindist	=$(call cp-fromto,$(patsubst %/,%,$(1)),$(patsubst %/,%,$(2)),$(3))

# usage: $(call cp-relative,<out topdir>,<local files>)
cp-relative	=$(call ensure-dirs-of-files,$(patsubst %,$(1)/%,$(2))) && \
		 $(foreach file,$(2),$(call cp-echo,$(file),$(1)/$(patsubst %/,%,$(dir $(file)))) && ):

cp-srcdist	=$(call cp-relative,$(TOPDIR)/$(SRCDIST-SRCDIR),$(1))
cp-docdist	=$(CD) $(1) && $(call cp-relative,$(TOPDIR)/$(DOCDIST-SRCDIR),$(patsubst $(1)/%,%,$(2)))

# zip commands
zip-add		=echo zipping: $(1); $(ZIP) -y -9 $(TOPDIR)/$(1) $(2)
zip-add-rec     =echo zipping: $(1); $(ZIP) -r -y -9 $(TOPDIR)/$(1) $(2)

#--------------------------------------------------------------------------
# The main targets.
#--------------------------------------------------------------------------
.SUFFIXES: .hs .hi .o .c .cpp
.PHONY: all install uninstall doc webdoc clean realclean
#.PHONY: wxc wxd wxcore wx
#.PHONY: wxc-install wxcore-install wx-install
#.PHONY: wxc-uninstall wxcore-uninstall wx-uninstall
#.PHONY: wxc-dirs wxd-dirs wxcore-dirs wx-dirs 

# global variables
OUTDIR	= out

# main targets
all:		wx
clean:		wxc-clean wxd-clean wxcore-clean wx-clean 

realclean: wxcore-realclean 
	-@$(call full-remove-dir,$(OUTDIR)) 

#--------------------------------------------------------------------------
# Install (unfortunately with extra clauses for the mac)
#--------------------------------------------------------------------------
install:	wx-install
ifeq ($(TOOLKIT),mac)
	@$(call install-files,config,$(BINDIR),config/macosx-app)
endif
	
uninstall:	wx-uninstall wxcore-uninstall wxc-uninstall
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
BIN-VERSION	=$(HCNAME)$(HCVERSION)-$(VERSION)

DIST-OUTDIR	=$(OUTDIR)
DIST-DOC	=$(DIST-OUTDIR)/wxhaskell-doc-$(VERSION).zip
DIST-SRC	=$(DIST-OUTDIR)/wxhaskell-src-$(VERSION).zip
DIST-BIN	=$(DIST-OUTDIR)/wxhaskell-bin-$(TOOLKIT)-$(BIN-VERSION).zip
DISTS		=$(DIST-DOC) $(DIST-SRC) $(DIST-BIN)

SRCDIST-OUTDIR  =$(DIST-OUTDIR)/srcdist
SRCDIST-SRCDIR  =$(SRCDIST-OUTDIR)/$(WXHASKELLVER)

DOCDIST-OUTDIR  =$(DIST-OUTDIR)/docdist
DOCDIST-SRCDIR  =$(DOCDIST-OUTDIR)/$(WXHASKELLVER)


BINDIST-OUTDIR  =$(DIST-OUTDIR)/bindist
BINDIST-LIBDIR  =$(BINDIST-OUTDIR)/$(WXHASKELLVER)/lib
BINDIST-DLLDIR  =$(BINDIST-OUTDIR)/$(WXHASKELLVER)/lib
BINDIST-BINDIR  =$(BINDIST-OUTDIR)/$(WXHASKELLVER)/bin

# full distribution
dist: dist-dirs all srcdist bindist docdist

dist-dirs:
	@$(call ensure-dirs-of-files,$(DISTS))

dist-clean: srcdist-clean bindist-clean
	-@$(call safe-remove-files,$(DISTS))

# source distribution
srcdist: srcdist-clean dist-dirs wxc-dist wxd-dist wxcore-dist wx-dist
	@$(call cp-srcdist, $(WXHASKELL-SOURCES))
	@$(call cp-srcdist, $(SAMPLE-SOURCES))
	@echo zipping: $(DIST-SRC)
	@$(CD) $(SRCDIST-OUTDIR) && $(call zip-add-rec,$(DIST-SRC),*)

srcdist-clean:
	-@$(call full-remove-dir,$(SRCDIST-OUTDIR))
	-@$(call safe-remove-file,$(DIST-SRC))

# generic binary distribution as a zip
bindist: all bindist-clean dist-dirs wxc-bindist wxcore-bindist wx-bindist
	@$(call cp-bindist,config,$(BINDIST-BINDIR),config/wxcore.pkg config/wx.pkg)
ifeq ($(TOOLKIT),msw)
	@$(call cp-bindist,config,$(BINDIST-BINDIR),config/wxhaskell-register.bat config/wxhaskell-unregister.bat)
else
	@$(call cp-bindist,bin,$(BINDIST-BINDIR),bin/wxhaskell-register bin/wxhaskell-unregister)
endif
ifeq ($(TOOLKIT),mac)
	@$(call cp-bindist,config,$(BINDIST-BINDIR),config/macosx-app)
endif
	@$(RM) $(DIST-BIN)
	@$(CD) $(BINDIST-OUTDIR) && $(call zip-add-rec,$(DIST-BIN),*)

bindist-clean:
	-@$(call full-remove-dir,$(BINDIST-OUTDIR))
	-@$(call safe-remove-file,$(DIST-BIN))


# specific binary distributions
WXHASKELLDMG=$(DIST-OUTDIR)/wxhaskell-bin-$(TOOLKIT)-$(HC)$(HCVERSION)-$(VERSION).dmg
RESOURCEDIR=$(OUTDIR)/macdist/recources
PACKAGEDIR=$(OUTDIR)/macdist/$(WXHASKELLVER)
INFOFILE=$(PACKAGEDIR).info

macdist: bindist
	@$(call ensure-dir,$(RESOURCEDIR))
	@$(call ensure-dir,$(PACKAGEDIR))
	# copy packages
	@$(call cp-echo,$(BINDIST-BINDIR)/wxcore.pkg,$(RESOURCEDIR)/wxcore.pkg)
	@$(call cp-echo,$(BINDIST-BINDIR)/wx.pkg,$(RESOURCEDIR)/wx.pkg)
	# copy post install scripts
	@$(call cp-echo,config/macosx-postinstall,$(RESOURCEDIR)/$(WXHASKELLVER).post_install)
	@$(call cp-echo,config/macosx-postinstall,$(RESOURCEDIR)/$(WXHASKELLVER).post_upgrade)
	# copy info file for installer
	@$(call cp-echo,config/macosx-install.info,$(INFOFILE))
	# license and readme
	@$(call cp-echo,license.txt,$(RESOURCEDIR)/License.txt)
	@echo "See <http://wxhaskell.sourceforge.net> for more information." > $(RESOURCEDIR)/Readme.txt
	# create package
	bin/macosx-package $(BINDIST-OUTDIR) $(INFOFILE) -d $(PACKAGEDIR) -r $(RESOURCEDIR)
	bin/macosx-builddmg $(PACKAGEDIR) $(OUTDIR)
	@mv -f $(OUTDIR)/$(WXHASKELLVER).dmg $(WXHASKELLDMG)
	echo "created: $(WXHASKELLDMG)"

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

WX-OBJS		=$(call make-objs, $(WX-IMPORTSDIR), $(WX-SOURCES))
WX-DEPS		=$(call make-deps, $(WX-IMPORTSDIR), $(WX-SOURCES))
WX-HIS		=$(call make-his,  $(WX-IMPORTSDIR), $(WX-SOURCES))
WX-HS		=$(call make-hs,   $(WX-SRCDIR),     $(WX-SOURCES))
WX-DOCS		=$(WX-HS)
WX-BINS		=$(WX-HIS) $(WX-LIBS)
WX-HCFLAGS	=$(HCFLAGS) -package-name $(WX)

# build main library
wx: wxcore wx-dirs $(WX-LIBS)

wx-dirs:
	@$(call ensure-dirs-of-files,$(WX-OBJS))

wx-clean:
	-@$(call full-remove-dir,$(WX-OUTDIR))

# source dist
wx-dist: $(WX-HS)
	@$(call cp-srcdist, $^)

# bindist
wx-bindist: wx
	@$(call cp-bindist,$(WX-OUTDIR),$(BINDIST-LIBDIR),$(WX-BINS))

# install
wx-install: wx wxcore-install
	@$(call install-files,$(WX-OUTDIR),$(LIBDIR),$(WX-BINS))
	@$(call install-pkg  ,$(LIBDIR),$(WX-PKG))

wx-uninstall:
	-@$(call uninstall-pkg  ,$(WX))
	-@$(call uninstall-files,$(WX-OUTDIR),$(LIBDIR),$(WX-BINS))

# build ghci object files
$(WX-OBJ): $(WX-OBJS)
	$(call combine-objs,$@,$^)

# build a library
$(WX-LIB): $(WX-OBJS)
	$(call make-archive,$@,$^)

# create an object file from source files.
$(WX-OBJS): $(WX-IMPORTSDIR)/%.o: $(WX-SRCDIR)/%.hs
	@$(call compile-hs,$@,$<,$(WX-HCFLAGS) -i$(WX-IMPORTSDIR):$(WXCORE-IMPORTSDIR))

# automatically include all dependency information.
-include $(WX-DEPS)


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

# build executable
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
	$(HC) $(HCFLAGS) -o $@ $^

# create an object file from source files.
$(WXD-OBJS): $(WXD-OUTDIR)/%.o: $(WXD-SRCDIR)/%.hs
	@$(call compile-hs,$@,$<,$(HCFLAGS) -i$(WXD-OUTDIR))

# automatically include all dependency information.
-include $(WXD-DEPS)

#--------------------------------------------------------------------------
# WXCORE: the direct haskell wrapper of the wxc api
#--------------------------------------------------------------------------
WXCORE		=wxcore
WXCORE-PKG	=config/$(WXCORE).pkg
WXCORE-SRCDIR	=$(WXCORE)/src
WXCORE-HPATH	=Graphics/UI/WXCore
WXCORE-OUTDIR	=$(OUTDIR)/$(WXCORE)
WXCORE-IMPORTSDIR=$(WXCORE-OUTDIR)/imports

WXCORE-OBJ	=$(WXCORE-OUTDIR)/$(WXCORE).o
WXCORE-LIB	=$(WXCORE-OUTDIR)/lib$(WXCORE).a
WXCORE-CORE-OBJ	=$(WXCORE-OUTDIR)/$(WXCORE)0.o
WXCORE-CORE-LIB	=$(WXCORE-OUTDIR)/lib$(WXCORE)0.a
WXCORE-LIBS	=$(WXCORE-CORE-LIB) $(WXCORE-CORE-OBJ) $(WXCORE-LIB) $(WXCORE-OBJ)

WXCORE-OBJS	=$(call make-objs, $(WXCORE-IMPORTSDIR), $(WXCORE-SOURCES))
WXCORE-CORE-OBJS=$(call make-objs, $(WXCORE-IMPORTSDIR), $(WXCORE-CORE-SOURCES))
WXCORE-STUB-OBJS=$(call make-objs, $(WXCORE-IMPORTSDIR), $(patsubst %,%_stub,$(WXCORE-STUBS)))
WXCORE-DEPS	=$(call make-deps, $(WXCORE-IMPORTSDIR), $(WXCORE-CORE-SOURCES) $(WXCORE-SOURCES))
WXCORE-HIS	=$(call make-his,  $(WXCORE-IMPORTSDIR), $(WXCORE-CORE-SOURCES) $(WXCORE-SOURCES))
WXCORE-HS	=$(call make-hs,   $(WXCORE-SRCDIR),     $(WXCORE-SOURCES) $(WXCORE-CORE-SOURCES))
WXCORE-GEN-HS   =$(call make-hs,   $(WXCORE-SRCDIR),     $(WXCORE-GEN-SOURCES))
WXCORE-NONGEN-HS=$(filter-out $(WXCORE-GEN-HS),$(WXCORE-HS))
WXCORE-BINS	=$(WXCORE-HIS) $(WXCORE-LIBS)
WXCORE-DOCS	=$(filter-out $(WXCORE-SRCDIR)/$(WXCORE-HPATH)/IntMap.hs,$(WXCORE-HS))
WXCORE-HCFLAGS	=$(HCFLAGS) -fvia-C -package-name $(WXCORE)


# build main library
wxcore: wxc wxd wxcore-dirs $(WXCORE-LIBS)

wxcore-dirs:
	@$(call ensure-dirs-of-files,$(WXCORE-OBJS))

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

# install
wxcore-install: wxcore wxc-install
	@$(call install-files,$(WXCORE-OUTDIR),$(LIBDIR),$(WXCORE-BINS))
	@$(call install-pkg  ,$(LIBDIR),$(WXCORE-PKG))

wxcore-uninstall:
	-@$(call uninstall-pkg  ,$(WXCORE))
	-@$(call uninstall-files,$(WXCORE-OUTDIR),$(LIBDIR),$(WXCORE-BINS))
	

# build marshall modules
$(WXCORE-SRCDIR)/$(WXCORE-HPATH)/WxcClasses.hs: $(WXD-EXE) $(WXC-SPECS-HEADER)
	$(WXD-EXE) -c $(WXD-FLAGS) $(word 1,$(WXC-SPECS-HEADER))

$(WXCORE-SRCDIR)/$(WXCORE-HPATH)/WxcDefs.hs: $(WXD-EXE) $(WXC-SPECS-EIFFEL)
	$(WXD-EXE) -d $(WXD-FLAGS) $(WXC-SPECS-EIFFEL)

$(WXCORE-SRCDIR)/$(WXCORE-HPATH)/WxcClassTypes.hs: $(WXD-EXE) $(WXC-SPECS-HEADER)
	$(WXD-EXE) -t $(WXD-FLAGS) $(word 1,$(WXC-SPECS-HEADER))

# build ghci object files
$(WXCORE-OBJ): $(WXCORE-OBJS)  $(WXCORE-STUB-OBJS)
	  $(call combine-objs,$@,$^)

$(WXCORE-CORE-OBJ): $(WXCORE-CORE-OBJS)
	  $(call combine-objs,$@,$^)

# build a library
$(WXCORE-LIB): $(WXCORE-OBJS)  $(WXCORE-STUB-OBJS)
	  $(call make-archive,$@,$^)

$(WXCORE-CORE-LIB): $(WXCORE-CORE-OBJS)
	  $(call make-archive,$@,$^)

# create an object file from source files.
$(WXCORE-CORE-OBJS) $(WXCORE-OBJS): $(WXCORE-IMPORTSDIR)/%.o: $(WXCORE-SRCDIR)/%.hs
	@$(call compile-hs,$@,$<,$(WXCORE-HCFLAGS) -i$(WXCORE-IMPORTSDIR) -Iwxc/include)

# automatically include all dependency information.
-include $(WXCORE-DEPS)


#--------------------------------------------------------------------------
# WXC: the C wrapper of the (C++) wxWindows API
#--------------------------------------------------------------------------
WXC		=wxc
WXC-CPATH	=ewxw
WXC-OUTDIR	=$(OUTDIR)/$(WXC)
WXC-SRCDIR	=$(WXC)/src
WXC-INCDIR	=$(WXC)/include

WXC-ARCHIVE	=$(WXC-OUTDIR)/lib$(WXC-LIBNAME)-$(VERSION).a
WXC-LIB		=$(WXC-OUTDIR)/$(LIB)$(WXC-LIBNAME)-$(VERSION)$(DLL)

MSC-ARCHIVE	=$(WXC-OUTDIR)/lib$(WXC-LIBNAME).a
MSC-LIB		=$(WXC-OUTDIR)/$(LIB)$(WXC-LIBNAME)$(DLL)

WXC-OBJS	=$(call make-objs, $(WXC-OUTDIR), $(WXC-SOURCES))
WXC-DEPS	=$(call make-deps, $(WXC-OUTDIR), $(WXC-SOURCES))
WXC-LIBS	=$(WXWIN-LIBS)
WXC-CXXFLAGS	=$(WXWIN-CXXFLAGS) -I$(WXC-INCDIR)


wxc: wxc-dirs $(WXC-LIB)

wxc-dirs:
	@$(call ensure-dirs-of-files,$(WXC-OBJS))

wxc-clean:
	-@$(call full-remove-dir,$(WXC-OUTDIR))

wxc-compress: wxc
	@$(call run-compress,$(WXC-LIB))

# source dist
wxc-dist: $(WXC-SRCS)
	@$(call cp-srcdist, $^)

# binary distribution. A complication is that sometimes wxWindows is in a separate dll
# and sometimes it is statically linked into wxc.dll (as with microsoft visual c++).
wxc-bindist: wxc-compress
	@$(call cp-bindist,$(dir $(WXC-LIB)),$(BINDIST-DLLDIR),$(WXC-LIB))
ifneq ($(WXWINLIB),)
	@$(call cp-bindist,$(dir $(WXWINLIB)),$(BINDIST-DLLDIR),$(basename $(WXWINLIB))*$(DLL))
endif
ifeq ($(DLL),.dll)
	@$(call cp-bindist,$(dir $(WXC-ARCHIVE)),$(BINDIST-LIBDIR),$(WXC-ARCHIVE))
endif
ifeq ($(TOOLKIT),mac)
	@$(call cp-bindist,$(dir $(WXWINLIB)),$(BINDIST-DLLDIR),$(basename $(WXWINLIB))*.r)
	@$(call cp-bindist,$(dir $(WXWINLIB)),$(BINDIST-DLLDIR),$(basename $(WXWINLIB))*.rsrc)
endif

# install
wxc-install: wxc-compress
	@$(call install-files,$(WXC-OUTDIR),$(LIBDIR),$(WXC-LIB))
ifeq ($(DLL),.dll)
	@$(call install-files,$(WXC-OUTDIR),$(LIBDIR),$(WXC-ARCHIVE))
endif

wxc-uninstall: 
	-@$(call uninstall-files,$(WXC-OUTDIR),$(LIBDIR),$(WXC-LIB) $(WXC-ARCHIVE))

# dynamic link library on mingw32/cygwin: generates wxc.dll and a libwxc.a import library
ifeq ($(WITHMSC),yes)
$(basename $(WXC-LIB)).dll: $(MSC-LIB)
	$(CP) $(MSC-LIB) $(WXC-LIB)
	$(CP) $(MSC-ARCHIVE) $(WXC-ARCHIVE)
else
$(basename $(WXC-LIB)).dll: $(WXC-OBJS)
	$(CXX) -shared -o $@ $^ $(WXC-LIBS) -Wl,--output-def,$(WXC-OUTDIR)/$(WXC).def,--out-implib,$(WXC-ARCHIVE)
endif

# dynamic link library on unix: generates single .so file
$(basename $(WXC-LIB)).so: $(WXC-OBJS)
	$(CXX) -shared -o $@ $^ $(WXC-LIBS) -Wl --soname=$@

# dynamic link library on macOSX: generates single .so file
$(basename $(WXC-LIB)).dylib: $(WXC-OBJS)
	$(CXX) -r -keep_private_externs -nostdlib -o $(WXC-OUTDIR)/master.o $^
	$(CXX) -dynamiclib -install_name $(LIBDIR)/$(notdir $@) -undefined suppress -flat_namespace -o $@ $(WXC-OUTDIR)/master.o $(filter-out %.a,$(WXC-LIBS))
	$(RM) -f $(WXC-OUTDIR)/master.o
	
# create an object file from source files
$(WXC-OBJS): $(WXC-OUTDIR)/%.o: $(WXC-SRCDIR)/%.cpp
	@$(call compile-c,$@,$<,$(WXC-CXXFLAGS))

# automatically include dependencies
-include $(WXC-DEPS)



#--------------------------------------------------------------------------
# Documentation
#--------------------------------------------------------------------------
ifdef HDOCBASE
HDOC-BASE   = -ihttp://haskell.cs.yale.edu/ghc/docs/latest/html/base,$(HDOCBASE)
endif

DOC-OUTDIR  =$(OUTDIR)/doc
DOCFILE     =$(DOC-OUTDIR)/wxhaskell.haddock
HDOCFLAGS   = --odir $(DOC-OUTDIR) --dump-interface=$(DOCFILE) --prologue=prologue.txt --html $(HDOC-BASE)
DOCSOURCES  = $(WX-DOCS) $(WXCORE-DOCS)

doc: doc-dirs $(DOCFILE)

doc-dirs:
	@$(call ensure-dir,$(DOC-OUTDIR))

doc-clean:
	-@$(call full-remove-dir,$(DOC-OUTDIR))

# copy documentation to the wxhaskell website
webdoc: doc
	cd $(DOC-OUTDIR); scp *.* $(USERNAME)@shell.sourceforge.net:/home/groups/w/wx/wxhaskell/htdocs/doc

# documentation distribution
ifeq ($(HDOCFOUND),yes)
docdist: docdist-clean doc
	@echo "-- adding documentation"
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
$(DOCFILE): prologue.txt $(DOCSOURCES)
	$(HDOC) $(HDOCFLAGS) $(DOCSOURCES)
