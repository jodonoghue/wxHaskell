#-----------------------------------------------------------------------
#  Copyright (c) 2003, Daan Leijen.
#  This file is distributed under the wxWindows library license.
#  See "license.txt" for more details.
#-----------------------------------------------------------------------

# $Id: makefile,v 1.14 2003/07/21 08:48:14 dleijen Exp $

#--------------------------------------------------------------------------
# make [all]	 - build the libraries (in "lib").
# make install	 - install the libraries as packages.
# make uninstall - uninstall the libraries
# make doc	 - generate documentation (in "doc")
# make clean	 - remove generated object files and binaries.
#       wx-clean
#       wxh-clean
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
# wxh
#--------------------------------------------------------------------------
WXH-SOURCES = \
	Graphics/UI/WXH/WxcDefs \
	Graphics/UI/WXH/WxcClassTypes \
	Graphics/UI/WXH/IntMap \
	Graphics/UI/WXH/Types \
	Graphics/UI/WXH/Draw \
	Graphics/UI/WXH/Events \
	Graphics/UI/WXH/Layout \
	Graphics/UI/WXH/Process \
	Graphics/UI/WXH/Frame \
	Graphics/UI/WXH/Dialogs \
	Graphics/UI/WXH/Image \
	Graphics/UI/WXH/Controls \
	Graphics/UI/WXH

WXH-CORE-SOURCES = \
	Graphics/UI/WXH/WxcTypes \
	Graphics/UI/WXH/WxcClasses


WXH-GEN-SOURCES = \
	Graphics/UI/WXH/WxcClasses \
	Graphics/UI/WXH/WxcClassTypes \
	Graphics/UI/WXH/WxcDefs 
	
# all sources that generate stub files (ie. containing: foreign import "wrapper")
WXH-STUBS = \
	Graphics/UI/WXH/Events

#--------------------------------------------------------------------------
# wxdirect
#--------------------------------------------------------------------------
WXD-SOURCES = \
	Map Set MultiSet \
	HaskellNames Types Classes \
	ParseEiffel ParseC \
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
	ewxw_main extra wrapper eljevent eljmime

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
	toolbar treectrl validator window wizard \
	findrepldlg artprov tipwnd icnbndl ctxhelp singleinst

WXC-SOURCES = \
	$(WXC-CORE) $(patsubst %,ewxw/elj%,$(WXC-EWXW))

WXC-SPECS-EIFFEL = \
	wxc/eiffel/wxc_defs.e  wxc/eiffel/ewxw/wx_defs.e

WXC-SPECS-HEADER = \
	wxc/include/wxc.h wxc/include/ewxw/wxc_glue.h

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
	bin/macosx-app bin/reimp.exe \
	bin/wxhaskell-register bin/wxhaskell-register.bat

SAMPLE-SOURCES= \
	samples/wx/Camels.hs samples/wx/desert.bmp \
	samples/wx/BouncingBalls.hs \
	samples/wx/ByeDemo.hs \
	samples/wx/Controls.hs \
	samples/wx/HelloWorld.hs \
	samples/wx/Layout.hs \
	samples/wx/Minimal.hs \
	samples/wx/Process.hs \
	samples/wx/Paint.hs \
	samples/wx/ImageViewer.hs \
	\
	samples/wxh/BouncingBalls.hs \
	samples/wxh/ByeDemo.hs \
	samples/wxh/HelloWorld.hs \
	samples/wxh/Minimal.hs \
	samples/wxh/Paint.hs \
	samples/wxh/ImageViewer.hs samples/wxh/ImageViewer.ico

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
run-silent	=$(1) > /dev/null 2> /dev/null
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
ensure-dirs-of-files=$(foreach dir,$(call dirs-of-files,$(1)),$(call ensure-dir,$(dir)) &&) :

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
uninstall-filesx=$(foreach file,$(2),$(call uninstall-file,$(file)) &&) \
		 $(CD) $(1) && \
		 $(foreach dir,$(call dirs-of-files,$(call relative-to,$(1),$(2))),$(call uninstall-dir,$(1),$(dir)) &&) \
		 :
uninstall-files =$(call uninstall-filesx,$(2),$(call relative-fromto,$(1),$(2),$(3)))

# install packages
# usage: $(call install-pkg,<install dir>,<package file>)
# usage: $(call uninstall-pkg,<package name>)
install-pkg=env installdir=$(1) $(HCPKG) -u -i $(2)
uninstall-pkg=if $(call run-silent,$(HCPKG) -s $(1)); then echo "unregister package: $(1)" && $(HCPKG) -r $(1); fi


#--------------------------------------------------------------------------
# The main targets.
#--------------------------------------------------------------------------
.SUFFIXES: .hs .hi .o .c .cpp
.PHONY: all install uninstall doc webdoc clean realclean
#.PHONY: wxc wxd wxh wx
#.PHONY: wxc-install wxh-install wx-install
#.PHONY: wxc-uninstall wxh-uninstall wx-uninstall
#.PHONY: wxc-dirs wxd-dirs wxh-dirs wx-dirs 

# global variables
OUTDIR	= out

# main targets
all:		wx
install:	wx-install
uninstall:	wx-uninstall wxh-uninstall wxc-uninstall
clean:		wxc-clean wxd-clean wxh-clean wx-clean 

realclean: wxhrealclean 
	-@$(call full-remove-dir,$(OUTDIR)) 

#--------------------------------------------------------------------------
# Distribution
#--------------------------------------------------------------------------
.PHONY: dist srcdist bindist docdist dist-dirs
#.PHONY: wxc-dist wxd-dist wxh-dist wx-dist
#.PHONY: wxc-bindist wxh-bindist wx-bindist

DIST-OUTDIR	=$(OUTDIR)
DIST-DOC	=$(DIST-OUTDIR)/wxhaskell-doc-$(VERSION).zip
DIST-SRC	=$(DIST-OUTDIR)/wxhaskell-src-$(VERSION).zip
DIST-BIN	=$(DIST-OUTDIR)/wxhaskell-bin-$(TOOLKIT)-$(VERSION).zip
DISTS		=$(DIST-DOC) $(DIST-SRC) $(DIST-BIN)

# extract toplevel directory name  (=wxhaskell)
TOPDIRS   =$(subst \, ,$(subst /, ,$(TOPDIR)))
ROOTDIR   =$(word $(words $(TOPDIRS)),$(TOPDIRS))

# zip commands
# usage: $(call zip-bindist,<relative directory>,<files>)
# usage: $(call zip-srcdist,<local files>)
zip-add		=echo zipping: $(1); $(ZIP) -9 $(TOPDIR)/$(1) $(2)

zip-docdist	=$(CD) $(1); $(call zip-add,$(DIST-DOC), $(call relative-to,$(1),$(2)))
zip-bindist	=$(CD) $(1); $(call zip-add,$(DIST-BIN), $(call relative-to,$(1),$(2)))
zip-srcdist	=$(CD) ..;   $(call zip-add,$(DIST-SRC), $(patsubst %,$(ROOTDIR)/%, $(1)))

# full distribution
dist: dist-dirs all srcdist bindist docdist

dist-dirs:
	@$(call ensure-dirs-of-files,$(DISTS))

dist-clean:
	-@$(call safe-remove-files,$(DISTS))

# source distribution
srcdist: dist-dirs wxc-dist wxd-dist wxh-dist wx-dist
	@$(call zip-srcdist, $(WXHASKELL-SOURCES))
	@$(call zip-srcdist, $(SAMPLE-SOURCES))

# binary distribution
bindist: all dist-dirs wxc-bindist wxh-bindist wx-bindist
	@$(call zip-bindist,config, wxh.pkg wx.pkg)
	@$(call zip-bindist,bin, wxhaskell-register)
ifeq ($(DLL),.dll)
	@$(call zip-bindist,bin, wxhaskell-register.bat)
endif
ifeq ($(DLL),.dylib)
	@$(call zip-bindist,bin, macosx-app)
endif




#--------------------------------------------------------------------------
# WX: the medium level abstraction on wxh
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
wx: wxh wx-dirs $(WX-LIBS)

wx-dirs:
	@$(call ensure-dirs-of-files,$(WX-OBJS))

wx-clean:
	-@$(call full-remove-dir,$(WX-OUTDIR))

# bindist
wx-bindist: wx
	@$(call zip-bindist,$(WX-OUTDIR),$(WX-BINS))

# source dist
wx-dist: $(WX-HS)
	@$(call zip-srcdist, $^)

# install
wx-install: wx wxh-install
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
	@$(call compile-hs,$@,$<,$(WX-HCFLAGS) -i$(WX-IMPORTSDIR):$(WXH-IMPORTSDIR))

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
WXD-FLAGS	= --wxc $(WXC) -o $(WXH-SRCDIR)/$(WXH-HPATH)

# build executable
wxd: wxd-dirs $(WXD-EXE)

wxd-dirs:
	@$(call ensure-dirs-of-files,$(WXD-OBJS))

wxd-clean:
	-@$(call full-remove-dir,$(WXD-OUTDIR))


# source dist
wxd-dist: $(WXD-HS)
	@$(call zip-srcdist, $^)

# build executable
$(WXD-EXE): $(WXD-OBJS)
	$(HC) $(HCFLAGS) -o $@ $^

# create an object file from source files.
$(WXD-OBJS): $(WXD-OUTDIR)/%.o: $(WXD-SRCDIR)/%.hs
	@$(call compile-hs,$@,$<,$(HCFLAGS) -i$(WXD-OUTDIR))

# automatically include all dependency information.
-include $(WXD-DEPS)

#--------------------------------------------------------------------------
# WXH: the direct haskell wrapper of the wxc api
#--------------------------------------------------------------------------
WXH		=wxh
WXH-PKG		=config/$(WXH).pkg
WXH-SRCDIR	=$(WXH)/src
WXH-HPATH	=Graphics/UI/WXH
WXH-OUTDIR	=$(OUTDIR)/$(WXH)
WXH-IMPORTSDIR	=$(WXH-OUTDIR)/imports

WXH-OBJ		=$(WXH-OUTDIR)/$(WXH).o
WXH-LIB		=$(WXH-OUTDIR)/lib$(WXH).a
WXH-CORE-OBJ	=$(WXH-OUTDIR)/$(WXH)core.o
WXH-CORE-LIB	=$(WXH-OUTDIR)/lib$(WXH)core.a
WXH-LIBS	=$(WXH-CORE-LIB) $(WXH-CORE-OBJ) $(WXH-LIB) $(WXH-OBJ)

WXH-OBJS	=$(call make-objs, $(WXH-IMPORTSDIR), $(WXH-SOURCES))
WXH-CORE-OBJS	=$(call make-objs, $(WXH-IMPORTSDIR), $(WXH-CORE-SOURCES))
WXH-STUB-OBJS	=$(call make-objs, $(WXH-IMPORTSDIR), $(patsubst %,%_stub,$(WXH-STUBS)))
WXH-DEPS	=$(call make-deps, $(WXH-IMPORTSDIR), $(WXH-CORE-SOURCES) $(WXH-SOURCES))
WXH-HIS		=$(call make-his,  $(WXH-IMPORTSDIR), $(WXH-CORE-SOURCES) $(WXH-SOURCES))
WXH-HS		=$(call make-hs,   $(WXH-SRCDIR),     $(WXH-SOURCES) $(WXH-CORE-SOURCES))
WXH-GEN-HS      =$(call make-hs,   $(WXH-SRCDIR),     $(WXH-GEN-SOURCES))
WXH-NONGEN-HS   =$(filter-out $(WXH-GEN-HS),$(WXH-HS))
WXH-BINS	=$(WXH-HIS) $(WXH-LIBS)
WXH-DOCS	=$(filter-out $(WXH-SRCDIR)/$(WXH-HPATH)/IntMap.hs,$(WXH-HS))
WXH-HCFLAGS	=$(HCFLAGS) -fvia-C -package-name $(WXH)


# build main library
wxh: wxc wxd wxh-dirs $(WXH-LIBS)

wxh-dirs:
	@$(call ensure-dirs-of-files,$(WXH-OBJS))

wxh-clean:
	-@$(call full-remove-dir,$(WXH-OUTDIR))

wxhrealclean: wxh-clean
	-@$(call safe-remove-files,$(WXH-GEN-HS))

# bindist
wxh-bindist: wxh
	@$(call zip-bindist,$(WXH-OUTDIR), $(WXH-BINS))

# source dist
wxh-dist: $(WXH-NONGEN-HS)
	@$(call zip-srcdist, $^)

# install
wxh-install: wxh wxc-install
	@$(call install-files,$(WXH-OUTDIR),$(LIBDIR),$(WXH-BINS))
	@$(call install-pkg  ,$(LIBDIR),$(WXH-PKG))

wxh-uninstall:
	-@$(call uninstall-pkg  ,$(WXH))
	-@$(call uninstall-files,$(WXH-OUTDIR),$(LIBDIR),$(WXH-BINS))
	

# build marshall modules
$(WXH-SRCDIR)/$(WXH-HPATH)/WxcClasses.hs: $(WXD-EXE) $(WXC-SPECS-HEADER)
	$(WXD-EXE) -c $(WXD-FLAGS) $(WXC-SPECS-HEADER)

$(WXH-SRCDIR)/$(WXH-HPATH)/WxcDefs.hs: $(WXD-EXE) $(WXC-SPECS-EIFFEL)
	$(WXD-EXE) -d $(WXD-FLAGS) $(WXC-SPECS-EIFFEL)

$(WXH-SRCDIR)/$(WXH-HPATH)/WxcClassTypes.hs: $(WXD-EXE) $(WXC-SPECS-HEADER)
	$(WXD-EXE) -t $(WXD-FLAGS) $(WXC-SPECS-HEADER)

# build ghci object files
$(WXH-OBJ): $(WXH-OBJS)  $(WXH-STUB-OBJS)
	  $(call combine-objs,$@,$^)

$(WXH-CORE-OBJ): $(WXH-CORE-OBJS)
	  $(call combine-objs,$@,$^)

# build a library
$(WXH-LIB): $(WXH-OBJS)  $(WXH-STUB-OBJS)
	  $(call make-archive,$@,$^)

$(WXH-CORE-LIB): $(WXH-CORE-OBJS)
	  $(call make-archive,$@,$^)

# create an object file from source files.
$(WXH-CORE-OBJS) $(WXH-OBJS): $(WXH-IMPORTSDIR)/%.o: $(WXH-SRCDIR)/%.hs
	@$(call compile-hs,$@,$<,$(WXH-HCFLAGS) -i$(WXH-IMPORTSDIR) -Iwxc/include)

# automatically include all dependency information.
-include $(WXH-DEPS)


#--------------------------------------------------------------------------
# WXC: the C wrapper of the (C++) wxWindows API
#--------------------------------------------------------------------------
WXC		=wxc
WXC-CPATH	=ewxw
WXC-OUTDIR	=$(OUTDIR)/$(WXC)
WXC-SRCDIR	=$(WXC)/src
WXC-INCDIR	=$(WXC)/include

ifeq ($(WITHMSC),yes)
WXC-ARCHIVE	=$(WXC-OUTDIR)/lib$(WXC-LIBNAME).a
WXC-LIB		=$(WXC-OUTDIR)/$(LIB)$(WXC-LIBNAME)$(DLL)
else
WXC-ARCHIVE	=$(WXC-OUTDIR)/lib$(WXC).a
WXC-LIB		=$(WXC-OUTDIR)/$(LIB)$(WXC)$(DLL)
endif

WXC-OBJS	=$(call make-objs, $(WXC-OUTDIR), $(WXC-SOURCES))
WXC-DEPS	=$(call make-deps, $(WXC-OUTDIR), $(WXC-SOURCES))
WXC-LIBS	=$(WXWIN-LIBS)
WXC-CXXFLAGS	=$(WXWIN-CXXFLAGS) -I$(WXC-INCDIR)

ifeq ($(WITHMSC),yes)
wxc: 
else
wxc: wxc-dirs $(WXC-LIB)
endif

wxc-dirs:
	@$(call ensure-dirs-of-files,$(WXC-OBJS))

wxc-clean:
	-@$(call full-remove-dir,$(WXC-OUTDIR))

wxc-compress: wxc
	@$(call run-compress,$(WXC-LIB))

# binary distribution. A bit complicated since on windows, we want to put .dll modules
# in a lib/bin directory so that they are probably in the PATH when installed on the
# ghc directory. Further complication is that sometimes wxWindows is in a separate dll
# and sometimes it is statically linked into wxc.dll (as with microsoft visual c++).
wxc-bindist: wxc-compress
ifeq ($(DLL),.dll)
	@#put the dll in a temporary "bin" dir so that it can be put in the ghc bin directory on installation
	@$(MKDIR) $(WXC-OUTDIR)/bin
	@$(CP) $(WXC-LIB) $(WXC-OUTDIR)/bin
ifneq ($(WXWINLIB),)
	@$(CP) $(WXWINLIB) $(WXC-OUTDIR)/bin
	@$(call zip-bindist,$(WXC-OUTDIR), bin/$(notdir $(WXWINLIB)))
endif
	@$(call zip-bindist,$(WXC-OUTDIR), bin/$(notdir $(WXC-LIB)) $(WXC-ARCHIVE))
	@-$(RM) $(WXC-OUTDIR)/bin/*
	@-$(RMDIR) $(WXC-OUTDIR)/bin
else
ifneq ($(WXWINLIB),)
	@$(call zip-bindist,$(dir $(WXWINLIB)),$(notdir $(WXWINLIB)))
endif
	@$(call zip-bindist,$(WXC-OUTDIR), $(WXC-LIB))
endif

# source dist
wxc-dist: $(WXC-SRCS)
	@$(call zip-srcdist, $^)

# install
wxc-install: wxc-compress
	@$(call install-files,$(WXC-OUTDIR),$(LIBDIR),$(WXC-LIB))
ifeq ($(DLL),.dll)
	@$(call install-files,$(WXC-OUTDIR),$(LIBDIR),$(WXC-ARCHIVE))
endif

wxc-uninstall: 
	-@$(call uninstall-files,$(WXC-OUTDIR),$(LIBDIR),$(WXC-LIB) $(WXC-ARCHIVE))

# dynamic link library on mingw32/cygwin: generates wxc.dll and a libwxc.a import library
$(basename $(WXC-LIB)).dll: $(WXC-OBJS)
	$(CXX) -shared -o $@ $^ $(WXC-LIBS) -Wl,--output-def,$(WXC-OUTDIR)/$(WXC).def,--out-implib,$(WXC-ARCHIVE)
	
# dynamic link library on unix: generates single .so file
$(basename $(WXC-LIB)).so: $(WXC-OBJS)
	$(CXX) -shared -o $@ $^ $(WXC-LIBS) -Wl --soname=$@

# dynamic link library on macOSX: generates single .so file
$(basename $(WXC-LIB)).dylib: $(WXC-OBJS)
	$(CXX) -r -keep_private_externs -nostdlib -o $(WXC-OUTDIR)/master.o $^
	$(CXX) -dynamiclib -undefined suppress -flat_namespace -o $@ $(WXC-OUTDIR)/master.o $(filter-out %.a,$(WXC-LIBS))
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
DOCSOURCES  = $(WX-DOCS) $(WXH-DOCS)

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
docdist: doc
	@echo "-- adding documentation"
	@$(call zip-docdist,$(OUTDIR), $(DOC-OUTDIR)/*)
else
docdist:
	@echo "-- haddock not available: documentation can not be added"
endif
	@echo "-- adding samples"
	@$(call zip-docdist,., $(SAMPLE-SOURCES))
	
# generate documentation with haddock
$(DOCFILE): prologue.txt $(DOCSOURCES)
	$(HDOC) $(HDOCFLAGS) $(DOCSOURCES)