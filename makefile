#-----------------------------------------------------------------------
#  Copyright 2003, Daan Leijen.
#-----------------------------------------------------------------------

# $Id: makefile,v 1.2 2003/07/13 21:40:59 dleijen Exp $

#--------------------------------------------------------------------------
# make [all]	 - build the libraries (in "lib").
# make install	 - install the libraries as packages.
# make uninstall - uninstall the libraries
# make doc	 - generate documentation (in "doc")
# make clean	 - remove generated object files and binaries.
#       wx-clean
#       wxh-clean
#       wxc-clean
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

WXH-NONGEN-SOURCES = \
	$(filter-out Graphics/UI/WXH/WxcClasses, \
	  $(filter-out Graphics/UI/WXH/WxcClassTypes, \
	    $(filter-out Graphics/UI/WXH/WxcDefs, \
		$(WXH-SOURCES) $(WXH-CORE-SOURCES))))

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
WXC-SRCS=$(shell echo wxc/src/*.cpp)   $(shell echo wxc/src/ewxw/*.cpp) $(shell echo wxc/src/ewxw/*.h)\
	 $(shell echo wxc/include/*.h) $(shell echo wxc/include/ewxw/*.h) \
	 $(shell echo wxc/eiffel/*.e)  $(shell echo wxc/eiffel/ewxw/*.e) \
	 wxc/src/wxc.rc wxc/wxc.dsp wxc/wxc.dsw

#--------------------------------------------------------------------------
# wxhaskell project itself
#--------------------------------------------------------------------------
WXHASKELL-SOURCES= \
	config.search configure makefile \
	prologue.txt \
	bin/macosx-app bin/wxhaskell-register bin/wxhaskell-register.bat

SAMPLE-SOURCES= \
	samples/wx/Camels.hs samples/wx/desert.bmp \
	samples/wx/BouncingBalls.hs \
	samples/wx/ByeDemo.hs \
	samples/wx/Controls.hs \
	samples/wx/HelloWorld.hs \
	samples/wx/Layout.hs \
	samples/wx/Minimal.hs \
	samples/wx/Process.hs \
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
# usage: $(call make-hs, <source root path>, <file base names>)
make-hs  =$(patsubst %,$(1)/%.hs,$(2))
make-objs=$(patsubst %,$(1)/%.o,$(2))
make-deps=$(patsubst %,$(1)/%.d,$(2))
make-his =$(patsubst %,$(1)/%.hi,$(2))

# usage: $(call ensure-dir, <directory>)
ensure-dir=if test -d $(1); then :; else $(MKDIR) $(1); fi

# usage: $(call unprefix, <prefix>, <names>)
unprefix=$(patsubst $(1)%,%,$(2))

# usage: $(call reprefix, <old-prefix>, <new-prefix>, <names>)
reprefix=$(patsubst $(1)%,$(2)%,$(3))

# usage: $(call install-lib, <local dir>, <install dir>, <file base names>)
install-lib=echo install: $(3); $(INSTALL) $(3) $(dir $(patsubst $(1)%,$(2)%,$(3)))

#--------------------------------------------------------------------------
# The main targets.
#--------------------------------------------------------------------------
.SUFFIXES: .hs .hi .o .c .cpp
.PHONY: all install uninstall doc webdoc clean realclean
.PHONY: wxc wxd wxh wx
.PHONY: wxc-install wxh-install wx-install
.PHONY: wxc-uninstall wxh-uninstall wx-uninstall
.PHONY: wxc-dirs wxd-dirs wxh-dirs wx-dirs 

# global variables
OUTDIR	= out

# main targets
all:		wx
install:	wx-install
uninstall:	wx-uninstall wxh-uninstall wxc-uninstall
clean:		wxc-clean wxd-clean wxh-clean wx-clean

realclean: wxhrealclean
	-$(RM) -r $(OUTDIR)/*
	-$(RMDIR) $(OUTDIR)

#--------------------------------------------------------------------------
# Distribution
#--------------------------------------------------------------------------
.PHONY: dist srcdist bindist docdist dist-dirs
.PHONY: wxc-dist wxd-dist wxh-dist wx-dist
.PHONY: wxc-bindist wxh-bindist wx-bindist

# extract toplevel directory name  (=wxhaskell)
TOPDIRS   =$(subst \, ,$(subst /, ,$(TOPDIR)))
ROOTDIR   =$(word $(words $(TOPDIRS)),$(TOPDIRS))

# zip commands
# usage: $(call zip-bindist, <directory>, <files>)
# usage: $(call zip-srcdist, <local files>)
zip-add		=echo zipping: $(OUTDIR)/$(1); $(ZIP) -9 $(TOPDIR)/$(OUTDIR)/$(1) $(2)

zip-docdist	=$(CD) $(1); $(call zip-add,wxhaskell-doc-$(VERSION).zip, $(call unprefix,$(1)/,$(2)))
zip-bindist	=$(CD) $(1); $(call zip-add,wxhaskell-bin-$(VERSION).zip, $(call unprefix,$(1)/,$(2)))
zip-srcdist	=$(CD) ..;   $(call zip-add,wxhaskell-src-$(VERSION).zip, $(patsubst %,$(ROOTDIR)/%, $(1)))

# full distribution
dist: dist-dirs all srcdist bindist docdist

dist-dirs:
	@$(call ensure-dir,$(OUTDIR))

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
WX-HPATH	=Graphics/UI/WX
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
WX-BINS		=$(WX-OBJS) $(WX-HIS) $(WX-LIBS)
WX-HCFLAGS	=$(HCFLAGS) -package-name $(WX)

# build main library
wx: wxh wx-dirs $(WX-LIBS)

wx-dirs:
	@$(call ensure-dir,$(WX-IMPORTSDIR)/$(WX-HPATH))

wx-clean:
	-$(RM) -r $(WX-OUTDIR)/*

# bindist
wx-bindist: wx
	@$(call zip-bindist,$(WX-OUTDIR), $(WX-BINS))

# source dist
wx-dist: $(WX-HS)
	@$(call zip-srcdist, $^)

# install
wx-install: wx wxh-install
	$(INSTALLDIR) $(call reprefix,$(WX-OUTDIR),$(LIBDIR), $(WX-IMPORTSDIR)/$(WX-HPATH))
	@$(foreach file, $(WX-BINS), $(call install-lib,$(WX-OUTDIR),$(LIBDIR),$(file)); )
	env installdir=$(LIBDIR) $(HCPKG) -u -i $(WX-PKG)

wx-uninstall:
	-$(HCPKG) -r $(WX)
	-$(RM) $(call reprefix,$(WX-OUTDIR),$(LIBDIR),$(WX-BINS))
	-$(CD) $(LIBDIR); $(RMDIR) -p $(call unprefix,$(WX-OUTDIR)/,$(WX-IMPORTSDIR)/$(WX-HPATH))

# build ghci object files
$(WX-OBJ): $(WX-OBJS)
	  $(LD) -r -o $@ $^

# build a library
$(WX-LIB): $(WX-OBJS)
	  $(AR) -sr  $@ $^

# create an object file from source files.
$(WX-OBJS): $(WX-IMPORTSDIR)/%.o: $(WX-SRCDIR)/%.hs
	$(HC) -c $< -o $@ -ohi $(subst .o,.hi,$@) -odir $(WX-IMPORTSDIR)/$(*D) $(WX-HCFLAGS) -i$(WX-IMPORTSDIR):$(WXH-IMPORTSDIR)
	@# move stub files
	@-if test -f $(<D)/$(*F)_stub.h; then $(MV) $(<D)/$(*F)_stub.* $(WX-IMPORTSDIR)/$(*D); fi
	@# create dependency file
	@$(HC) $< $(WX-HCFLAGS) -M -optdep-f -optdep$(*F).d -i$(WX-IMPORTSDIR):$(WXH-IMPORTSDIR)
	@sed -e 's|$(subst .hs,,$<)\.o|$(WX-IMPORTSDIR)/$*\.o|' -e 's|\.hi|\.o|g' $(*F).d > $(WX-IMPORTSDIR)/$*.d
	@$(RM) $(*F).d

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
	@$(call ensure-dir,$(WXD-OUTDIR))

wxd-clean:
	-$(RM) -r $(WXD-OUTDIR)/*

# source dist
wxd-dist: $(WXD-HS)
	@$(call zip-srcdist, $^)

# build executable
$(WXD-EXE): $(WXD-OBJS)
	  $(HC) $(HCFLAGS) -o $@ $(WXD-OBJS)

# create an object file from source files.
$(WXD-OBJS): $(WXD-OUTDIR)/%.o: $(WXD-SRCDIR)/%.hs
	$(HC) -c $< -o $@ -ohi $(subst .o,.hi,$@) -odir $(WXD-OUTDIR)/$(*D) $(HCFLAGS) -i$(WXD-OUTDIR)
	@# move stub files
	@-if test -f $(<D)/$(*F)_stub.h; then $(MV) $(<D)/$(*F)_stub.* $(WXD-OUTDIR)/$(*D); fi
	@# create dependency file
	@$(HC) $< $(HCFLAGS) -M -optdep-f -optdep$(*F).d -i$(WXD-OUTDIR)
	@sed -e 's|$(subst .hs,,$<)\.o|$(WXD-OUTDIR)/$*\.o|' -e 's|\.hi|\.o|g' $(*F).d > $(WXD-OUTDIR)/$*.d
	@$(RM) $(*F).d

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
WXH-NONGEN-HS   =$(call make-hs,   $(WXH-SRCDIR),     $(WXH-NONGEN-SOURCES)) 
WXH-BINS	=$(WXH-OBJS) $(WXH-CORE-OBJS) $(WXH-HIS) $(WXH-LIBS)
WXH-DOCS	=$(filter-out $(WXH-SRCDIR)/$(WXH-HPATH)/IntMap.hs,$(WXH-HS))
WXH-HCFLAGS	=$(HCFLAGS) -fvia-C -package-name $(WXH)


# build main library
wxh: wxc wxd wxh-dirs $(WXH-LIBS)

wxh-dirs:
	@$(call ensure-dir,$(WXH-IMPORTSDIR)/$(WXH-HPATH))

wxh-clean:
	-$(RM) -r $(WXH-OUTDIR)/*

wxhrealclean: wxh-clean
	-$(RM) $(WXH-SRCDIR)/$(WXH-HPATH)/WxcClasses.hs
	-$(RM) $(WXH-SRCDIR)/$(WXH-HPATH)/WxcClassTypes.hs
	-$(RM) $(WXH-SRCDIR)/$(WXH-HPATH)/WxcDefs.hs

# bindist
wxh-bindist: wxh
	@$(call zip-bindist,$(WXH-OUTDIR), $(WXH-BINS))

# source dist
wxh-dist: $(WXH-NONGEN-HS)
	@$(call zip-srcdist, $^)

# install
wxh-install: wxh wxc-install
	$(INSTALLDIR) $(call reprefix,$(WXH-OUTDIR),$(LIBDIR),$(WXH-IMPORTSDIR)/$(WXH-HPATH))
	@$(foreach file, $(WXH-BINS), $(call install-lib,$(WXH-OUTDIR),$(LIBDIR),$(file)); )
	env installdir=$(LIBDIR) $(HCPKG) -u -i $(WXH-PKG)

wxh-uninstall:
	-$(HCPKG) -r $(WXH)
	-$(RM) $(call reprefix,$(WXH-OUTDIR),$(LIBDIR),$(WXH-BINS))
	-$(CD) $(LIBDIR); $(RMDIR) -p $(call unprefix,$(WXH-OUTDIR)/,$(WXH-IMPORTSDIR)/$(WXH-HPATH))
	

# build marshall modules
$(WXH-SRCDIR)/$(WXH-HPATH)/WxcClasses.hs: $(WXD-EXE) $(WXC-SPECS-HEADER)
	$(WXD-EXE) -c $(WXD-FLAGS) $(WXC-SPECS-HEADER)

$(WXH-SRCDIR)/$(WXH-HPATH)/WxcDefs.hs: $(WXD-EXE) $(WXC-SPECS-EIFFEL)
	$(WXD-EXE) -d $(WXD-FLAGS) $(WXC-SPECS-EIFFEL)

$(WXH-SRCDIR)/$(WXH-HPATH)/WxcClassTypes.hs: $(WXD-EXE) $(WXC-SPECS-HEADER)
	$(WXD-EXE) -t $(WXD-FLAGS) $(WXC-SPECS-HEADER)

# build ghci object files
$(WXH-OBJ): $(WXH-OBJS)  $(WXH-STUB-OBJS)
	  $(LD) -r -o $@ $^

$(WXH-CORE-OBJ): $(WXH-CORE-OBJS)
	  $(LD) -r -o $@ $^

# build a library
$(WXH-LIB): $(WXH-OBJS)  $(WXH-STUB-OBJS)
	  $(AR) -sr  $@ $^

$(WXH-CORE-LIB): $(WXH-CORE-OBJS)
	  $(AR) -sr  $@ $^

# create an object file from source files.
$(WXH-CORE-OBJS) $(WXH-OBJS): $(WXH-IMPORTSDIR)/%.o: $(WXH-SRCDIR)/%.hs
	$(HC) -c $< -o $@ -ohi $(subst .o,.hi,$@) -odir $(WXH-IMPORTSDIR)/$(*D) $(WXH-HCFLAGS) -i$(WXH-IMPORTSDIR) -Iwxc/include
	@# move stub files
	@-if test -f $(<D)/$(*F)_stub.h; then $(MV) $(<D)/$(*F)_stub.* $(WXH-IMPORTSDIR)/$(*D); fi
	@# create dependency file
	@$(HC) $< $(WXH-HCFLAGS) -M -optdep-f -optdep$(*F).d -i$(WXH-IMPORTSDIR)
	@sed -e 's|$(subst .hs,,$<)\.o|$(WXH-IMPORTSDIR)/$*\.o|' -e 's|\.hi|\.o|g' $(*F).d > $(WXH-IMPORTSDIR)/$*.d
	@$(RM) $(*F).d

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

ifeq ($(WITHMSC),)
WXC-ARCHIVE	=$(WXC-OUTDIR)/lib$(WXC).a
WXC-LIB		=$(WXC-OUTDIR)/$(LIB)$(WXC)$(DLL)
else
WXC-ARCHIVE	=$(WXC-OUTDIR)/lib$(WXC-LIBNAME).a
WXC-LIB		=$(WXC-OUTDIR)/$(LIB)$(WXC-LIBNAME)$(DLL)
endif

WXC-OBJS	=$(call make-objs, $(WXC-OUTDIR), $(WXC-SOURCES))
WXC-DEPS	=$(call make-deps, $(WXC-OUTDIR), $(WXC-SOURCES))
WXC-LIBS	=$(WXWIN-LIBS)
WXC-CXXFLAGS	=$(WXWIN-CXXFLAGS) -I$(WXC-INCDIR)

ifeq ($(WITHMSC),)
wxc: wxc-dirs $(WXC-LIB)
else
wxc: 
endif

wxc-dirs:
	@$(call ensure-dir,$(WXC-OUTDIR)/$(WXC-CPATH))

wxc-clean:
	-$(RM) -r $(WXC-OUTDIR)/*

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
	$(INSTALL) $(WXC-LIB) $(LIBDIR)
ifeq ($(DLL),.dll)
	$(INSTALL) $(WXC-ARCHIVE) $(LIBDIR)
endif

wxc-uninstall: 
	-$(RM) $(call reprefix,$(WXC-OUTDIR),$(LIBDIR),$(WXC-LIB) $(WXC-ARCHIVE))

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
	$(CXX) -c $< -o $@ $(WXC-CXXFLAGS) -MD
	@-if test -f $(*F).d; then mv $(*F).d $(WXC-OUTDIR)/$(*D); fi

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

# copy documentation to the wxhaskell website
webdoc: doc
	cd $(DOC-OUTDIR); scp *.* $(USERNAME)@shell.sourceforge.net:/home/groups/w/wx/wxhaskell/htdocs/doc

# documentation distribution
docdist: doc
	@$(call zip-docdist,$(OUTDIR), $(DOC-OUTDIR)/*)
	@$(call zip-docdist,., $(SAMPLE-SOURCES))

# generate documentation with haddock
$(DOCFILE): prologue.txt $(DOCSOURCES)
	$(HDOC) $(HDOCFLAGS) $(DOCSOURCES)