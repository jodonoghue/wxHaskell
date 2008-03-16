COMPILE=ghc -package wx
ifeq ($(shell uname),Darwin)
PREP=macosx-app
else
PREP=echo
endif

APPS= \
      NonModalDialog\
      StartStart\
      SubMenu\
      TextColor\

.PHONY: clean

all: $(APPS)

%: %.hs
	$(COMPILE) --make -o $@ $<
	$(PREP) $@

clean:
	rm -rf $(APPS) *.o *.hi *.app *exe *.manifest
