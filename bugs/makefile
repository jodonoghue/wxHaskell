COMPILE=ghc -package wx
ifeq ($(shell uname),Darwin)
PREP=macosx-app
else
PREP=#
endif

APPS= \
      NonModalDialog\
      SubMenu\

.PHONY: clean

all: $(APPS)

%: %.hs
	$(COMPILE) --make -o $@ $<
	$(PREP) $@

clean:
	rm -rf $(APPS) *.o *.hi *.app
