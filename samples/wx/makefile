COMPILE=ghc -package wx
ifeq ($(shell uname),Darwin)
PREP=macosx-app
else
PREP=#
endif

APPS= BouncingBalls\
      ByeDemo\
      Controls\
      CustomControl\
      DbBrowse\
      DbConsole\
      FileBrowse\
      Grid\
      HelloWorld\
      ImageViewer\
      Layout\
      ListCtrl\
      Minimal\
      Paint\
      Print\
      Process\
      TimeFlows\
      TimeFlowsEx\

.PHONY: clean

all: $(APPS)

%: %.hs
	$(COMPILE) -o $@ $<
	$(PREP) $@

clean:
	rm -rf $(APPS) *.o *.hi *.app
