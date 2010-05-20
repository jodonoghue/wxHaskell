COMPILE=ghc -package wx

ifeq ($(shell uname),Darwin)
%: %.hs
	$(COMPILE) -o $@ $<
	macosx-app $@
else
%: %.hs
	$(COMPILE) -o $@ $<
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
      TestTaskBarIcon \
      TimeFlows\
      TimeFlowsEx\

.PHONY: clean

all: $(APPS)

clean:
	rm -rf $(APPS) *.o *.hi *.app *exe *.manifest
