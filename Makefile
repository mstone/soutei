# Compiling Soutei library and sample applications

GHC := ghc
GHC_MAKE = $(GHC) --make
GHC_OPTS := -O2
GHC_INCLUDES = `$(GHC) --print-libdir`/include


PROGS := soutei-server soutei-cli

SOURCES := $(wildcard Soutei/*.hs)
MODULES := $(subst /,.,$(patsubst %.hs,%,$(SOURCES)))
OBJECTS := $(patsubst %.hs,$(BUILD_DIR)/objs/%.o,$(SOURCES))
HC_CMD := $(GHC_MAKE) $(GHC_OPTS) -I$(GHC_INCLUDES)

all: $(PROGS)

test:
	cd demo/metcast-channels/ && $(MAKE) test

clean::
	cd demo/metcast-channels/ && $(MAKE) clean


%: %.hs
	$(HC_CMD) -o $@ $*.hs

clean::
	rm -f *.hi *.o $(PROGS)
	rm -f Soutei/*.hi
	rm -f Soutei/*.o

# The following targets require the FCGI library

FCGIP=XXXSet-FGCI-library-here
soutei-fcgi: soutei-fcgi.hs
	ghc -O2 --make -I$(FCGIP)/fcgi/include -i$(FCGIP) \
	$(FCGIP)/fcgi/fcgiapp.o $(FCGIP)/fcgi/os_unix.o \
	soutei-fcgi.hs -o $@

soutei-pipelined: soutei-pipelined.hs
	ghc -O2 --make -i$(FCGIP) \
	soutei-pipelined.hs -o $@

soutei-mux: soutei-mux.hs
	ghc -O2 --make -i$(FCGIP) $(FCGIP)/System/sys_open.o \
	soutei-mux.hs -o $@
