# Run the regression test suite for Soutei or for the
# Metcast Channels + Soutei combo

# The Soutei engine root
ROOT=../../

GHC := ghc
GHC_MAKE = $(GHC) --make
GHC_OPTS := -O2
GHC_INCLUDES = `$(GHC) --print-libdir`/include

HC_CMD := $(GHC_MAKE) $(GHC_OPTS) -I$(GHC_INCLUDES) -i$(ROOT) 

test: soutei-metcast-tests
	./soutei-metcast-tests 10 10

soutei-metcast-tests: soutei-metcast-tests.hs
	$(HC_CMD) $^ -o $@ 

clean::
	rm -f soutei-metcast-tests


clean::
	rm -f *.hi *.o $(PROGS)
	rm -f Soutei/*.hi
	rm -f Soutei/*.o
