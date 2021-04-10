WORKDIR := .
HASKELL_SOURCES := $(wildcard *.hs)

OBJDIR := $(WORKDIR)
OBJS := $(patsubst %.hs,%.o,$(HASKELL_SOURCES))

all: Test

.PNONY: clean dist

Test:
	ghc -Wall --make -o Test Test.hs

%.o : %.hs
	ghc -c -o $@ $<

clean:
	rm Test *.o *.hi
