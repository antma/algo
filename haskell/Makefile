WORKDIR := .
HASKELL_SOURCES := $(wildcard *.hs)

OBJDIR := $(WORKDIR)
OBJS := $(patsubst %.hs,%.o,$(HASKELL_SOURCES))

all: $(OBJS) Test

.PNONY: clean dist

Test: $(OBJS)
	ghc --make -o Test Test.hs

%.o : %.hs
	ghc -c -o $@ $<

clean:
	rm $(OBJS)
