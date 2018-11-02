WORKDIR := .
HASKELL_SOURCES := $(wildcard *.hs)

OBJDIR := $(WORKDIR)
OBJS := $(patsubst %.hs,%.o,$(HASKELL_SOURCES))

all: $(OBJS)

.PNONY: clean dist

%.o : %.hs
	ghc -c -o $@ $<

clean:
	rm $(OBJS)
