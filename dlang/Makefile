WORKDIR := .
SOURCES := $(wildcard *.d)

OBJDIR := $(WORKDIR)
OBJECTS := $(patsubst %.d,%.o,$(SOURCES))

DMDFLAGS := -unittest
DMD := dmd

all: test

#.PNONY: clean

%.o : %.d
	$(DMD) $(DMDFLAGS) -c $<

test: $(OBJECTS)
	$(DMD) $(DMDFLAGS) -of$@ $^
