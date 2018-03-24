WORKDIR := .
SOURCES := $(wildcard *.d)

OBJDIR := $(WORKDIR)
OBJECTS := $(patsubst %.d,%.o,$(SOURCES))

DMDFLAGS := -unittest

all: test

#.PNONY: clean

%.o : %.d
	dmd $(DMDFLAGS) -c $<

test: $(OBJECTS)
	dmd $(DMDFLAGS) -of$@ $^
