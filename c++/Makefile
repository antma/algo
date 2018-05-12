WORKDIR := .
SOURCES := $(wildcard *.cpp)

OBJDIR := $(WORKDIR)
OBJECTS := $(patsubst %.cpp,%.o,$(SOURCES))

CPPFLAGS := -Wall 

all: $(OBJECTS)

#.PNONY: clean

%.o : %.cpp
	g++ $(CPPFLAGS) -c $<

test: $(OBJECTS)
	dmd $(DMDFLAGS) -of$@ $^
