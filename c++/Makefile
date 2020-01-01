WORKDIR := .
SOURCES := $(wildcard *.cpp)

OBJDIR := $(WORKDIR)
OBJECTS := $(patsubst %.cpp,%.o,$(SOURCES))

CPPFLAGS := -Wall -DUNITTEST -std=c++14 -pedantic 

all: $(OBJECTS) test

#.PNONY: clean

%.o : %.cpp
	g++ $(CPPFLAGS) -c $<

test: $(OBJECTS)
	g++ $(CPPFLAGS) -o$@ $^

