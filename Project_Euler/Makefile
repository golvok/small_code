.PHONY: all $OBJECTS

EXE_DIR=EXE

SOURCES=$(shell find . -name "*.c++")

OBJECTS=$(SOURCES:%.c++=%.o)

EXES=$(SOURCES: %.c++ = %)

all: $(OBJECTS)

%.o: %.c++ | $(EXE_DIR)
	g++ -Wall -Weffc++ -Wextra -Werror -pedantic -std=c++11 $< -o $(EXE_DIR)/$(@:%.o=%) -ggdb -D_GLIBCXX_DEBUG
	g++ -c -Wall -Weffc++ -Wextra -Werror -pedantic -std=c++11 $< -o $@ -ggdb -D_GLIBCXX_DEBUG

$(EXE_DIR):
	mkdir -p $(EXE_DIR)

.SUFFIXES:
