.PHONY: all
.SUFFIXES:
MAKEFLAGS+=" -r "

EXE_DIR=EXE
OBJ_DIR=OBJ

DIRS=$(EXE_DIR) $(OBJ_DIR)

SOURCES=$(shell find * -type f -name "*.c++")

OBJECTS=$(SOURCES:%.c++=$(OBJ_DIR)/%.o)

EXES=$(SOURCES:%.c++=$(EXE_DIR)/%)

all: $(EXES)

$(OBJ_DIR)/%.o: %.c++ | $(OBJ_DIR)
	g++ -c -Wall -Weffc++ -Wextra -Werror -pedantic -std=c++11 $< -o $@ -ggdb -D_GLIBCXX_DEBUG

$(EXE_DIR)/%: $(OBJ_DIR)/%.o | $(EXE_DIR)
	g++ -Wall -Weffc++ -Wextra -Werror -pedantic -std=c++11 $< -o $@ -ggdb -D_GLIBCXX_DEBUG

$(DIRS):
	mkdir -p $@

clean:
	rm -r $(EXES)
