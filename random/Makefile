include ../default.mk

$(EXE_DIR)/wordle-analysis: LDFLAGS+=$(shell pkg-config hunspell --libs)
$(EXE_DIR)/solitaire-solver-tests: LDFLAGS+=$(shell pkg-config --libs catch2-with-main fmt)
$(EXE_DIR)/solitaire-solver: LDFLAGS+=$(shell pkg-config --libs fmt)
$(EXE_DIR)/alphabrackets: LDFLAGS+=$(shell pkg-config --libs catch2-with-main fmt) -std=c++23
$(OBJ_DIR)/alphabrackets.o: CXXFLAGS+=-std=c++23
