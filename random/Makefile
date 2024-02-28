include ../default.mk

$(EXE_DIR)/wordle-analysis: LDFLAGS+=$(shell pkg-config hunspell --libs)
$(EXE_DIR)/solitare-solver-tests: LDFLAGS+=$(shell pkg-config --libs catch2-with-main fmt)
$(EXE_DIR)/solitare-solver: LDFLAGS+=$(shell pkg-config --libs fmt)
