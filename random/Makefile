include ../default.mk

$(EXE_DIR)/wordle-analysis: LDFLAGS+=$(shell pkg-config hunspell --libs)
$(EXE_DIR)/solitare-solver-tests: $(OBJ_DIR)/catch_main.o
