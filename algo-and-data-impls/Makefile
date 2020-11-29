.PHONY:test
test:all
	$(EXE_DIR)/tests/Trie_tests

include ../default_Makefile

$(OBJ_DIR)/tests/mergesort_tests.o: mergesort.h++
$(EXE_DIR)/tests/mergesort_tests: $(OBJ_DIR)/tests/catch_main.o

$(OBJ_DIR)/tests/heapsort_tests.o: heapsort.h++
$(EXE_DIR)/tests/heapsort_tests: $(OBJ_DIR)/tests/catch_main.o

$(OBJ_DIR)/tests/Trie_tests.o: Trie.h++
$(EXE_DIR)/tests/Trie_tests: $(OBJ_DIR)/tests/catch_main.o
