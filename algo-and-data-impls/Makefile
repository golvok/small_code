include ../default.mk

$(EXE_DIR)/tests/Trie_tests: LDFLAGS+=$(shell pkg-config --libs catch2-with-main)
$(EXE_DIR)/tests/heapsort_tests: LDFLAGS+=$(shell pkg-config --libs catch2-with-main)
$(EXE_DIR)/tests/mergesort_tests: LDFLAGS+=$(shell pkg-config --libs catch2-with-main)
$(EXE_DIR)/tests/memory_allocator_tests: LDFLAGS+=$(shell pkg-config --libs catch2-with-main)