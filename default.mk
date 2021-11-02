.PHONY: all
.SUFFIXES:
MAKEFLAGS+=" -r -j 4"

MY_CFLAGS = -Wall -Wextra -pedantic -Werror -std=gnu99 -I ../include $(CFLAGS)
MY_CXXFLAGS = -Wall -Weffc++ -Wextra -pedantic -std=c++20 -g -I ../include $(CXXFLAGS)
MY_LDFLAGS = $(LDFLAGS)

BUILD_DIR=build
EXE_DIR=$(BUILD_DIR)/EXE
OBJ_DIR=$(BUILD_DIR)/OBJ
DEPS_DIR=$(BUILD_DIR)/DEPS

DIRS+=$(EXE_DIR) $(OBJ_DIR)

CPPSOURCES+=$(shell find * -type f -name "*.c++")
CSOURCES+=$(shell find * -type f -name "*.c")

OBJECTS+=$(CPPSOURCES:%.c++=$(OBJ_DIR)/%.o)
OBJECTS+=$(CSOURCES:%.c=$(OBJ_DIR)/%.o)
.PRECIOUS: $(OBJECTS)

EXES+=$(filter-out $(IGNORE_EXES),$(CPPSOURCES:%.c++=$(EXE_DIR)/%))
EXES+=$(filter-out $(IGNORE_EXES),$(CSOURCES:%.c=$(EXE_DIR)/%))

all: $(EXES)

# include all the dependency files, if any exist
EXISTING_DEP_FILES = $(shell [ -d $(DEPS_DIR) ] && find $(DEPS_DIR) -name '*.d')
ifneq (,$(EXISTING_DEP_FILES))
include $(EXISTING_DEP_FILES)
endif

$(OBJ_DIR)/%.o: %.c++
	@mkdir -p $(dir $@)
	@mkdir -p $(DEPS_DIR)/$(dir $*)
	$(CXX) -c  '$(abspath $<)' -o  $@ $(MY_CXXFLAGS)
	@$(CXX) -MM $< -MF $(DEPS_DIR)/$<.d.tmp $(MY_CXXFLAGS)
	@sed -e 's|.*:|$@:|' < $(DEPS_DIR)/$<.d.tmp > $(DEPS_DIR)/$<.d
	@sed -e 's/.*://' -e 's/\\$$//' < $(DEPS_DIR)/$<.d.tmp| fmt -1 | \
	 sed -e 's/^ *//' -e 's/$$/:/' >> $(DEPS_DIR)/$<.d
	@rm -f $(DEPS_DIR)/$<.d.tmp

$(OBJ_DIR)/%.o: %.c
	@mkdir -p $(dir $@)
	@mkdir -p $(DEPS_DIR)/$(dir $*)
	$(CC) -c  '$(abspath $<)' -o  $@ $(MY_CFLAGS)
	@$(CC) -MM $< -MF $(DEPS_DIR)/$<.d.tmp $(MY_CFLAGS)
	@sed -e 's|.*:|$@:|' < $(DEPS_DIR)/$<.d.tmp > $(DEPS_DIR)/$<.d
	@sed -e 's/.*://' -e 's/\\$$//' < $(DEPS_DIR)/$<.d.tmp| fmt -1 | \
	 sed -e 's/^ *//' -e 's/$$/:/' >> $(DEPS_DIR)/$<.d
	@rm -f $(DEPS_DIR)/$<.d.tmp

$(EXE_DIR)/%: $(OBJ_DIR)/%.o
	@mkdir -p $(dir $@)
	$(CXX) $^ -o $@ $(MY_LDFLAGS)

run__%: $(EXE_DIR)/%
	$(RUN_UNDER) "$<"

clean:
	rm -f $(EXES)
	rm -f $(OBJECTS)
