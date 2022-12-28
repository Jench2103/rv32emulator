TARGET = obj/emulator
CPP_HEADERS := $(wildcard *.hpp)
CPP_SRCS := $(wildcard *.cpp)
TEST_DIR ?= test_assembly
TEST_PREFIX ?= test_
TEST_INSTR ?= undefined
TEST_EXT ?= .txt
TEST_ASM ?= test.S
TRANSLATE_OUTPUT_DIR ?= output/

all: $(TARGET)
$(TARGET): $(CPP_HEADERS) $(CPP_SRCS)
	@mkdir -p "$(dir $(TARGET))"
	@g++ $(CPP_SRCS) -g -std=c++11 -o $(TARGET)

.PHONY: emulate
emulate: $(TARGET) $(TEST_ASM)
	@mkdir -p $(TRANSLATE_OUTPUT_DIR)
	@$(TARGET) $(TEST_ASM) $(TRANSLATE_OUTPUT_DIR)

.PHONY: test
test: $(TARGET) $(TEST_DIR)/$(TEST_PREFIX)$(TEST_INSTR)$(TEST_EXT)
	@$(TARGET) $(TEST_DIR)/$(TEST_PREFIX)$(TEST_INSTR)$(TEST_EXT)

.PHONY: clean
clean:
	@rm -f "$(dir $(TARGET))"/*
