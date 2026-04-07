DEBUG ?= 0

EXE = test

FC        = gfortran
SRC_DIR   = src
TEST_DIR  = test
BUILD_DIR = build
BIN_DIR   = build/bin

ifeq ($(DEBUG),0)
	FFLAGS := -O3
else
	FFLAGS := -Og -g -ffpe-trap=invalid,zero,overflow,underflow -fcheck=all -pedantic -fbacktrace -cpp -Dpure=""
endif
FFLAGS += -Wall -Wextra -Wimplicit-interface -Wunderflow
LDFLAGS =

SRC      := $(wildcard $(SRC_DIR)/*.f90)
TEST_SRC := $(wildcard $(TEST_DIR)/*.f90)

TEST_PROG_SRC   := $(wildcard $(TEST_DIR)/*_test.f90)
TEST_HELPER_SRC := $(filter-out $(TEST_PROG_SRC),$(TEST_SRC))

SRC_OBJ          := $(patsubst $(SRC_DIR)/%.f90,$(BUILD_DIR)/%.o,$(SRC))
TEST_PROG_OBJ    := $(patsubst $(TEST_DIR)/%.f90,$(BUILD_DIR)/%.o,$(TEST_PROG_SRC))
TEST_HELPER_OBJ  := $(patsubst $(TEST_DIR)/%.f90,$(BUILD_DIR)/%.o,$(TEST_HELPER_SRC))

TEST_EXE := $(patsubst $(TEST_DIR)/%.f90,$(BIN_DIR)/%,$(TEST_PROG_SRC))

VPATH := $(SRC_DIR):$(TEST_DIR)

all: $(TEST_EXE)

$(BIN_DIR)/%: $(BUILD_DIR)/%.o $(SRC_OBJ) $(TEST_HELPER_OBJ) | $(BIN_DIR)
	$(FC) $(FFLAGS) -o $@ $^

$(BUILD_DIR)/%.o: %.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -c $< -o $@

$(BUILD_DIR) $(BIN_DIR):
	mkdir -p $@

clean:
	rm -rf $(BUILD_DIR)/*

test: $(TEST_EXE)
	@for t in $^; do \
		name=$${t##*/}; \
		printf "Running $$name... "; \
		if $$t; then \
			printf "\033[32mOK\033[0m\n"; \
		else \
			printf "\033[31mFAIL\033[0m\n"; \
		fi; \
	done

# dependencies
$(BUILD_DIR)/sort_test.o: $(BUILD_DIR)/sort.o
$(BUILD_DIR)/rtree.o: $(BUILD_DIR)/sort.o
$(BUILD_DIR)/rtree_test.o: $(BUILD_DIR)/rtree.o $(BUILD_DIR)/geometry.o
