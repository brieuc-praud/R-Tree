DEBUG ?= 0

EXE = test

FC        = gfortran
SRC_DIR   = src
TEST_DIR  = test
BUILD_DIR = build
BIN_DIR   = build/bin

ifeq ($(DEBUG),0)
	FFLAGS := -O3 -march=native
else
	FFLAGS := -Og -g -ffpe-trap=invalid,zero,overflow,underflow -fcheck=all -pedantic -fbacktrace -cpp -Dpure=""
endif
FFLAGS += -Wall -Wextra
LDFLAGS =

SRC      := $(wildcard $(SRC_DIR)/*.f90)
TEST_SRC := $(wildcard $(TEST_DIR)/*.f90)
OBJ      := $(patsubst %.f90,$(BUILD_DIR)/%.o,$(notdir $(SRC) $(TEST_SRC)))

VPATH    := $(SRC_DIR):$(TEST_DIR)

all: $(BIN_DIR)/$(EXE)

$(BIN_DIR)/$(EXE): $(OBJ) | $(BIN_DIR)
	gfortran $(FFLAGS) -o $@ $^

$(BUILD_DIR)/%.o: %.f90 | $(BUILD_DIR)
	gfortran $(FFLAGS) -J$(BUILD_DIR) -c $< -o $@

$(BUILD_DIR) $(BIN_DIR):
	mkdir -p $@

clean:
	rm -rf $(BUILD_DIR)/*

test: $(BIN_DIR)/$(EXE)
	$< && printf "\033[32m test succeeded\n" || printf "\033[31m test failed\n"

# dependencies
$(BUILD_DIR)/rtree.o: $(BUILD_DIR)/sort.o
$(BUILD_DIR)/test.o: $(BUILD_DIR)/rtree.o $(BUILD_DIR)/geometry.o
