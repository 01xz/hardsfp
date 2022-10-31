ifeq ($(VERILATOR_ROOT),)
VERILATOR = verilator
else
export VERILATOR_ROOT
VERILATOR = $(VERILATOR_ROOT)/bin/verilator
endif

TARGET_LIST ?= SFPMul

CSRC_DIR  ?= csrc
VSRC_DIR  ?= ./../../rtl
BUILD_DIR ?= build

VCD ?= $(foreach target, $(TARGET_LIST), $(BUILD_DIR)/$(target).vcd)
EXE ?= $(foreach target, $(TARGET_LIST), $(BUILD_DIR)/V$(target))

sim: $(VCD) $(EXE)

SOFTSFP_HOME = /workspaces/hardsfp/utils/softsfp

include $(SOFTSFP_HOME)/scripts/softsfp.mk

VERILATOR_FLAGS += -cc --exe --build
VERILATOR_FLAGS += -Wall -Wno-lint
VERILATOR_FLAGS += --Mdir $(BUILD_DIR)

INC_PATH ?= $(SOFTSFP_HOME)/include

INCFLAGS = $(addprefix -I, $(INC_PATH))
CFLAGS  += $(INCFLAGS)
LDFLAGS ?=

$(BUILD_DIR)/%.vcd: $(BUILD_DIR)/V%
	@$^

$(BUILD_DIR)/V%: $(VSRC_DIR)/%.v $(CSRC_DIR)/C%.cpp $(SOFTSFP_ARCHIVE)
	$(VERILATOR) $(VERILATOR_FLAGS) --trace \
		$(addprefix -CFLAGS , $(CFLAGS)) $(addprefix -LDFLAGS , $(LDFLAGS)) \
		-o $(abspath $@) --top $(*F) $^

clean:
	-rm -rf $(BUILD_DIR)

.PHONY: sim clean
