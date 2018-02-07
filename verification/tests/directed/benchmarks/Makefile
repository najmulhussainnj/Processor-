#=======================================================================
# UCB VLSI FLOW: Makefile for riscv-bmarks
#-----------------------------------------------------------------------
# Yunsup Lee (yunsup@cs.berkeley.edu)
#
include ./Makefile.inc

default: all

src_dir = .

instname = shakti-bmarks
instbasedir = ./outputfiles
base_address = 2147483648
linesize=$$(($(XLEN)/8))
simdir = ./sim



#--------------------------------------------------------------------
# Sources
#--------------------------------------------------------------------

bmarks = median \
	qsort \
	rsort \
	towers \
	vvadd \
	multiply \
	mm \
	dhrystone \
	dhry\
	spmv \
	mt-vvadd \
	mt-matmul \
	pmp \

#--------------------------------------------------------------------
# Build rules
#--------------------------------------------------------------------

RISCV_PREFIX ?= riscv$(XLEN)-unknown-elf-
RISCV_GCC ?= $(RISCV_PREFIX)gcc
RISCV_GCC_OPTS ?= -DCONFIG_RISCV64=True -DPREALLOCATE=1 -DITERATIONS=10000  -mcmodel=medany -static -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf -march=rv64imafd -mbranch-cost=2 -frename-registers -w
RISCV_LINK ?= $(RISCV_GCC) -T $(src_dir)/common/test.ld $(incs)
RISCV_LINK_OPTS ?= -static -nostartfiles -lgcc -T $(src_dir)/common/test.ld
RISCV_OBJDUMP ?= $(RISCV_PREFIX)objdump -D #--disassemble-all --disassemble-zeroes --section=.text --section=.text.startup --section=.data
RISCV_SIM ?= spike --isa=rv$(XLEN)gc
RISCV_HEX = elf2hex $(linesize) 32768

incs  += -I$(src_dir)/../env -I$(src_dir)/common $(addprefix -I$(src_dir)/, $(bmarks))
objs  :=

define compile_template
$(1).riscv: $(wildcard $(src_dir)/$(1)/*) $(wildcard $(src_dir)/common/*)
	@$$(RISCV_GCC) $$(incs) $$(RISCV_GCC_OPTS) $$(RISCV_LINK_OPTS) -o $$@ $(wildcard $(src_dir)/$(1)/*.c) $(wildcard $(src_dir)/common/*.c) $(wildcard $(src_dir)/common/*.S)
endef

$(foreach bmark,$(bmarks),$(eval $(call compile_template,$(bmark))))

#------------------------------------------------------------
# Build and run benchmarks on riscv simulator

copy_shakti:
	@cp common/syscalls.c.shakti common/syscalls.c
copy_spike:
	@cp common/syscalls.c.spike common/syscalls.c

bmarks_riscv_bin  = $(addsuffix .riscv,  $(bmarks))
bmarks_riscv_dump = $(addsuffix .riscv.dump, $(bmarks))
bmarks_riscv_out  = $(addsuffix .riscv.out,  $(bmarks))
bmarks_riscv_hex =  $(addsuffix .riscv.hex, $(bmarks))

$(bmarks_riscv_dump): %.riscv.dump: %.riscv
	@$(RISCV_OBJDUMP) $< > $@

$(bmarks_riscv_out): %.riscv.out: %.riscv
	@$(RISCV_SIM) $< > $@

$(bmarks_riscv_hex): %.riscv.hex: %.riscv
	@$(RISCV_HEX) $< $(base_address) > $@

riscv: $(bmarks_riscv_dump)  $(bmarks_riscv_hex)
run: $(bmarks_riscv_out)

junk += $(bmarks_riscv_bin) $(bmarks_riscv_dump) $(bmarks_riscv_hex) $(bmarks_riscv_out)

#------------------------------------------------------------
# Default
define simulate_benchmarks

endef

all: shakti
shakti: copy_shakti riscv install
spike: copy_spike riscv install
regress: shakti
	@echo "Starting regression"
	@mkdir -p $(simdir);
	@cut -c1-8 ${SHAKTI_HOME}/verification/dts/boot.hex > $(simdir)/boot.MSB;
	@cut -c9-16 ${SHAKTI_HOME}/verification/dts/boot.hex > $(simdir)/boot.LSB;
	@cd $(simdir); ln -f -s ${SHAKTI_HOME}/bin/out* .;
	@touch $(simdir)/regression.log
	@$(foreach bench, $(bmarks),\
		echo '===================== Running BENCHMARK: $(bench) ==================='>> $(simdir)/regression.log;	\
		cut -c1-8 outputfiles/$(bench).riscv.hex > $(simdir)/code.mem.MSB; \
		cut -c9-16 outputfiles/$(bench).riscv.hex > $(simdir)/code.mem.LSB; \
		cd $(simdir); timeout 1m ./out > log; \
		cat app_log >> regression.log; \
		mkdir -p $(bench);\
		mv app_log $(bench);\
		cd ..;\
	)
	@echo 'Regression complete'
#------------------------------------------------------------
# Install

date_suffix = $(shell date +%Y-%m-%d_%H-%M)
install_dir = $(instbasedir)/$(instname)-$(date_suffix)
latest_install = $(shell ls -1 -d $(instbasedir)/$(instname)* | tail -n 1)

install:
	@mkdir -p $(instbasedir)
	@mv $(bmarks_riscv_bin) $(bmarks_riscv_dump) $(bmarks_riscv_hex) $(instbasedir)
	@echo 'Compilation of Benchmarks done'

install-link:
	rm -rf $(instbasedir)/$(instname)
	ln -s $(latest_install) $(instbasedir)/$(instname)

#------------------------------------------------------------
# Clean up

clean:
	rm -rf $(objs) $(junk) $(instbasedir) $(simdir)
