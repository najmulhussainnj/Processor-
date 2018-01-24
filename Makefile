### Makefile for the eclass project
### Generated by Bluespec Workstation on Thu Nov 12 19:54:06 IST 2015

include ./old_vars
include soc_config.inc

TOP_MODULE:=mkTbSoc
TOP_FILE:=TbSoc.bsv
TOP_DIR:=./src/testbench/
WORKING_DIR := $(shell pwd)

ifneq (,$(findstring RV64,$(ISA)))
  define_macros += -D RV64=True
  XLEN=64
endif
ifneq (,$(findstring RV32,$(ISA)))
  XLEN=32
endif
ifneq (,$(findstring M,$(ISA)))
  define_macros += -D muldiv=True
  ifeq ($(MUL),sequential)
     define_macros	+= -D sequential=True
  endif
  ifeq ($(MUL),parallel)
     define_macros	+= -D parallel=True
  endif
endif
ifneq (,$(findstring A,$(ISA)))
  define_macros += -D atomic=True
endif
ifneq (,$(findstring F,$(ISA)))
  define_macros += -D spfpu=True
  FLOAT=--float
endif
ifneq (,$(findstring D,$(ISA)))
  define_macros += -D dpfpu=True
  FLOAT=--float
endif
ifeq ($(BPU),enable)
  define_macros += -D bpu=True
endif
ifeq ($(VERBOSE),enable)
  define_macros += -D verbose=True
endif
ifeq ($(MMU),enable)
  define_macros += -D MMU=True
endif
ifeq ($(PERF),enable)
  define_macros	+= -D perf=True
endif
ifeq ($(PREFETCH),enable)
  define_macros	+= -D prefetch=True
endif
ifeq ($(JTAG),enable)
  define_macros	+= -D JTAG=True
endif
ifeq ($(DEBUG),enable)
  define_macros += -D Debug=True
endif
ifeq ($(OPENOCD),enable)
  define_macros += -D Openocd=True
endif
ifeq ($(QSPI0),enable)
  define_macros += -D QSPI0=True
endif
ifeq ($(QSPI1),enable)
  define_macros += -D QSPI1=True
endif
ifeq ($(SDRAM),enable)
  define_macros += -D SDRAM=True
endif
ifeq ($(UART0),enable)
  define_macros += -D UART0=True
endif
ifeq ($(UART1),enable)
  define_macros += -D UART1=True
endif
ifeq ($(BOOTROM),enable)
  define_macros += -D BOOTROM=True
endif
ifeq ($(PLIC),enable)
  define_macros += -D PLIC=True
endif
ifeq ($(I2C0),enable)
  define_macros += -D I2C0=True
endif
ifeq ($(I2C1),enable)
  define_macros += -D I2C1=True
endif
ifeq ($(HYPER),enable)
  define_macros += -D HYPER=True
endif
ifeq ($(DMA),enable)
  define_macros += -D DMA=True
endif
ifeq ($(AXIEXP),enable)
  define_macros += -D AXIEXP=True
endif
ifeq ($(TCM),enable)
  define_macros += -D TCMemory=True
endif
ifeq ($(CLINT),enable)
  define_macros += -D CLINT=True
endif
ifeq ($(SYNTH),SIM)
  define_macros += -D simulate=True
endif
ifeq ($(SYNTH),ASIC)
  define_macros += -D asic=True
endif
ifeq ($(SYNTH),FPGA)
  define_macros += -D fpga=True
endif

PERIPHERALS:=src/peripherals/bootrom:src/peripherals/clint:src/peripherals/plic:./src/peripherals/uart/:./src/peripherals/tcm/:./src/peripherals/jtagdtm
UNCORE:=./src/uncore:./src/uncore/axi4:./src/uncore/debug
CORE:=./src/core/fpu:./src/core/
TESTBENCH:=./src/testbench/
LIB:=./src/lib/

BSVINCDIR:= .:%/Prelude:%/Libraries:%/Libraries/BlueNoC:$(CORE):$(UNCORE):$(PERIPHERALS):$(TESTBENCH):$(LIB)
default: compile_bluesim link_bluesim generate_boot_files

set_variables:
ifneq (,$(findstring RV64,$(ISA)))
	@echo "XLEN=64" > benchmarks/Makefile.inc
	@sed -i '/bitwidth/c\bitwidth=64' verification/AAPG/config.py
	@echo "XLEN=64" > verification/csmith_run/Makefile.inc
	@echo "integer size = 8" > verification/csmith_run/platform.info
	@echo "pointer size = 8" >> verification/csmith_run/platform.info
endif
ifneq (,$(findstring RV32,$(ISA)))
	@echo "XLEN=32" > benchmarks/Makefile.inc
	@sed -i '/bitwidth/c\bitwidth=32' verification/AAPG/config.py
	@echo "XLEN=32" > verification/csmith_run/Makefile.inc
	@echo "integer size = 4" > verification/csmith_run/platform.info
	@echo "pointer size = 4" >> verification/csmith_run/platform.info
endif


generate_boot_files:
	@mkdir -p bin
	@head -n 4096 ./config_string64.hex > $(BOOT_DIR)/boot.l
	@tail -n 4097 ./config_string64.hex > $(BOOT_DIR)/boot.h
	@cut -c1-4 $(BOOT_DIR)/boot.l > $(BOOT_DIR)/boot.3l
	@cut -c5-8 $(BOOT_DIR)/boot.l > $(BOOT_DIR)/boot.2l
	@cut -c9-12 $(BOOT_DIR)/boot.l > $(BOOT_DIR)/boot.1l
	@cut -c13-16 $(BOOT_DIR)/boot.l > $(BOOT_DIR)/boot.0l
	@cut -c1-4 $(BOOT_DIR)/boot.h > $(BOOT_DIR)/boot.3h
	@cut -c5-8 $(BOOT_DIR)/boot.h > $(BOOT_DIR)/boot.2h
	@cut -c9-12 $(BOOT_DIR)/boot.h > $(BOOT_DIR)/boot.1h
	@cut -c13-16 $(BOOT_DIR)/boot.h > $(BOOT_DIR)/boot.0h

check-blue:
	@if test -z "$$BLUESPECDIR"; then echo "BLUESPECDIR variable not set"; exit 1; fi; 

check-py:
	@if ! [ -a /usr/bin/python3 ] ; then echo "Python3 is required in /usr/bin to run AAPG" ; exit 1; fi;

###### Setting the variables for bluespec compile #$############################
BSVCOMPILEOPTS:= -check-assert -suppress-warnings G0020:T0054 -keep-fires -opt-undetermined-vals -remove-false-rules -remove-empty-rules
BSVLINKOPTS:=-parallel-sim-link 8 -keep-fires
VERILOGDIR:=./verilog/
BSVBUILDDIR:=./bsv_build/
BSVLOG:= bsv_compile.log
BSVOUTDIR:=./bin
################################################################################

########## BSIM COMLILE, LINK AND SIMULATE TARGETS #################################
.PHONY: check-restore
check-restore:
	@if [ "$(define_macros)" != "$(old_define_macros)" ];	then	make clean_bsim ;	fi;

.PHONY:  compile_bluesim
compile_bluesim: check-restore check-blue
	@echo "Compiling $(TOP_MODULE) in Bluesim..."
	@mkdir -p $(BSVBUILDDIR) 
	@echo "old_define_macros = $(define_macros)" > old_vars
	bsc -u -sim -simdir $(BSVBUILDDIR) -bdir $(BSVBUILDDIR) -info-dir $(BSVBUILDDIR) $(define_macros) $(BSVCOMPILEOPTS) -p $(BSVINCDIR) -g $(TOP_MODULE) $(TOP_DIR)/$(TOP_FILE) | tee $(BSVLOG)
	@echo "Compilation finished"

.PHONY: link_bluesim
link_bluesim:check-blue
	@echo "Linking $(TOP_MODULE) in Bluesim..."
	@mkdir -p $(BSVOUTDIR)
	bsc -e $(TOP_MODULE) -sim -o $(BSVOUTDIR)/out -simdir $(BSVOUTDIR) -p $(INCDIR) -bdir $(BSVOUTDIR) $(BSVLINKOPTS)./BSV_src/UNCORE_src/RBB_Shakti.c;
	@echo Linking finished

.PHONY: simulate
simulate:
	@echo Simulation...
	@exec ./$(BSVOUTDIR)/out
	@echo Simulation finished
########################################################################################


.PHONY: generate_verilog 
generate_verilog: check-restore check-blue set_variables generate_boot_files
	@echo Compiling mkTbSoc in Verilog for simulations ...
	@mkdir -p $(BSVBUILDDIR); 
	@mkdir -p $(VERILOGDIR); 
	@echo "old_define_macros = $(define_macros)" > old_vars
	bsc -u -verilog -elab -vdir verilog -bdir BSV_src/build_bsim -info-dir BSV_src/build_bsim $(define_macros) -D verilog=True -keep-fires -suppress-warnings G0020  -opt-undetermined-vals -verilog-filter ${BLUESPECDIR}/bin/basicinout -remove-empty-rules -remove-false-rules -remove-starved-rules -p $(INCDIR) -g $(TOP_MODULE) BSV_src/$(TOP_DIR)/$(TOP_FILE)
	@cp -f ${BLUESPECDIR}/Verilog/SizedFIFO.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog/FIFOL1.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog/Counter.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog/RevertReg.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog/FIFO2.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog/FIFO1.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog/main.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog.Vivado/BRAM2.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog.Vivado/BRAM2BELoad.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog.Vivado/BRAM1BELoad.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog.Vivado/BRAM1Load.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog.Vivado/BRAM1.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog.Vivado/BRAM1BE.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog.Vivado/BRAM2BE.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog.Vivado/RegFile.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog/SyncReset0.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog/SyncFIFO1.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog/SyncFIFO.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog/SyncHandshake.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog/SyncRegister.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog/ClockDiv.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog/ClockInverter.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog/TriState.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog/MakeClock.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog/MakeReset0.v ./verilog/
	@cp -f ${BLUESPECDIR}/Verilog/SyncPulse.v ./verilog/
	@cp -fr ../peripherals/SDRAM_Controller/synth_files/*.v ./verilog/
	@cp -fr ../peripherals/qspi/sim_files/* ./verilog/
	@sed -i "s/39'h0000001000/reset_vector/g" ./verilog/mkfetch.v
	@sed -i "s/(rg_tdo)/(rg_tdo\$$D_IN)/g" ./verilog/mkTbSoc.v
	@cp ../peripherals/I2C/BSV_src/M24AA1025.v ./verilog/
	@cp ../peripherals/qspi/cypress/CypressFlashWrapper.v ./verilog/
	@cp ../peripherals/qspi/cypress/s25fs512s* ./verilog/
#	@cp Ip_fix/bscan/*.v ./verilog/
#	@cp Ip_fix/gpio*.v ./verilog/
	@echo Compilation finished

.PHONY: link_ncverilog
link_ncverilog: set_variables 
	@echo "Linking mkTbSoc using ncverilog..."
	@rm -rf work include bin/work
	@mkdir -p bin 
	@mkdir work
	@ln -f -s ./verilog/cds.lib cds.lib
	@ln -f -s ./verilog/hdl.var hdl.var
	@ln -f -s ./verilog/*.vmf ./bin/
	@ln -s ./verilog/include include
	@ncvlog -sv -cdslib ./cds.lib -hdlvar ./hdl.var -MESSAGES -NOCOPYRIGHT -LINEDEBUG +define+INTC_NO_PWR_PINS+INTC_FUNCTIONAL+INTC_MEM_FAST_SIM +define+TOP=mkTbSoc ./verilog/N25Qxxx.v
	@ncvlog -sv -cdslib ./cds.lib -hdlvar ./hdl.var -MESSAGES -NOCOPYRIGHT -LINEDEBUG +define+INTC_NO_PWR_PINS+INTC_FUNCTIONAL+INTC_MEM_FAST_SIM +define+TOP=mkTbSoc ./verilog/main.v -y ./verilog/ 
	@ncelab  -cdslib ./cds.lib -hdlvar ./hdl.var -mess -NOWARN CUDEFB work.main -access +r -timescale 1ns/1ps
	@echo 'ncsim -cdslib ./cds.lib -hdlvar ./hdl.var work.main #> /dev/null' > bin/out
	@cp cds.lib hdl.var bin/
	@mv work bin/
	@chmod +x bin/out
	@echo Linking finished

.PHONY: link_msim
link_msim: set_variables 
	@echo "Linking mkTbSoc using modelsim..."
	@rm -rf work* bin/*
	@mkdir -p bin 
	vlib work
	vlog -work work +libext+.v+.vqm -y ./verilog +define+TOP=mkTbSoc ./verilog/main.v ./verilog/mkTbSoc.v  > compile_log
	mv compile_log ./bin/
	mv work ./bin/
	echo 'vsim -quiet -novopt -lib work -do "run -all; quit" -c main' > bin/out
	@chmod +x bin/out
	@echo Linking finished

.PHONY: link_iverilog
link_iverilog: set_variables 
	@echo "Linking mkTbSoc using iverilog..."
	@mkdir -p bin 
	@iverilog -v -o bin/out -Wall -y ./verilog/ -DTOP=mkTbSoc ./verilog/main.v ./verilog/mkTbSoc.v
	@echo Linking finished

.PHONY: clean_bsim
clean_bsim:
	rm -rf BSV_src/build_bsim 
	rm -rf bin/out
	rm -rf bin/out.so

clean_verilog: clean_bsim 
	rm -f verilog/*.v
	rm -rf verilog_fpga/*.v


restore: clean_verilog
	echo 'old_define_macros=""' > old_vars
	cd verification/csmith_run; make clean
	cd verification/riscv-torture; make clean
	cd verification/riscv-tests; make simulate_clean
	cd verification/AAPG; ./make.py clean
	cd benchmarks; make clean
	rm -f include cds.lib hdl.var *.log
	rm -rf bin/*

