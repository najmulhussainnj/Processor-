# SHAKTI C-CLASS #

This is the c-class core of the SHAKTI Processor systems. This repo contains all the relevant
hardware and software related to the c-class core. The core and the peripherals are developed using If you wish to contribute or make some fixes please make a pull-request. You can reach out to team by mailing us at: shakti.iitm@gmail.com

### Current FEATURES of C-CLASS ###

* 6-stage 64/32-bit pipelined core.
* Supports ISA=RV64IMAFD based on riscv-spec-2.2 and privilege-spec-1.10.
* Bimodal branch predictor with a Return-Address-Stack support.
* Parameterized blocking Instruction and Data cache.
* Serialized Single and Double Precision Floating Point Units.
* Early out multiplier and a restoring divider.
* Supports sv39.
* JTAG Debugger based on debug-spec-0.13
* Boots riscv-linux kernel.
* Performance = 1.67DMIPS/MHz and 2.2 Coremarks/MHz



### Directory Structure ###

* src - holds the bluespec source code of the core, uncore, peripherals, etc.
* verification - holds the directed and random tests targetting the c-class core.


### Pre-requisites ###

A BSV compiler (version 2017 or above) is necessary to compile the core. More information
on Bluespec can be found [here](www.bluespec.com). 

### Configuring the Soc ###

The soc_config.inc file is used to configure the specs of the core and the Soc that you would like to generate. Following are the current options. The valid values for most of these are "enable" or "disable" unless specified otherwise:

* ISA: This controls the ISA support you want the core to provide. Unsuported instructions will be treated as illegal. Valid values are : RV64IMAFD, RV64IMAF, RV64IMA, RV64IM. RV32* versions will be available soon.
* MMU: This controls whether virtualization is supported or not by the core. Currently it is mandatory to keep this option active i.e. "enable". Work to make this optional is under progress.
* BPU: This controls whether core has a branch predictor or not. Currently it is mandatory to keep the predictor "enabled".
* MUL: This controls the type of integer multiplier that will be used to implement the "M" extension of the ISA. Valid values are :
  * sequential: This will imlpement an 8-cycle sequential multiplier with early-out mechanism.
  * parallel  : This will implement a single-cycle combo multiplier.
* PERF: This controls whether performance counters are available in the core or not.
* VERBOSE: This controls whether the $display statements need to be printed or not.
* PREFETCH: This controls whether instruction prefetch is enabled or not. 
* DEBUG: This controls whether a JTAG based Debugger is present or not.
* OPENOCD: This indicates whether OPENOCD is being used to connect to debugger from the testbench.
* QSPI0/1: Enabling this instantiates the home-grown qspi controller.
* SDRAM: Enabling this instantiates the open-source SDRAM controller. Disabling this instantiates a regular BRAM memory.
* UART0: Enabling this instantiates the UART16550 ip with full support of RTS, CTS, etc.
* UART1: Enabling this instantiates a small uart with just rx and tx capabilities.
* PLIC: Enabling this instantiates a peripheral logic interrupt controller.
* BOOTROM: Enabling this instantiates a read-only BRAM memory fo 64KB size.
* I2C0/1: Enabling these instantiates the home-grown I2C controller.
* DMA: Enabling this instantiates the home-grown DMA controller.
* TCM: Enabling this instantiates a 128KB BRAM based tightly-coupled memory.
* CLINT: Enables the core-level interrupt.
* SYNTH: This option controls where the core is being generated for simulation or synthesis. Valid values are:
  * SIM: This will generate a core which will have some simulate-only features like file-io, etc.
  * FPGA: This will generate a core which will ignore the simulate-only features.
* FLASHMODEL: This will instantiate either a cypress or a micron based FLASH BFM in the test-bench to be connected to the qspi. Valid values are "cypress" and "micron"

### Compiling the Core/SoC ###

The Makefile in the root-folder is to be used to compile the core/SoC. For the makefile to work you need to have soc_config.inc and an empty file called "old_vars" in the root-folder. Following are the make targets that a user can use:

* compile_bluesim: This will compile the code in the bsim environment and generate bsv intermediate files in the bsv_build folder.
* link_bluesim: This will link the bsim compiled code and generate a binary in the bin folder.
* generate_verilog: This will compile the code in the verilog environment and generate the verilog files in the verilog folder.
* link_ncverilog: This will compile the generated verilog files and generate an executable in the bin folder using Cadence ncvlog and ncelab tools.
* link_msim: This will compile the generated verilog files and generate an executable in the bin folder using Modelsim.
* link_iverilog: This will compile the generated verilog files and generate an executable in the bin folder using iVerilog.
* simulate: This executes the "out" executable created by any of the above link_* commands.
* clean: Will delete the bin and bsv_build folders
* clean_verilog: Will call clean and remove the verilog folder as well.
* restore: Will call clean and clean_verilog  and also perform a clean in the benchmarks folder.
* generate_boot_files: By default the core will start execution from 0x1000 which is mapped to the read-only BootROM. To match the execution with spike this region should hold the dts files which is available in verification/dts/boot.hex . This target will convert the hex file into a format which can be loaded into C-CLASS's bootrom.

### Simulation Requirements ###

While simulating the core using the "out" executable (generated by any of the link_* targets of the makefile) the following files are required to be present in the same folder as the executable:

* boot.LSB and boot.MSB: generated using the makefile target "generate_boot_files".
* code.mem.LSB and code.mem.MSB: For a software code compiled at 0x80000000 to be loaded into the SHAKTI's memory it has to be first converted to a hex file using the elf2hex command. A elf2hex command is as follows:

<p> elf2hex 8 32768 software.elf 2147483648 > code.hex</p>

This code.hex should now be further split into code.mem.LSB and code.mem.MSB as follows:
<p> cut -c1-8 code.hex> code.mem.MSB </p>
<p> cut -c9-16 code.hex > code.mem.LSB </p>

More details to follow.. :)







































