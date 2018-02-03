# SHAKTI C-CLASS #

This is the C-class core of the SHAKTI Processor family. This repo contains all the relevant
hardware and software related to the c-class core. The core and the peripherals are developed using Bluespec.
If you wish to contribute or help fix issues, please make a pull-request. You can reach  us at: shakti.iitm@gmail.com

### Current FEATURES of C-CLASS ###

* 6-stage 64/32-bit pipelined core.
* Supports ISA=RV64IMAFD based on riscv-spec-2.2 and privilege-spec-1.10.
* Bimodal branch predictor with a Return-Address-Stack support.
* Parameterized blocking Instruction and Data cache.
* Serialized Single and Double Precision Floating Point Units.
* Early out multiplier and a restoring divider.
* Supervisor mode - sv39.
* JTAG Debugger based on debug-spec-0.13
* Boots riscv-linux kernel.
* Performance = 1.67DMIPS/MHz and 2.2 Coremarks/MHz



### Directory Structure ###

* src - holds the bluespec source code of the core, uncore, peripherals and other related files
* verification - holds the directed and random tests for the c-class core.


### Pre-requisites ###

A BSV compiler (version 2017 or above) is necessary to compile the core. More information
on Bluespec can be found [here](www.bluespec.com). 

### Configuring the Soc ###

The soc_config.inc file is used to configure the specs of the core and the Soc that you would like to generate. Following are the current options. The valid values for most of these are "enable" or "disable" unless specified otherwise:

* __ISA__: This controls the ISA support you want the core to provide. Unsuported instructions will be treated as illegal. Valid values are : RV64IMAFD, RV64IMAF, RV64IMA, RV64IM. RV32* versions will be available soon.
* __MMU__: This controls whether virtualization is supported or not by the core. Currently it is mandatory to keep this option active i.e. "enable". Work to make this optional is under progress.
* __BPU__: This controls whether core has a branch predictor or not. Currently it is mandatory to keep the predictor "enabled".
* __MUL__: This controls the type of integer multiplier that will be used to implement the "M" extension of the ISA. Valid values are :
     * sequential: This will imlpement an 8-cycle sequential multiplier with early-out mechanism.
     * parallel  : This will implement a single-cycle combo multiplier.
* __PERF__: This controls whether performance counters are available in the core or not.
* __VERBOSE__: This controls whether the $display statements need to be printed or not.
* __PREFETCH__: This controls whether instruction prefetch is enabled or not. 
* __DEBUG__: This controls whether a JTAG based Debugger is present or not.
* __OPENOCD__: This indicates whether OPENOCD is being used to connect to debugger from the testbench.
* __QSPI0/1__: Enabling this instantiates the home-grown qspi controller.
* __SDRAM__: Enabling this instantiates the open-source SDRAM controller. Disabling this instantiates a regular BRAM memory.
* __UART0__: Enabling this instantiates the UART16550 ip with full support of RTS, CTS, etc.
* __UART1__: Enabling this instantiates a small uart with just rx and tx capabilities.
* __PLIC__: Enabling this instantiates a peripheral logic interrupt controller.
* __BOOTROM__: Enabling this instantiates a read-only BRAM memory fo 64KB size.
* __I2C0/1__: Enabling these instantiates the home-grown I2C controller.
* __DMA__: Enabling this instantiates the home-grown DMA controller.
* __TCM__: Enabling this instantiates a 128KB BRAM based tightly-coupled memory.
* __CLINT__: Enables the core-level interrupt.
* __SYNTH__: This option controls where the core is being generated for simulation or synthesis. Valid values are:
     * SIM: This will generate a core which will have some simulate-only features like file-io, etc.
     * FPGA: This will generate a core which will ignore the simulate-only features.
* __FLASHMODEL__: This will instantiate either a cypress or a micron based FLASH BFM in the test-bench to be connected to the qspi. Valid values are "cypress" and "micron"

### Compiling the Core/SoC ###

The Makefile in the root-folder is to be used to compile the core/SoC. For the makefile to work you need to have soc_config.inc and an empty file called "old_vars" in the root-folder. Following are the make targets that a user can use:

* __compile_bluesim__: This will compile the code in the bsim environment and generate bsv intermediate files in the bsv_build folder.
* __link_bluesim__: This will link the bsim compiled code and generate a binary in the bin folder.
* __generate_verilog__: This will compile the code in the verilog environment and generate the verilog files in the verilog folder.
* __link_ncverilog__: This will compile the generated verilog files and generate an executable in the bin folder using Cadence ncvlog and ncelab tools.
* __link_msim__: This will compile the generated verilog files and generate an executable in the bin folder using Modelsim.
* __link_iverilog__: This will compile the generated verilog files and generate an executable in the bin folder using iVerilog.
* __simulate__: This executes the "out" executable created by any of the above link_* commands.
* __clean__: Will delete the bin and bsv_build folders
* __clean_verilog__: Will call clean and remove the verilog folder as well.
* __restore__: Will call clean and clean_verilog  and also perform a clean in the benchmarks folder.
* __generate_boot_files__: By default the core will start execution from 0x1000 which is mapped to the read-only BootROM. To match the execution with spike this region should hold the dts files which is available in verification/dts/boot.hex . This target will convert the hex file into a format which can be loaded into C-CLASS's bootrom.

### Simulation Requirements ###

While simulating the core using the "out" executable (generated by any of the link_* targets of the makefile) the following files are required to be present in the same folder as the executable:

* boot.LSB and boot.MSB: generated using the makefile target "generate_boot_files".
* code.mem.LSB and code.mem.MSB: For a software code compiled at 0x80000000 to be loaded into the SHAKTI's memory it has to be first converted to a hex file using the elf2hex command. A elf2hex command is as follows:

elf2hex 8 32768 software.elf 2147483648 > code.hex

This code.hex should now be further split into code.mem.LSB and code.mem.MSB as follows:
   cut -c1-8 code.hex> code.mem.MSB 
   cut -c9-16 code.hex > code.mem.LSB 
   
To generate VCD:
    * in bsim environment: ./out -V
    * in verilog environment: ./out +bscvcd

More details to follow.. :)







































