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

### Setting up environment variables ###

Follow the below steps before using the code-line:
    
              $ git clone https://bitbucket.org/casl/c-class.git
              $ cd c-class
              $ export SHAKTI_HOME=$(pwd)
              
It is important to set the SHAKTI_HOME variable for regression tests to work.

### Directory Structure ###

* src - holds the bluespec source code of the core, uncore, peripherals and other related files
* verification - holds the directed and random tests for the c-class core.


### Pre-requisites ###

A BSV compiler (version 2017 or above) is necessary to compile the code. More information
on Bluespec can be found [here](www.bluespec.com). 

### Configuring the Soc ###

The soc_config.inc file is used to configure the specs of the core and the Soc that you would like to generate. Following are the current options. The valid values for most of these are "enable" or "disable" unless specified otherwise:

| Feature  | Valid Values | Description |
|----------|--------------|-------------|
| __ISA__  | RV64IMAFD, RV64IMAF,</br> RV64IMA, RV64IM | The ISA support you want the core to provide. Unsuported instructions will be treated as illegal |
| __MMU__  | enable,disable      | virtualization is supported or not by the core. __Mandatory ENABLED__ |
| __BPU__  |  enable,disable    |  Branch predictory is present or not __Mandatory ENABLED__|
| __MUL__  | sequential, parallel| sequential: will implement an 8-cycle early-out multiplier. parallel: will implement single-cycle multiplier.|
|__PERF__  | enable,disable |performance counters are available in the core or not.|
|__VERBOSE__|enable,disable|This controls whether the $display statements are printed|
|__PREFETCH__|enable,disable|instruction prefetch is present|
|__DEBUG__|enable,disable| A JTAG based Debugger is present or not|
|__OPENOCD__|enable,disable|OPENOCD is being used to connect to debugger|
|__QSPI0/1__|enable,disable|Enable/disable the home-grown QSPI0|.
|__SDRAM__| enable,disable|Instantiates the open-source SDRAM controller. Disabling this instantiates a regular BRAM memory.|
|__UART0__|enable,disable|Instantiates the UART16550 ip with full support of RTS, CTS, etc.|
|__UART1__|enable,disable|Instantiates a small uart with just rx and tx capabilities.|
|__PLIC__|enable,disable|Instantiates a peripheral logic interrupt controller.|
|__BOOTROM__|enable,disable|Instantiates a read-only BRAM memory of 64KB size.|
|__I2C0/1__|enable,disable|Instantiates the home-grown I2C controller.|
|__DMA__|enable,disable|Instantiates the home-grown DMA controller.|
|__TCM__|enable,disable|Instantiates a 128KB BRAM based tightly-coupled memory.|
|__CLINT__|enable,disable|Instantiates a core-level interrupt.|
|__SYNTH__|SIM, FPGA| controls whether the core is being generated for simulation or synthesis. </br>* SIM: This will generate a core which will have some simulate-only features like file-io, etc.</br>* FPGA: This will generate a core which will ignore the simulate-only features.
|__FLASHMODEL__|cypress,micron| instantiate either a cypress or a micron based FLASH BFM in the test-bench to be connected to the qspi.|

### Compiling the Core/SoC ###

The Makefile in the root-folder is to be used to compile the core/SoC. For the makefile to work you need to have soc_config.inc and an empty file called "old_vars" in the root-folder. Following are the make targets that a user can use:

| Target | Description |
|--------|-------------|
|__compile_bluesim__| This will compile the code in the bsim environment and generate bsv intermediate files in the bsv_build folder.|
|__link_bluesim__|This will link the bsim compiled code and generate a binary in the bin folder.|
|__generate_verilog__|This will compile the code in the verilog environment and generate the verilog files in the verilog folder.|
|__link_ncverilog__| This will compile the generated verilog files and generate an executable in the bin folder using Cadence ncvlog and ncelab tools.|
|__link_msim__|This will compile the generated verilog files and generate an executable in the bin folder using Modelsim.|
|__link_iverilog__|This will compile the generated verilog files and generate an executable in the bin folder using iVerilog.|
|__link_verilator__| This will link the generated verilog files using verilator|
|__simulate__|This executes the "out" executable created by any of the above link_* commands.|
|__clean__| Will delete the bin and bsv_build folders.|
|__clean_verilog__|Will call clean and remove the verilog folder as well.|
|__restore__|Will call clean and clean_verilog  and also perform a clean in the benchmarks folder.|
|__generate_boot_files__|By default the core will start execution from 0x1000 which is mapped to the read-only BootROM. To match the execution with spike this region should hold the dts files which is available in verification/dts/boot.hex . This target will convert the hex file into a format which can be loaded into C-CLASS's bootrom.|

### Simulation Requirements ###

While simulating the core using the "out" executable (generated by any of the link_* targets of the makefile) the following files are required to be present in the same folder as the executable:

* boot.LSB and boot.MSB: generated using the makefile target "generate_boot_files".
* code.mem.LSB and code.mem.MSB: For a software code compiled at 0x80000000 to be loaded into the SHAKTI's memory it has to be first converted to a hex file using the elf2hex command. A elf2hex command is as follows:

            $ elf2hex 8 32768 software.elf 2147483648 > code.hex

This code.hex should now be further split into code.mem.LSB and code.mem.MSB as follows:

            $ cut -c1-8 code.hex> code.mem.MSB 
            $ cut -c9-16 code.hex > code.mem.LSB 
   
To generate VCD go to the bin and execute the following:

* in bsim environment: ./out -V
* in verilog environment: ./out +bscvcd

More details to follow.. :)







































