## Synthesis Using Cadence RTL Compiler and Synopsys Design Compiler

### Generating verilog files
Verilog files are generated using the ***generate_verilog*** target of the Makefile available in $SHAKTI\_HOME. Refer to the README in the root-folder $SHAKTI\_HOME

### Pre-Synthesis setup

#### Synopsys Design Compiler
|File ($SHAKTI_HOME/asic_flow/syn/design_compiler/                           | Update   |
|--------------------------------| -------- |
|./scripts/environment.tcl       |set the top module name, library path and libraries(.db)|
|./scripts/fab_specific.tcl          |list the foundary specific cells that should not be used for synthesis|
|./constraints/constraints.sdc         |Specify clock constraints here|

#### Cadence RTL Compiler
|File ($SHAKTI_HOME/asic_flow/syn/rtl_compiler/)                           | Update   |
|--------------------------------| -------- |
|./scripts/environment.tcl       |set the top module name, library path and libraries(.lib)|
|./scripts/fab_specific.tcl          |list the foundary specific cells that should not be used for synthesis|
|./constraints/constraints.sdc         |Specify clock constraints here|

### Fire Synthesis
From $SHAKTI_HOME/asic_flow/syn/ folder invoke using ***dc*** and ***rc*** target of the Makefile for synopsys design compiler and Cadence RTL compiler respectively.

Reports, Outputs and logs will be availble at corresponding design_compiler and rtl_compiler folder

### Note

* This script does not have DFT and MBIST feature inclusion. Will be updated soon.
* Make sure the Block RAM instances are replaced with its corresponding configurattion of SRAM memory library available from Foundary. This step will have to be manually done for now. Automation for this step will take some time.
~                                                                                                                                                                                                                   
~                                                                                                                                                                                                                   
~                         
