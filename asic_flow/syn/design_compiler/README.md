# **RTL Synthesis using Synopsys Design Compiler**

## Generating verilog files

Verilog files are generated using the ***generate_verilog*** target of the Makefile available in $SHAKTI_HOME. Please refer to the README present in the root-folder $SHAKTI_HOME

## Pre-Synthesis setup
The following files need to be updated before firing a synthesis
| File                     | Updates |
|--------------------------|---------|
|./scripts/environment.tcl |set the top module name and the library variables         |
|./scripts/dont_use.tcl    |list out all the foundary specific cells that should not be used for synthesis         |
|./constraints/block.sdc   |set the clock constraints here.         |

## Fire Synthesis
from the $SHAKTI_HOME/asic_flow/syn folder execute the following command:
        $dc_shell -f ./scripts/dc.tcl | tee dc.log"
        
## Notes and Tips
* This script does not have DFT and MBIST feature inclusion. Will be updated soon.
* Make sure the Block RAM instances are replaced with its corresponding configurattion of SRAM memory library available from Foundary. This step will have to be manually done for now. Automation for this step will take some time.
