
source ./scripts/environment.tcl 

# for formality
set_svf ./output/$design.svf

analyze -format verilog -recursive -autoread ../verilog/
elaborate $design

#----------------------------
# current_design 
#----------------------------
current_design $design
link

## uniquify is done automatically by compile_ultra
#----------------------------
# dc setting 
#----------------------------
source ./scripts/appvariables.tcl

#----------------------------
# constraint settings 
#----------------------------
source ./constraints/block.sdc 
set_max_area 0
set_dont_retime $design true

#----------------------------
# path group settings 
#----------------------------
source ./scripts/path_groups.tcl
#----------------------------
# sanity check 
#----------------------------
check_design                             > ./reports/$design.check_design.pre_compile.rpt
report_net_fanout -threshold 50 -nosplit > ./reports/$design.fanout.pre_compile.rpt
#----------------------------
# misc constraints 
#----------------------------
set verilogout_no_tri  "true"
source ./scripts/dont_use.tcl

##----------------------------
## compile 
##----------------------------
compile_ultra -incremental -no_autoungroup -no_boundary_optimization  -no_seq_output_inversion 
write -f ddc     -hier -o   ./output/$design_prechange.ddc
write -f verilog -hier -o   ./output/$design_prechange.v

#----------------------------
# checkpoint options 
#----------------------------
define_name_rules verilog -allowed "a-z A-Z 0-9 _ /"
report_name_rules verilog 
# variable setting for verilog out
change_names -hier -rules verilog

#----------------------------
# checkpoint
#----------------------------
write -f ddc     -hier -o   ./output/$design.dc_compile_ultra_1.flat.ddc
write -f verilog -hier -o   ./output/$design.dc_compile_ultra_1.flat.v
report_reference -nosplit > ./reports/$design.dc_compile_ultra_1.ref.flat.rpt
write_sdc ./output/$design.dc_compile_ultra_1.sdc
write_sdf ./output/$design.dc_compile_ultra_1.sdf
write_test_model -format ctl -output ./output/$design.dc_compile_ultra_1.flat.ctl

#---------------------------
# report generation 
#---------------------------
report_qor > ./reports/$design.dc_compile_ultra_1.qor
report_timing -nosplit -max_paths 5000 -input -nets -cap -tran -nosplit -sig 3                 > ./reports/$design.dc_compile_ultra_1.setup.all.tim.rpt.by_group
report_timing -nosplit -max_paths 5000 -input -nets -cap -tran -nosplit -sig 3 -sort_by slack  > ./reports/$design.dc_compile_ultra_1.setup.all.tim.rpt.by_slack
report_timing -nosplit -max_paths 2000 -input -nets -cap -tran -nosplit -sig 3 -group f2f      > ./reports/$design.dc_compile_ultra_1.setup.f2f.tim.rpt
report_timing -nosplit -max_paths 2000 -input -nets -cap -tran -nosplit -sig 3 -group i2f      > ./reports/$design.dc_compile_ultra_1.setup.i2f.tim.rpt
report_timing -nosplit -max_paths 2000 -input -nets -cap -tran -nosplit -sig 3 -group f2o      > ./reports/$design.dc_compile_ultra_1.setup.f2o.tim.rpt
report_timing -nosplit -max_paths 2000 -input -nets -cap -tran -nosplit -sig 3 -group i2o      > ./reports/$design.dc_compile_ultra_1.setup.i2o.tim.rpt


report_area -nosplit -hier										 > ./reports/$design.dc_compile_ultra_1.area.rpt
report_clock_gating -verbose -gated  -gating_elements  > ./reports/$design.dc_compile_1.clock_gate.rpt
check_design													    > ./reports/$design.check_design.dc_compile_1.rpt
report_net_fanout -threshold 50 -nosplit					 > ./reports/$design.fanout.dc_compile_1.rpt
check_timing													    > ./reports/$design.check_timing.dc_compile_1.rpt
report_resources -nosplit -hierarchy					    > ./reports/$design.report_resources.compile_1
report_power														 > ./reports/$design.power.rpt

create_block_abstraction
write -f ddc -hier -o ./output/$design.abstraction.ddc
exit
