
source ../scripts/environment.tcl 

if {![file exists ../${_OUTPUTS_PATH}]} {
  file mkdir ../${_OUTPUTS_PATH}
  puts "Creating directory ${_OUTPUTS_PATH}"
}

if {![file exists ../${_REPORTS_PATH}]} {
  file mkdir ../${_REPORTS_PATH}
  puts "Creating directory ${_REPORTS_PATH}"
}

# for formality
set_svf ../${_OUTPUTS_PATH}/$design.svf

analyze -format verilog -recursive -autoread $RTL
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
source ../scripts/appvariables.tcl

#----------------------------
# constraint settings 
#----------------------------
source ../constraints/constraints.sdc 
set_max_area 0
set_dont_retime $design true

#----------------------------
# path group settings 
#----------------------------
source ../scripts/path_groups.tcl
#----------------------------
# sanity check 
#----------------------------
check_design                             > ../${_REPORTS_PATH}/$design.check_design.pre_compile.rpt
report_net_fanout -threshold 50 -nosplit > ../${_REPORTS_PATH}/$design.fanout.pre_compile.rpt
#----------------------------
# misc constraints 
#----------------------------
set verilogout_no_tri  "true"
source ../scripts/fab_specific.tcl

##----------------------------
## compile 
##----------------------------
compile_ultra -incremental -no_autoungroup -no_boundary_optimization  -no_seq_output_inversion 
write -f ddc     -hier -o   ../${_OUTPUTS_PATH}/$design.prechange.ddc
write -f verilog -hier -o   ../${_OUTPUTS_PATH}/$design.prechange.v

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
write -f ddc     -hier -o   ../${_OUTPUTS_PATH}/$design.dc_compile_ultra_1.flat.ddc
write -f verilog -hier -o   ../${_OUTPUTS_PATH}/$design.dc_compile_ultra_1.flat.v
report_reference -nosplit > ../${_REPORTS_PATH}/$design.dc_compile_ultra_1.ref.flat.rpt
write_sdc ../${_OUTPUTS_PATH}/$design.dc_compile_ultra_1.sdc
write_sdf ../${_OUTPUTS_PATH}/$design.dc_compile_ultra_1.sdf
write_test_model -format ctl -output ../${_OUTPUTS_PATH}/$design.dc_compile_ultra_1.flat.ctl

#---------------------------
# report generation 
#---------------------------
report_qor > ../${_REPORTS_PATH}/$design.dc_compile_ultra_1.qor
report_timing -nosplit -max_paths 5000 -input -nets -cap -tran -nosplit -sig 3                 > ../${_REPORTS_PATH}/$design.dc_compile_ultra_1.setup.all.tim.rpt.by_group
report_timing -nosplit -max_paths 5000 -input -nets -cap -tran -nosplit -sig 3 -sort_by slack  > ../${_REPORTS_PATH}/$design.dc_compile_ultra_1.setup.all.tim.rpt.by_slack
report_timing -nosplit -max_paths 2000 -input -nets -cap -tran -nosplit -sig 3 -group f2f      > ../${_REPORTS_PATH}/$design.dc_compile_ultra_1.setup.f2f.tim.rpt
report_timing -nosplit -max_paths 2000 -input -nets -cap -tran -nosplit -sig 3 -group i2f      > ../${_REPORTS_PATH}/$design.dc_compile_ultra_1.setup.i2f.tim.rpt
report_timing -nosplit -max_paths 2000 -input -nets -cap -tran -nosplit -sig 3 -group f2o      > ../${_REPORTS_PATH}/$design.dc_compile_ultra_1.setup.f2o.tim.rpt
report_timing -nosplit -max_paths 2000 -input -nets -cap -tran -nosplit -sig 3 -group i2o      > ../${_REPORTS_PATH}/$design.dc_compile_ultra_1.setup.i2o.tim.rpt


report_area -nosplit -hier										 > ../${_REPORTS_PATH}/$design.dc_compile_ultra_1.area.rpt
report_clock_gating -verbose -gated  -gating_elements  > ../${_REPORTS_PATH}/$design.dc_compile_1.clock_gate.rpt
check_design													    > ../${_REPORTS_PATH}/$design.check_design.dc_compile_1.rpt
report_net_fanout -threshold 50 -nosplit					 > ../${_REPORTS_PATH}/$design.fanout.dc_compile_1.rpt
check_timing													    > ../${_REPORTS_PATH}/$design.check_timing.dc_compile_1.rpt
report_resources -nosplit -hierarchy					    > ../${_REPORTS_PATH}/$design.report_resources.compile_1
report_power														 > ../${_REPORTS_PATH}/$design.power.rpt

create_block_abstraction
write -f ddc -hier -o ../${_OUTPUTS_PATH}/$design.abstraction.ddc
exit
