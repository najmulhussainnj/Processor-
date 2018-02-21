
############################################## Replace design with the top module name ############################################################
 
#-------------------------
# Library setting
#-------------------------
set target_library    [list]
set link_library      "*"

set synthetic_library [list \
#list your synopsys synthetic .db here \
]

set target_library      [concat $target_library \
#list your .db library here \
]

set link_library      [concat $link_library \

#list your .db library here \
]

set link_library      [concat $link_library $target_library $synthetic_library ]

#list your .db library here \



# for formality
set_svf ./output/design.svf


analyze -format verilog -recursive -autoread ../verilog/
elaborate design

#----------------------------
# current_design 
#----------------------------
current_design design

link


## Add for MBIST COnstraints
#tessent_create_functional_clocks
#set_app_var timing_enable_multiple_clocks_per_reg true
#tessent_constrain_design_non_modal
#set_app_var compile_enable_constant_propagation_with_no_boundary_opt false
#set preserve_instances [tessent_get_preserve_instances icl_extract]
#set_boundary_optimization $preserve_instances false
#set_ungroup $preserve_instances false
#set_app_var compile_seqmap_propagate_high_effort false
#set_app_var compile_delete_unloaded_sequential_cells false
#set_boundary_optimization [tessent_get_optimize_instances] true
#set_size_only -all_instances [tessent_get_size_only_instances]

## End for MBIST

## uniquify is done automatically by compile_ultra
## uniquify -force
#----------------------------
# dc setting 
#----------------------------
source ./scripts/appvariables.tcl

#----------------------------
# constraint settings 
#----------------------------

source ./constraints/block.sdc 
set_max_area 0
set_dont_retime design true

#----------------------------
# path group settings 
#----------------------------
source ./scripts/create_path_group.tcl
#----------------------------
# sanity check 
#----------------------------
check_design                             > ./rpt/design.check_design.pre_compile.rpt
report_net_fanout -threshold 50 -nosplit > ./rpt/design.fanout.pre_compile.rpt
#----------------------------
# misc constraints 
#----------------------------
#source -verbose -echo /asic/proj/ikanos/velocity3/script/tcl/phy/phy.dc.misc_constraints.tcl
set verilogout_no_tri  "true"
source ./scripts/dont_use.tcl

##### DFT #############################################
#create_port TE       -direction in
#
#set  test_deault_scan_style multiplexed_flip_flop
#set_scan_configuration -chain_count 1
#set_scan_configuration -insert_terminal_lockup true
##set_scan_configuration -internal_clocks multi
##set_scan_configuration -internal_clocks multi
##set_scan_configuration -mix_internal_clock_driver true
##set_scan_configuration -clock_mixing mix_clocks
#
#
#create_port SE       -direction in
#create_port test_si  -direction in
#create_port test_so  -direction out
#
#current_design design
#
#set_dft_signal -view existing -type ScanClock -port CLK  -timing {45 55 }
##set_dft_signal -view existing -type ScanClock -port ijtag_tck  -timing {45 55 }
#set_dft_signal -view existing -type Reset -active_state  0 -port RST_N
##set_dft_signal -view existing -type Reset -active_state  1 -port ijtag_reset
#
#
#set_dft_signal -view  spec -type ScanEnable -port SE
#set_dft_signal -view  spec -type ScanDataIn -port test_si
#set_dft_signal -view  spec -type ScanDataOut  -port test_so
#
#######  auto fix   Reset controll for MBIST ASYNC_RESET flops   ####################
#
#set_dft_signal -view spec -type TestMode   -active_state 1  -port  TE
#set_dft_signal -view spec -type ScanEnable -port SE -active 1
##set_dft_configuration  -fix_reset enable
##set_dft_configuration -fix_set enable
##set_dft_configuration -fix_clock enable
##set_autofix_configuration -type reset 
##set_autofix_configuration -type set -test_data TE
##set_autofix_configuration -type clock -test_data TE
##set_autofix_configuration -type reset  -fix_data enable -control_signal SE
#
#
#
#
################################################################
#
#set_scan_path chain0 -view spec -scan_data_in test_si -scan_data_out test_so -scan_enable SE
#
#
#compile_ultra -no_autoungroup -no_boundary_optimization -scan -no_seq_output_inversion 
#
#create_test_protocol
#dft_drc -verbose >  ./rpt/dmem_scan_pre_drc_scan.rpt
#preview_dft -show all > ./rpt/dmem_scan_pre.reports
#
#
#insert_dft
#
#report_scan_path > ./rpt/dmem_scan_path.rpt
#dft_drc -coverage >  ./rpt/design_scan_post_coverage.rpt
#
#write -format verilog -hier -output ./output/dmem_scan.v
#write_test_protocol  -output  ./output/dmem_scan.spf
#write_test_model -format ctl -output ./output/design.ctl
#write -format ddc -hierarchy -output ./output/design_scan.ddc
#
#set_max_dynamic_power 0
##----------------------------
## compile 
##----------------------------
##compile_ultra -no_autoungroup -no_boundary_optimization -gate_clock -scan
#set compile_enable_constant_propagation_with_no_boundary_opt false
compile_ultra -incremental -no_autoungroup -no_boundary_optimization  -no_seq_output_inversion 
#compile_ultra -gate_clock -scan
write -f ddc     -hier -o   ./output/design_prechange.ddc
write -f verilog -hier -o   ./output/design_prechange.v
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
write -f ddc     -hier -o   ./output/design.dc_compile_ultra_1.flat.ddc
write -f verilog -hier -o   ./output/design.dc_compile_ultra_1.flat.v
report_reference -nosplit > ./rpt/design.dc_compile_ultra_1.ref.flat.rpt
#write_scan_def -output ./output/design_postchange_scan.def
write_sdc ./output/design.dc_compile_ultra_1.sdc
write_sdf ./output/design.dc_compile_ultra_1.sdf
write_test_model -format ctl -output ./output/design.dc_compile_ultra_1.flat.ctl

report_qor > ./rpt/design.dc_compile_ultra_1.qor
report_timing -nosplit -max_paths 5000 -input -nets -cap -tran -nosplit -sig 3                 > ./rpt/design.dc_compile_ultra_1.setup.all.tim.rpt.by_group
report_timing -nosplit -max_paths 5000 -input -nets -cap -tran -nosplit -sig 3 -sort_by slack  > ./rpt/design.dc_compile_ultra_1.setup.all.tim.rpt.by_slack
report_timing -nosplit -max_paths 2000 -input -nets -cap -tran -nosplit -sig 3 -group f2f      > ./rpt/design.dc_compile_ultra_1.setup.f2f.tim.rpt
report_timing -nosplit -max_paths 2000 -input -nets -cap -tran -nosplit -sig 3 -group i2f      > ./rpt/design.dc_compile_ultra_1.setup.i2f.tim.rpt
report_timing -nosplit -max_paths 2000 -input -nets -cap -tran -nosplit -sig 3 -group f2o      > ./rpt/design.dc_compile_ultra_1.setup.f2o.tim.rpt
report_timing -nosplit -max_paths 2000 -input -nets -cap -tran -nosplit -sig 3 -group i2o      > ./rpt/design.dc_compile_ultra_1.setup.i2o.tim.rpt


report_area -nosplit -hier > ./rpt/design.dc_compile_ultra_1.area.rpt
report_clock_gating -verbose -gated  -gating_elements  > ./rpt/design.dc_compile_1.clock_gate.rpt
check_design                             > ./rpt/design.check_design.dc_compile_1.rpt
report_net_fanout -threshold 50 -nosplit > ./rpt/design.fanout.dc_compile_1.rpt
check_timing                             > ./rpt/design.check_timing.dc_compile_1.rpt
report_resources -nosplit -hierarchy > ./rpt/design.report_resources.compile_1
report_power > ./rpt/design.power.rpt

create_block_abstraction
write -f ddc -hier -o   ./output/design.abstraction.ddc
