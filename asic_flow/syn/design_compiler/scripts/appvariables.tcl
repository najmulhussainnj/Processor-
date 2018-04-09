#----------------------------
# dc setting 
#----------------------------
set compile_slack_driven_buffering true
set_wire_load_mode enclosed
#set compile_delete_unloaded_sequential_cells true
set compile_seqmap_propagate_constants true
set dont_bind_unused_pins_to_logic_constant true
set synlib_model_map_effort high
set hdlin_enable_presto true
set compile_seqmap_synchronous_extraction true
set timing_enable_multiple_clocks_per_reg true
#set_ultra_optimization true
set_fix_multiple_port_nets  -all -buffer_constants
set_cost_priority -delay
set compile_seqmap_identify_shift_registers false
set collection_result_display_limit -1
set power_cg_flatten  true
#set timing_non_unate_clock_compatibility true
set case_analysis_with_logic_constants true
set_leakage_optimization true
set_wire_load_mode enclosed


