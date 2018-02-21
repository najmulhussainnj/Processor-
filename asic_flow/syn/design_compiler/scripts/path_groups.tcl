group_path -name INPUT  -from [remove_from_collection [all_inputs] CLK]  -to [all_registers]
group_path -name IN2OUT -from [remove_from_collection [all_inputs] CLK]  -to [all_outputs]
group_path -critical_range 500 -name OUTPUT -from [all_registers] -to [all_outputs]
group_path -critical_range 500 -name REG2REG -weight 4 -from  [all_registers] -to   [all_registers]
group_path -critical_range 500 -name REG2MEM -weight 4 \
    -from [filter_collection [all_registers] "is_hard_macro != true"] \
    -to   [filter_collection [all_registers] "is_hard_macro == true"]
group_path -critical_range 500 -name MEM2REG -weight 4 \
   -from [filter_collection [all_registers] "is_hard_macro == true"] \
    -to   [filter_collection [all_registers] "is_hard_macro != true"]
group_path -critical_range 500 -name MEM2MEM -weight 5 \
    -from [filter_collection [all_registers] "is_hard_macro == true"] \
   -to   [filter_collection [all_registers] "is_hard_macro == true"]

