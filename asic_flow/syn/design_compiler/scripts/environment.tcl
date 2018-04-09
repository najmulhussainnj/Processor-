#-------------------------
# Library setting
#-------------------------
set design <TOP_MODULE_NAME>
set search_path "<Provide all library path to be used for synthesis> $search_path"
set target_library    [list]
set link_library      "*"
set synthetic_library [list <List synopsys synthetic library .sldb>]
set target_library    [concat $target_library <List all the target library .db>]

set link_library      [concat $link_library $target_library $synthetic_library ]
#------------------------
# Variables
#-----------------------
set RTL $env(SHAKTI_HOME)/verilog
set DATE [clock format [clock seconds] -format "%b%d-%T"]
set _OUTPUTS_PATH outputs_${DATE}
set _REPORTS_PATH reports_${DATE}
set _LOG_PATH logs_${DATE}
