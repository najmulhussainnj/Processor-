##############################################################################
## Preset global variables and attributes
##############################################################################
set RTL $env(SHAKTI_HOME)/verilog
set DESIGN <TOP_MODULE_NAME>
set SYN_EFF high
###set MAP_EFF medium
set MAP_EFF high
exec ls $RTL/ > rtl_files.txt
set fp [open "../work/rtl_files.txt" r]
set rtl_list [read $fp]
set DATE [clock format [clock seconds] -format "%b%d-%T"] 
set _OUTPUTS_PATH outputs_${DATE}
set _REPORTS_PATH reports_${DATE}
set _LOG_PATH logs_${DATE}

##set_attribute hdl_vhdl_read_version 1993 /

##set ET_WORKDIR <ET work directory>
set_attribute lib_search_path {. <Paths to standard cell libraries separated by space>}  
set_attribute script_search_path {../constraints}
set_attribute hdl_search_path $RTL
set_attribute library {<Specify all library.lib separated by space>}
