#/*
#Copyright (c) 2013, IIT Madras All rights reserved.
#
#Redistribution and use in source and binary forms, with or without modification, are permitted
#provided that the following conditions are met:
#
#* Redistributions of source code must retain the above copyright notice, this list of conditions
#  and the following disclaimer.  
#* Redistributions in binary form must reproduce the above copyright notice, this list of 
#  conditions and the following disclaimer in the documentation and/or other materials provided 
# with the distribution.  
#* Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or 
#  promote products derived from this software without specific prior written permission.
#
#THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
#OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
#AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
#CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
#DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
#IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
#OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#--------------------------------------------------------------------------------------------------
#
#Author: M.Gopinathan
#Email id: gopinathan18@gmail.com
#Details:Make file for Synthesis in Cadence RTL Compiler and Synopsys Design Compiler 
#
#--------------------------------------------------------------------------------------------------
#*/
dc:
	@mkdir -p design_compiler/work
	@rm -rf $$SHAKTI_HOME/verilog/*.h $$SHAKTI_HOME/verilog/*.c
	@cd design_compiler/work; design_vision -f ../scripts/dc.tcl | tee dc.log

rc:
	@mkdir -p rtl_compiler/work
	@rm -rf $$SHAKTI_HOME/verilog/*.h $$SHAKTI_HOME/verilog/*.c
	@cd rtl_compiler/work; rc -64 -files ../scripts/run_rtlc.tcl | tee rc.log

dc_clean:
	@rm -rf design_compiler/work/*.log design_compiler/work/*.pvl design_compiler/work/*.syn design_compiler/work/*.mr design_compiler/work/alib-52
	
rc_clean:
	@rm -rf rtl_compiler/work/rc*

dc_clean_all: dc_clean
	@rm -rf design_compiler/report* design_compiler/output* design_compiler/work

rc_clean_all: rc_clean
	@rm -rf rtl_compiler/report* rtl_compiler/output* rtl_compiler/log* rtl_compiler/work
