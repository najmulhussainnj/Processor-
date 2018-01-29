/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*/
// Bluespec wrapper, created by Import BVI Wizard
// Created on: Thu Nov 09 18:42:21 IST 2017
// Created by: vinod
// Bluespec version: 2017.03.beta1 2017-03-16 35049


interface Ifc_MicronFlashWrapper;
	interface Inout#(Bit#(1)) dq0;
	interface Inout#(Bit#(1)) dq1;
	interface Inout#(Bit#(1)) hold_dq3;
	interface Inout#(Bit#(1)) vpp_w_dq2;
	(*always_ready , always_enabled*)
	method Action iS (Bit#(1) s);
	(*always_ready , always_enabled*)
	method Action iC (Bit#(1) c);
	(*always_ready , always_enabled*)
	method Action iVcc (Bit#(32) vcc);
endinterface

import "BVI" MicronFlashWrapper =
module mkMicronFlashWrapper  (Ifc_MicronFlashWrapper);

	default_clock clk_clk;
	default_reset rst_rst;

	input_clock clk_clk (clk)  <- exposeCurrentClock;
	input_reset rst_rst (rst) clocked_by(clk_clk)  <- exposeCurrentReset;

	ifc_inout dq0(DQ0);
	ifc_inout dq1(DQ1);
	ifc_inout hold_dq3(HOLD_DQ3);
	ifc_inout vpp_w_dq2(Vpp_W_DQ2);

	method iS (S )
		 enable((*inhigh*)iS_enable) clocked_by(clk_clk) reset_by(rst_rst);
	method iC (C )
		 enable((*inhigh*)iC_enable) clocked_by(clk_clk) reset_by(rst_rst);
	method iVcc (Vcc /*31:0*/)
		 enable((*inhigh*)iVcc_enable) clocked_by(clk_clk) reset_by(rst_rst);

	schedule iS CF iS;
	schedule iS CF iC;
	schedule iS CF iVcc;
	schedule iC CF iC;
	schedule iC CF iVcc;
	schedule iVcc CF iVcc;
endmodule


