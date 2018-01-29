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
// Created on: Wed Dec 13 19:09:48 IST 2017
// Created by: vinod
// Bluespec version: 2017.07.A 2017-07-21 1da80f1


interface Ifc_FlashWrapper;
	interface Inout#(Bit#(1)) si;
	interface Inout#(Bit#(1)) so;
	interface Inout#(Bit#(1)) wpNeg;
	interface Inout#(Bit#(1)) resetNeg;
	(*always_ready , always_enabled*)
	method Action iSCK (Bit#(1) sck);
	(*always_ready , always_enabled*)
	method Action iCSNeg (Bit#(1) icsneg);
endinterface

import "BVI" CypressFlashWrapper =
module mkCypressFlashWrapper  (Ifc_FlashWrapper);

	default_clock clk_clk;
	default_reset rst_rst;

	input_clock clk_clk (clk)  <- exposeCurrentClock;
	input_reset rst_rst (rst) clocked_by(clk_clk)  <- exposeCurrentReset;
	ifc_inout si(SI);
	ifc_inout so(SO);
	ifc_inout wpNeg(WPNeg);
	ifc_inout resetNeg(RESETNeg);
	

	method iSCK (SCK)
		 enable((*inhigh*)iSCK_enable) clocked_by(clk_clk) reset_by(rst_rst);
	method iCSNeg (CSNeg)
		 enable((*inhigh*)iCSNeg_enable) clocked_by(clk_clk) reset_by(rst_rst);

	schedule iSCK C iSCK;
	schedule iSCK CF iCSNeg;
	schedule iCSNeg C iCSNeg;
endmodule


