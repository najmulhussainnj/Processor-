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
// Created on: Mon Nov 06 17:30:03 IST 2017
// Created by: vishvesh
// Bluespec version: 2017.03.beta1 2017-03-16 35049


interface Ifc_sdram_model;
	interface Inout#(Bit#(32)) dq;
	(*always_ready*)
	method Action iAddr (Bit#(11) addr);
	(*always_ready*)
	method Action iBa (Bit#(2) ba);
	(*always_ready*)
	method Action iCke (bit cke);
	(*always_ready*)
	method Action iCs_n (bit cs_n);
	(*always_ready*)
	method Action iRas_n (bit ras_n);
	(*always_ready*)
	method Action iCas_n (bit cas_n);
	(*always_ready*)
	method Action iWe_n (bit we_n);
	(*always_ready*)
	method Action iDqm (Bit#(4) dqm);
endinterface

import "BVI" mt48lc2m32b2 =
module mksdram_model_wrapper #(String file) (Ifc_sdram_model);

	parameter FILENAME = file;
	parameter addr_bits = 11;
	parameter data_bits = 32;
	parameter col_bits = 8;
	parameter mem_sizes = 524287;
	parameter tAC = 5.5;
	parameter tHZ = 5.5;
	parameter tOH = 2.5;
	parameter tMRD = 2.0;
	parameter tRAS = 42.0;
	parameter tRC = 60.0;
	parameter tRCD = 18.0;
	parameter tRFC = 60.0;
	parameter tRP = 18.0;
	parameter tRRD = 12.0;
	parameter tWRa = 6.0;
	parameter tWRm = 12.0;

	default_clock clk_old_Clk;
	default_reset rst;

	input_clock clk_old_Clk (old_Clk)  <- exposeCurrentClock;
	input_reset rst (/* empty */) clocked_by(clk_old_Clk)  <- exposeCurrentReset;

	ifc_inout dq(Dq) clocked_by (clk_old_Clk) reset_by (rst);

	method iAddr (Addr /*addr_bits-1:0*/)
		 enable((*inhigh*)iAddr_enable) clocked_by(clk_old_Clk) reset_by(rst);
	method iBa (Ba /*1:0*/)
		 enable((*inhigh*)iBa_enable) clocked_by(clk_old_Clk) reset_by(rst);
	method iCke (Cke )
		 enable((*inhigh*)iCke_enable) clocked_by(clk_old_Clk) reset_by(rst);
	method iCs_n (Cs_n )
		 enable((*inhigh*)iCs_n_enable) clocked_by(clk_old_Clk) reset_by(rst);
	method iRas_n (Ras_n )
		 enable((*inhigh*)iRas_n_enable) clocked_by(clk_old_Clk) reset_by(rst);
	method iCas_n (Cas_n)
		 enable((*inhigh*)iCas_n_enable) clocked_by(clk_old_Clk) reset_by(rst);
	method iWe_n (We_n )
		 enable((*inhigh*)iWe_n_enable) clocked_by(clk_old_Clk) reset_by(rst);
	method iDqm (Dqm /*3:0*/)
		 enable((*inhigh*)iDqm_enable) clocked_by(clk_old_Clk) reset_by(rst);

	schedule iAddr C iAddr;
	schedule iAddr CF iBa;
	schedule iAddr CF iCke;
	schedule iAddr CF iCs_n;
	schedule iAddr CF iRas_n;
	schedule iAddr CF iCas_n;
	schedule iAddr CF iWe_n;
	schedule iAddr CF iDqm;
	schedule iBa C iBa;
	schedule iBa CF iCke;
	schedule iBa CF iCs_n;
	schedule iBa CF iRas_n;
	schedule iBa CF iCas_n;
	schedule iBa CF iWe_n;
	schedule iBa CF iDqm;
	schedule iCke C iCke;
	schedule iCke CF iCs_n;
	schedule iCke CF iRas_n;
	schedule iCke CF iCas_n;
	schedule iCke CF iWe_n;
	schedule iCke CF iDqm;
	schedule iCs_n C iCs_n;
	schedule iCs_n CF iRas_n;
	schedule iCs_n CF iCas_n;
	schedule iCs_n CF iWe_n;
	schedule iCs_n CF iDqm;
	schedule iRas_n C iRas_n;
	schedule iRas_n CF iCas_n;
	schedule iRas_n CF iWe_n;
	schedule iRas_n CF iDqm;
	schedule iCas_n C iCas_n;
	schedule iCas_n CF iWe_n;
	schedule iCas_n CF iDqm;
	schedule iWe_n C iWe_n;
	schedule iWe_n CF iDqm;
	schedule iDqm C iDqm;
endmodule


