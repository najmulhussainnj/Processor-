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
// Created on: Sat Dec 09 23:34:55 IST 2017
// Created by: vinod
// Bluespec version: 2017.07.A 2017-07-21 1da80f1
package bsvmkM24AA1025;

interface IFC_EEPROM;
	interface Inout#(bit) linesda;
	(*always_ready*)
	method Action iA0 (bit a0);
	(*always_ready*)
	method Action iA1 (bit a1);
	(*always_ready*)
	method Action iA2 (bit a2);
	(*always_ready*)
	method Action iWP (bit wp);
	(*always_ready*)
	method Action iSCL (bit scl);
endinterface

import "BVI" M24AA1025 =
module mkM24AA1025  (IFC_EEPROM);

	default_clock clk;
	default_reset rst_RESET;

	input_clock clk ()  <- exposeCurrentClock;
	input_reset rst_RESET (RESET) clocked_by(clk)  <- exposeCurrentReset;

	ifc_inout linesda(SDA);

	method iA0 (A0 )
		 enable((*inhigh*)iA0_enable) clocked_by(clk) reset_by(rst_RESET);
	method iA1 (A1 )
		 enable((*inhigh*)iA1_enable) clocked_by(clk) reset_by(rst_RESET);
	method iA2 (A2 )
		 enable((*inhigh*)iA2_enable) clocked_by(clk) reset_by(rst_RESET);
	method iWP (WP )
		 enable((*inhigh*)iWP_enable) clocked_by(clk) reset_by(rst_RESET);
	method iSCL (SCL )
		 enable((*inhigh*)iSCL_enable) clocked_by(clk) reset_by(rst_RESET);

	schedule iA0 C iA0;
	schedule iA0 CF iA1;
	schedule iA0 CF iA2;
	schedule iA0 CF iWP;
	schedule iA0 CF iSCL;
	schedule iA1 C iA1;
	schedule iA1 CF iA2;
	schedule iA1 CF iWP;
	schedule iA1 CF iSCL;
	schedule iA2 C iA2;
	schedule iA2 CF iWP;
	schedule iA2 CF iSCL;
	schedule iWP C iWP;
	schedule iWP CF iSCL;
	schedule iSCL C iSCL;
endmodule

endpackage
