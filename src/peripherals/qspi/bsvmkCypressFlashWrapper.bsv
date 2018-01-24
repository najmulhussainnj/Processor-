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


