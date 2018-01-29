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
package jtagdtm;
/*====== Package imports ======= */
	import Clocks::*;
	import ConcatReg::*;
	import FIFO::*;
	import FIFOF::*;
	import SpecialFIFOs::*;
	import DReg::*;
/*======= Project imports ===== */
	`include "jtagdefines.bsv"
	import defined_types::*;
/*============================== */

interface Ifc_jtagdtm;
	/*======== Scan input pins ===== */
	(*always_enabled,always_ready*)
	method Action scan_out_1_i(Bit#(1) scan_out_1);
	(*always_enabled,always_ready*)
	method Action scan_out_2_i(Bit#(1) scan_out_2);
	(*always_enabled,always_ready*)
	method Action scan_out_3_i(Bit#(1) scan_out_3);
	(*always_enabled,always_ready*)
	method Action scan_out_4_i(Bit#(1) scan_out_4);
	(*always_enabled,always_ready*)
	method Action scan_out_5_i(Bit#(1) scan_out_5);
	/*======= SCAN Output Pins ====== */
	(*always_enabled,always_ready*)
	method Bit#(1) scan_in_1;
	(*always_enabled,always_ready*)
	method Bit#(1) scan_in_2;
	(*always_enabled,always_ready*)
	method Bit#(1) scan_in_3;
	(*always_enabled,always_ready*)
	method Bit#(1) scan_in_4;
	(*always_enabled,always_ready*)
	method Bit#(1) scan_in_5;
	(*always_enabled,always_ready*)
	method Bit#(1) scan_en;
	(*always_enabled,always_ready*)
	method Bit#(1) scan_mode_te;
	/*======= BOUNDARY SCAN Output Pin ====== */
	(*always_enabled,always_ready*)
	method Action bs_chain_i(Bit#(1) bs_chain);
	/*======= BOUNDARY SCAN input Pins ====== */
	(*always_enabled,always_ready*)
    method Bit#(1) shiftBscan2Edge;
	(*always_enabled,always_ready*)
    method Bit#(1) selectJtagInput;
	(*always_enabled,always_ready*)
    method Bit#(1) selectJtagOutput;
	(*always_enabled,always_ready*)
    method Bit#(1) updateBscan;
	(*always_enabled,always_ready*)
	method Bit#(1) bscan_in;
	(*always_enabled,always_ready*)
	method Bit#(1) scan_shift_en;
	/*======== JTAG input pins ===== */
	(*always_enabled,always_ready*)
	method Action tms_i(Bit#(1) tms);
	(*always_enabled,always_ready*)
	method Action tdi_i(Bit#(1) tdi);
	/*==== inputs from Sub-modules === */
	method Action debug_tdi_i(Bit#(1) debug_tdi);
	/*======= JTAG Output Pins ====== */
	(*always_enabled,always_ready*)
	method Bit#(1) tdo;
	method Bit#(1) tdo_oe;
	/*======== TAP States ============= */
	method Bit#(1) shift_dr;
	method Bit#(1) pause_dr;
	method Bit#(1) update_dr;
	method Bit#(1) capture_dr;
	/*=========== Output for BS Chain ==== */
	method Bit#(1) extest_select;
	method Bit#(1) sample_preload_select;
	method Bit#(1) debug_select;
	method Bit#(1) debug_tdo;
	/*================================ */
	method Action response_from_dm(Bit#(34) responsedm);
	method ActionValue#(Bit#(40)) request_to_dm;

endinterface
  
  function Reg#(t) readOnlyReg(t r);
    return (interface Reg;
       method t _read = r;
       method Action _write(t x) = noAction;
    endinterface);
  endfunction
	function Reg#(Bit#(1)) condwriteSideEffect(Reg#(Bit#(1)) r, Action a);
		return (interface Reg;
            method Bit#(1) _read = r._read;
            method Action _write(Bit#(1) x);
                r._write(x);
					 if(x==1)
						a;
            endmethod
        endinterface);
	endfunction
	


typedef enum {TestLogicReset = 4'h0,  RunTestIdle    = 4'h1,  SelectDRScan   = 4'h2,
      CaptureDR      = 4'h3,  ShiftDR        = 4'h4,  Exit1DR        = 4'h5,
      PauseDR        = 4'h6,  Exit2DR        = 4'h7,  UpdateDR       = 4'h8,
      SelectIRScan   = 4'h9,  CaptureIR      = 4'ha,  ShiftIR        = 4'hb,
      Exit1IR        = 4'hc,  PauseIR        = 4'hd,  Exit2IR        = 4'he,
      UpdateIR       = 4'hf } TapStates deriving(Bits,Eq,FShow);

	(*synthesize*)
	(*descending_urgency="scan_logic,scan_shift_en"*)
	module mkjtagdtm(Ifc_jtagdtm);
	Clock def_clk<-exposeCurrentClock;
	Clock invert_clock<-invertCurrentClock;
	Reset invert_reset<-mkAsyncResetFromCR(0,invert_clock);
	
	/*========= FIFOs to communicate with the DM==== */
	FIFOF#(Bit#(40)) request_to_DM <-mkUGFIFOF1();
	FIFOF#(Bit#(34)) response_from_DM <-mkUGFIFOF1();
	/*================================================ */

	/*=== Wires to capture the input pins === */
	Wire#(Bit#(1)) wr_tms<-mkDWire(0);
	Wire#(Bit#(1)) wr_tdi<-mkDWire(0);
	Reg#(Bit#(1)) wr_debug_tdi<-mkRegA(0);
	Reg#(Bit#(1)) wr_bs_chain_tdo<-mkRegA(0);
	/*======================================== */
	
	Wire#(Bit#(1)) wr_scan_in_1_all <-mkDWire(0);
	Wire#(Bit#(1)) wr_scan_in_2_out1 <-mkDWire(0);
	Wire#(Bit#(1)) wr_scan_in_3_out2 <-mkDWire(0);
	Wire#(Bit#(1)) wr_scan_in_4_out3 <-mkDWire(0);
	Wire#(Bit#(1)) wr_scan_in_5_out4 <-mkDWire(0);
	Reg#(Bit#(1)) wr_scan_shift_en[2] <-mkCRegA(2,0);

	Reg#(TapStates) tapstate<-mkRegA(TestLogicReset);
	Reg#(Bit#(5)) instruction_shiftreg<-mkRegA(0);
	Reg#(Bit#(5)) instruction<-mkRegA(`IDCODE, clocked_by invert_clock, reset_by invert_reset); // clock this by the inverted clock
	Reg#(Bit#(1)) bypass_sr<-mkRegA(0);
	Reg#(Bit#(1)) scan1_sr <-mkRegA(0);
	Reg#(Bit#(1)) scan2_sr <-mkRegA(0);
	Reg#(Bit#(1)) scan3_sr <-mkRegA(0);
	Reg#(Bit#(1)) scan4_sr <-mkRegA(0);
	Reg#(Bit#(1)) scan5_sr <-mkRegA(0);
	Reg#(Bit#(1)) scanall_sr<-mkRegA(0);
	Reg#(Bit#(1)) scan_en_sr<-mkRegA(0);
	Reg#(Bit#(1)) scan_mode_te_sr<-mkRegA(0);
	Reg#(Bit#(1)) full_scan_en_sr<-mkRegA(0);
	Reg#(Bit#(1)) scan_out_1_sr<-mkRegA(0);
	Reg#(Bit#(1)) scan_out_2_sr<-mkRegA(0);
	Reg#(Bit#(1)) scan_out_3_sr<-mkRegA(0);
	Reg#(Bit#(1)) scan_out_4_sr<-mkRegA(0);
	Reg#(Bit#(1)) scan_out_5_sr<-mkRegA(0);
    Wire#(Bit#(1)) shiftBscan2Edge_sr<-mkDWire(0);  
    Wire#(Bit#(1)) selectJtagInput_sr<-mkDWire(0);
    Wire#(Bit#(1)) selectJtagOutput_sr<-mkDWire(0);
    Wire#(Bit#(1)) updateBscan_sr<-mkDWire(0);
    Reg#(Bit#(1)) bs_sr<-mkRegA(0);
	Reg#(Bit#(32)) idcode_sr<-mkRegA(`IDCODEVALUE);

	Wire#(Bool)		wr_dmihardreset_generated<-mkDWire(False);
	Reg#(Bit#(1))	rg_dmihardreset<-mkRegA(0);
	Reg#(Bit#(1))	dmihardreset=condwriteSideEffect(rg_dmihardreset,wr_dmihardreset_generated._write(True));
	Wire#(Bool)		wr_dmireset_generated<-mkDWire(False);
	Reg#(Bit#(1))	rg_dmireset<-mkDReg(0);
	Reg#(Bit#(1))	dmireset=condwriteSideEffect(rg_dmireset,wr_dmireset_generated._write(True));
	Reg#(Bit#(3))	idle=readOnlyReg(3'd7);
	Reg#(Bit#(2))	dmistat<-mkRegA(0);
	Reg#(Bit#(6))	abits =readOnlyReg(6'd6);
	Reg#(Bit#(4))	version = readOnlyReg('d1);
	Reg#(Bit#(32)) dtmcontrol=concatReg8(readOnlyReg(14'd0),
		dmihardreset,dmireset,readOnlyReg(1'd0),
		idle,readOnlyReg(dmistat),abits,version);
	Reg#(Bit#(32)) dtmcontrol_shiftreg<-mkRegA({17'd0,3'd7,2'd0,6'd6,4'd1});

	Reg#(Bit#(40)) dmiaccess_shiftreg[2]<-mkCReg(2,'d2);
	Reg#(Bit#(2))	response_status<-mkReg(0);
	Reg#(Bool)		capture_repsonse_from_dm<-mkRegA(False);
	Reg#(Bit#(1)) rg_tdo<-mkRegA(0, clocked_by invert_clock, reset_by invert_reset);

	ReadOnly#(TapStates) crossed_tapstate		<-mkNullCrossingWire(invert_clock,tapstate);
	ReadOnly#(Bit#(5))	crossed_instruction_shiftreg<-mkNullCrossingWire(invert_clock,instruction_shiftreg);
	ReadOnly#(Bit#(5))	crossed_instruction	<-mkNullCrossingWire(def_clk,instruction);
	ReadOnly#(Bit#(1))	crossed_scan_out_1_sr	<-mkNullCrossingWire(invert_clock,scan_out_1_sr);
	ReadOnly#(Bit#(1))	crossed_scan_out_2_sr	<-mkNullCrossingWire(invert_clock,scan_out_2_sr);
	ReadOnly#(Bit#(1))	crossed_scan_out_3_sr	<-mkNullCrossingWire(invert_clock,scan_out_3_sr);
	ReadOnly#(Bit#(1))	crossed_scan_out_4_sr	<-mkNullCrossingWire(invert_clock,scan_out_4_sr);
	ReadOnly#(Bit#(1))	crossed_scan_out_5_sr	<-mkNullCrossingWire(invert_clock,scan_out_5_sr);
	ReadOnly#(Bit#(1))	crossed_scan_en_sr		<-mkNullCrossingWire(invert_clock,scan_en_sr);
	ReadOnly#(Bit#(1))	crossed_scan_mode_te_sr	<-mkNullCrossingWire(invert_clock,scan_mode_te_sr);
	ReadOnly#(Bit#(1))	crossed_full_scan_en_sr	<-mkNullCrossingWire(invert_clock,full_scan_en_sr);
	ReadOnly#(Bit#(1))	crossed_bypass_sr		<-mkNullCrossingWire(invert_clock,bypass_sr);
	ReadOnly#(Bit#(32))	crossed_idcode_sr		<-mkNullCrossingWire(invert_clock,idcode_sr);
	ReadOnly#(Bit#(1))	crossed_bs_chain_tdo	<-mkNullCrossingWire(invert_clock,wr_bs_chain_tdo);
	ReadOnly#(Bit#(1))	crossed_debug_tdi		<-mkNullCrossingWire(invert_clock,wr_debug_tdi);
	ReadOnly#(Bit#(32))	crossed_dtmcontrol_shiftreg<-mkNullCrossingWire(invert_clock,dtmcontrol_shiftreg);
	ReadOnly#(Bit#(1)) crossed_output_tdo<-mkNullCrossingWire(def_clk,rg_tdo);
	ReadOnly#(Bit#(40)) crossed_dmiaccess_shiftreg<-mkNullCrossingWire(invert_clock,dmiaccess_shiftreg[0]);

   Bit#(1) bypass_sel   = crossed_instruction == `BYPASS?1:0;
   Bit#(1) scan_en_sel   = crossed_instruction == `SCANEN?1:0;
   Bit#(1) scan_mode_te_sel   = crossed_instruction == `SCANMODE_TE?1:0;
   Bit#(1) scan1_sel    = crossed_instruction == `SCAN1?1:0;
   Bit#(1) scan2_sel    = crossed_instruction == `SCAN2?1:0;
   Bit#(1) scan3_sel    = crossed_instruction == `SCAN3?1:0;
   Bit#(1) scan4_sel    = crossed_instruction == `SCAN4?1:0;
   Bit#(1) scan5_sel    = crossed_instruction == `SCAN5?1:0;
   Bit#(1) scanall_sel  = crossed_instruction == `SCANALL?1:0;
   Bit#(1) full_scan_en_sel  = crossed_instruction == `FULLSCANEN?1:0;
   Bit#(1) idcode_sel   = crossed_instruction == `IDCODE?1:0;
   Bit#(1) dbg_sel      = crossed_instruction == `DEBUG?1:0;
   Bit#(1) dtmcontrol_sel  = crossed_instruction == `DTMCONTROL?1:0;
   Bit#(1) dmi_sel      = crossed_instruction == `DMIACCESS?1:0;
   Bit#(1) extest_select=crossed_instruction==`EXTEST?1:0;
   Bit#(1) sample_preload_select=crossed_instruction==`SAMPLE_PRELOAD?1:0;

	Bit#(1) instruction_tdo=crossed_instruction_shiftreg[0];
	Bit#(1) bypass_tdo=crossed_bypass_sr;
	Bit#(1) scan_en_tdo=crossed_scan_en_sr;
	Bit#(1) scan_mode_te_tdo=crossed_scan_mode_te_sr;
	Bit#(1) full_scan_en_tdo=crossed_full_scan_en_sr;
	Bit#(1) scan_out_1_tdo=crossed_scan_out_1_sr;
	Bit#(1) scan_out_2_tdo=crossed_scan_out_2_sr;
	Bit#(1) scan_out_3_tdo=crossed_scan_out_3_sr;
	Bit#(1) scan_out_4_tdo=crossed_scan_out_4_sr;
	Bit#(1) scan_out_5_tdo=crossed_scan_out_5_sr;
	Bit#(1) idcode_tdo=crossed_idcode_sr[0];
	Bit#(1) dtmcontrol_tdo=crossed_dtmcontrol_shiftreg[0];
	Bit#(1) dmiaccess_tdo=crossed_dmiaccess_shiftreg[0][0];

	

	/*== This rule implements the TAPs STATE MACHINE====== */
	rule just_display;
		`ifdef verbose $display($time,"\tTAPSTATE: ",fshow(tapstate),"\tINSTRUCTION: %h",instruction_shiftreg); `endif
	endrule
	rule tap_state_machine;
		case(tapstate)
			TestLogicReset: if(wr_tms==0) tapstate<=RunTestIdle;
         RunTestIdle   : if(wr_tms==1) tapstate <= SelectDRScan;
         SelectDRScan  : if(wr_tms==1) tapstate <= SelectIRScan;
                         else          tapstate <= CaptureDR;
         CaptureDR     : if(wr_tms==0) tapstate <= ShiftDR;
                         else          tapstate <= Exit1DR;
         ShiftDR       : if(wr_tms==1) tapstate <= Exit1DR;
         Exit1DR       : if(wr_tms==0) tapstate <= PauseDR;
                         else          tapstate <= UpdateDR;
         PauseDR       : if(wr_tms==1) tapstate <= Exit2DR;
         Exit2DR       : if(wr_tms==1) tapstate <= UpdateDR;
                         else          tapstate <= ShiftDR;
         UpdateDR      : if(wr_tms==1) tapstate <= SelectDRScan;
                         else          tapstate <= RunTestIdle;
         SelectIRScan  : if(wr_tms==1) tapstate <= TestLogicReset;
                         else          tapstate <= CaptureIR;
         CaptureIR     : if(wr_tms==0) tapstate <= ShiftIR;
                         else          tapstate <= Exit1IR;
         ShiftIR       : if(wr_tms==1) tapstate <= Exit1IR;
         Exit1IR       : if(wr_tms==0) tapstate <= PauseIR;
                         else          tapstate <= UpdateIR;
         PauseIR       : if(wr_tms==1) tapstate <= Exit2IR;
         Exit2IR       : if(wr_tms==1) tapstate <= UpdateIR;
                         else          tapstate <= ShiftIR;
         UpdateIR      : if(wr_tms==1) tapstate <= SelectDRScan;
                         else          tapstate <= RunTestIdle;
         default       :               tapstate <= TestLogicReset;
		endcase
	endrule

	rule dmireset_generated(wr_dmireset_generated);
		`ifdef verbose $display($time,"\tDTM: Received DMIRESET"); `endif
		dmiaccess_shiftreg[1][1:0]<='d0;
		response_status<=0;
		capture_repsonse_from_dm<=False;
	endrule
	rule dmihardreset_generated(wr_dmihardreset_generated);
		request_to_DM.deq;
		response_from_DM.deq;
		capture_repsonse_from_dm<=False;
	endrule

	/*======= perform dtmcontrol shifts ======== */
	rule shift_dtm;
		case(tapstate)
			TestLogicReset: dtmcontrol<={17'd0,idle,2'b0,abits,version};
			CaptureDR:	if(dtmcontrol_sel==1) dtmcontrol_shiftreg<=dtmcontrol;
			ShiftDR:		if(dtmcontrol_sel==1) dtmcontrol_shiftreg<={wr_tdi,dtmcontrol_shiftreg[31:1]};
			UpdateDR:	if(dtmcontrol_sel==1) dtmcontrol<=dtmcontrol_shiftreg;
		endcase
	endrule
	/*========================================== */
	/*======= perform dmiaccess shifts ======== */
	rule shift_dmiaccess(!wr_dmihardreset_generated);
		case(tapstate)
			TestLogicReset: dmiaccess_shiftreg[0]<='d0;
			CaptureDR:	if(dmi_sel==1) 
				if(response_from_DM.notEmpty)begin 
					let x=response_from_DM.first[33:0];
					`ifdef verbose $display($time,"\tDTM: Getting response: data %h op: %h",x[33:2],x[1:0]); `endif
					x[1:0]=x[1:0]|response_status;// keeping the lower 2 bits sticky
					dmiaccess_shiftreg[0][33:0]<=x; 
					response_status<=x[1:0];
					response_from_DM.deq; 
					`ifdef verbose $display($time,"\tDTM: New DMIACCESS value: %h",x); `endif
					capture_repsonse_from_dm<=False;
					dmistat<=x[1:0];
				end
				else begin
					if(capture_repsonse_from_dm)
						response_status<=3;
					`ifdef verbose $display($time,"\tDTM: RESPONSE NOT AVAILABLE. DMIACCESS: %h",dmiaccess_shiftreg[0]); `endif
				end
			ShiftDR:		if(dmi_sel==1) dmiaccess_shiftreg[0]<={wr_tdi,dmiaccess_shiftreg[0][39:1]};
			UpdateDR:	if(dmi_sel==1) 
				if(request_to_DM.notFull && dmiaccess_shiftreg[0][1:0]!=0 && capture_repsonse_from_dm==False)begin
					request_to_DM.enq(dmiaccess_shiftreg[0]);
					dmiaccess_shiftreg[0][1:0]<='d3;
					capture_repsonse_from_dm<=True;
					`ifdef verbose $display($time,"\tDTM: Sending request to Debug: %h",dmiaccess_shiftreg[0]); `endif
				end
				else begin
					`ifdef verbose $display($time,"\tDTM: REQUEST NOT SERVED capture: %b DMIACCESS: %h",capture_repsonse_from_dm,dmiaccess_shiftreg[0]); `endif
//					dmistat<=3;
//					response_from_DM.enq('d3);
				end
		endcase
	endrule
	/*========================================== */

	/*== perform instruction register shifts === */
	rule shift_reg;
		case(tapstate)
			CaptureIR:	instruction_shiftreg<='b10101;
			ShiftIR  :	instruction_shiftreg<= {wr_tdi,instruction_shiftreg[4:1]};
		endcase
	endrule
	rule transfer_instruction_on_nedge; // TODO negedge here
		case(crossed_tapstate)
			TestLogicReset	:instruction<=`IDCODE;
			UpdateIR			:instruction<=crossed_instruction_shiftreg;
		endcase
	endrule

	/*==== Bypass Section === */
	rule bypass_logic;
		case(tapstate)
			TestLogicReset: bypass_sr<=1'b0;
			CaptureDR	  : if(bypass_sel==1) bypass_sr<=1'b0;
			ShiftDR		  : if(bypass_sel==1) bypass_sr<=wr_tdi;
		endcase
	endrule

	/*==== Boundary Scan Section === */
	rule bs_logic;
		case(tapstate)
			TestLogicReset: bs_sr<=1'b0;
			CaptureDR	  : begin
                                if(extest_select==1) begin 
                                    shiftBscan2Edge_sr <= 1'b0;
                                    selectJtagInput_sr <= 1'b0;
                                    selectJtagOutput_sr <= 1'b0;
                                    updateBscan_sr <= 1'b0;
                                    bs_sr<=1'b0;
                                end else if (sample_preload_select ==1) begin
                                    shiftBscan2Edge_sr <= 1'b0;
                                    selectJtagInput_sr <= 1'b0;
                                    selectJtagOutput_sr <= 1'b0;
                                    bs_sr<=1'b0;
                                end
                            end
			ShiftDR		  : begin
                                if(extest_select==1) begin 
                                    shiftBscan2Edge_sr <= 1'b1;
                                    selectJtagInput_sr <= 1'b0;
                                    selectJtagOutput_sr <= 1'b0;
                                    updateBscan_sr <= 1'b0;
                                    bs_sr<=wr_tdi;
                                end else if (sample_preload_select ==1) begin
                                    bs_sr<=wr_tdi;
                                    shiftBscan2Edge_sr <= 1'b1;
                                end 
                            end
            UpdateDR      : begin
                                if(extest_select==1) begin 
                                    shiftBscan2Edge_sr <= 1'b1;
                                    selectJtagInput_sr <= 1'b1;
                                    selectJtagOutput_sr <= 1'b1;
                                    updateBscan_sr <= 1'b1;
                                end 
                            end
		endcase
	endrule

	/*==== Scan Chain Section === */
	rule scan_logic;
		case(tapstate)
            TestLogicReset: begin
                                scan_en_sr<=1'b0;
                                scan_mode_te_sr<=1'b0;
                                scan1_sr<=1'b0;
                                scan2_sr<=1'b0;
                                scan3_sr<=1'b0;
                                scan4_sr<=1'b0;
                                scan5_sr<=1'b0;
                                scanall_sr<=1'b0;
                                full_scan_en_sr<=1'b0;
																wr_scan_shift_en[0]<=1'b0;
                            end
            CaptureDR	  : begin
                                if(scan_en_sel==1) scan_en_sr<=1'b0;
                                else if(scan_mode_te_sel==1) scan_mode_te_sr<=1'b0;
                                else if(scan1_sel==1) scan1_sr<=1'b0;
                                else if(scan2_sel==1) scan2_sr<=1'b0;
                                else if(scan3_sel==1) scan3_sr<=1'b0;
                                else if(scan4_sel==1) scan4_sr<=1'b0;
                                else if(scan5_sel==1) scan5_sr<=1'b0;
                                else if(scanall_sel==1) scanall_sr<=1'b0;
                                else if(full_scan_en_sel==1) full_scan_en_sr<=1'b0;
																wr_scan_shift_en[0]<=1'b0;
                            end
            ShiftDR		  : begin
                                if(scan_en_sel==1) scan_en_sr<=wr_tdi;
                                else if(scan_mode_te_sel==1) scan_mode_te_sr<=wr_tdi;
                                else if(scan1_sel==1) scan1_sr<=wr_tdi;
                                else if(scan2_sel==1) scan2_sr<=wr_tdi;
                                else if(scan3_sel==1) scan3_sr<=wr_tdi;
                                else if(scan4_sel==1) scan4_sr<=wr_tdi;
                                else if(scan5_sel==1) scan5_sr<=wr_tdi;
                                else if(scanall_sel==1) scanall_sr<=wr_tdi;
                                else if(full_scan_en_sel==1) full_scan_en_sr<=wr_tdi;
                                if ((scan1_sel == 1'b1 || scan2_sel  == 1'b1|| scan3_sel  == 1'b1|| scan4_sel  == 1'b1|| scan5_sel  == 1'b1|| scanall_sel == 1'b1) || (scan_en_sel == 1'b1 && wr_tdi == 1'b0)) wr_scan_shift_en[1] <=1'b1;
                            end
            UpdateDR		  : wr_scan_shift_en[0] <=1'b0;
		endcase
	endrule
    
	rule full_scan_mux_logic;
        if (full_scan_en_sr == 1'b1) begin
	        wr_scan_in_1_all <= scanall_sr;
	        wr_scan_in_2_out1 <= scan_out_1_sr;
	        wr_scan_in_3_out2 <= scan_out_2_sr;
	        wr_scan_in_4_out3 <= scan_out_3_sr;
	        wr_scan_in_5_out4 <= scan_out_4_sr;
        end
        else begin
	        wr_scan_in_1_all <= scan1_sr;
	        wr_scan_in_2_out1 <= scan2_sr;
	        wr_scan_in_3_out2 <= scan3_sr;
	        wr_scan_in_4_out3 <= scan4_sr;
	        wr_scan_in_5_out4 <= scan5_sr;
        end
	endrule

	/*======= IDCODE section === */
	rule idcode_logic;
		case(tapstate)
			TestLogicReset:idcode_sr<=`IDCODEVALUE;
			CaptureDR:	if(idcode_sel==1) idcode_sr<=`IDCODEVALUE;
			ShiftDR :   if(idcode_sel==1) idcode_sr<={wr_tdi,idcode_sr[31:1]};
		endcase
	endrule

	rule generate_tdo_outputpin;
		if(crossed_tapstate==ShiftIR)
			rg_tdo<=instruction_tdo;
		else
			case(instruction)
				`IDCODE: rg_tdo<=idcode_tdo;
				`DEBUG : rg_tdo<=crossed_debug_tdi;
				`EXTEST: rg_tdo<=crossed_bs_chain_tdo;
				`SAMPLE_PRELOAD: rg_tdo<=crossed_bs_chain_tdo;
				`BYPASS: rg_tdo<=bypass_tdo;
				`SCANEN: rg_tdo<=scan_en_tdo;
				`SCANMODE_TE: rg_tdo<=scan_mode_te_tdo;
				`FULLSCANEN: rg_tdo<=full_scan_en_tdo;
                `SCAN1: rg_tdo <= scan_out_1_tdo;
                `SCAN2: rg_tdo <= scan_out_2_tdo;
                `SCAN3: rg_tdo <= scan_out_3_tdo;
                `SCAN4: rg_tdo <= scan_out_4_tdo;
                `SCAN5: rg_tdo <= scan_out_5_tdo;
                `SCANALL: rg_tdo <= scan_out_5_tdo;
				`DTMCONTROL: rg_tdo<=dtmcontrol_tdo;
				`DMIACCESS: rg_tdo<=dmiaccess_tdo;
				default:	rg_tdo<=bypass_tdo;
			endcase
	endrule

	/*======== SCAN input (scan chain outputs) pins ===== */
	method Action scan_out_1_i(Bit#(1) scan_out_1);
		scan_out_1_sr<=scan_out_1;
	endmethod
	method Action scan_out_2_i(Bit#(1) scan_out_2);
		scan_out_2_sr<=scan_out_2;
	endmethod
	method Action scan_out_3_i(Bit#(1) scan_out_3);
		scan_out_3_sr<=scan_out_3;
	endmethod
	method Action scan_out_4_i(Bit#(1) scan_out_4);
		scan_out_4_sr<=scan_out_4;
	endmethod
	method Action scan_out_5_i(Bit#(1) scan_out_5);
		scan_out_5_sr<=scan_out_5;
	endmethod
	/*======== JTAG input pins ===== */
	method Action tms_i(Bit#(1) tms);
		wr_tms<=tms;
	endmethod
	method Action tdi_i(Bit#(1) tdi);
		wr_tdi<=tdi;
	endmethod
	/*============================= */
	method Action debug_tdi_i(Bit#(1) debug_tdi);
		wr_debug_tdi<=debug_tdi;
	endmethod
	/*======= Boundary Scan Input Pins ====== */
	method Action bs_chain_i(Bit#(1) bs_chain);
		wr_bs_chain_tdo<=bs_chain;
	endmethod
	/*======== TAP States ============= */
	method shift_dr=tapstate==ShiftDR?1:0;
	method pause_dr=tapstate==PauseDR?1:0;
	method update_dr=tapstate==UpdateDR?1:0;
	method capture_dr=tapstate==CaptureDR?1:0;
	/*=================================== */
	method debug_select				=crossed_instruction==`DEBUG?1:0;
	/*================================ */
	/*======= SCAN Output (Scan Chain Inputs) Pins ====== */
	method scan_in_1 = wr_scan_in_1_all;
	method scan_in_2 = wr_scan_in_2_out1;
	method scan_in_3 = wr_scan_in_3_out2;
	method scan_in_4 = wr_scan_in_4_out3;
	method scan_in_5 = wr_scan_in_5_out4;
	method scan_en   = scan_en_sr;
	method scan_mode_te = scan_mode_te_sr;
	/*======= Boundary Scan Output Pins ====== */
    method shiftBscan2Edge = shiftBscan2Edge_sr;
    method selectJtagInput = selectJtagInput_sr;
    method selectJtagOutput = selectJtagOutput_sr;
    method updateBscan = updateBscan_sr;
	method bscan_in   = bs_sr;
	method scan_shift_en = wr_scan_shift_en[1];
	/*======= JTAG Output Pins ====== */
	method tdo = crossed_output_tdo;
	method debug_tdo = wr_tdi;
	method Bit#(1) tdo_oe = ((tapstate == ShiftIR) || (tapstate == ShiftDR))?1:0;
	method Action response_from_dm(Bit#(34) responsedm) if(response_from_DM.notFull);
		if(capture_repsonse_from_dm)
			response_from_DM.enq(responsedm);
	endmethod
	method ActionValue#(Bit#(40)) request_to_dm if(request_to_DM.notEmpty);
		request_to_DM.deq;
		return request_to_DM.first;
	endmethod
	endmodule

endpackage
