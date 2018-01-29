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
package jtagdtm_new;
/*====== Package imports ======= */
	import ConcatReg::*;
	import FIFO::*;
	import FIFOF::*;
	import SpecialFIFOs::*;
/*======= Project imports ===== */
	`include "jtagdefines.bsv"
	import defined_types::*;
/*============================== */

interface Ifc_jtagdtm;
	/*======== JTAG input pins ===== */
	(*always_enabled,always_ready*)
	method Action tms_i(Bit#(1) tms);
	(*always_enabled,always_ready*)
	method Action tdi_i(Bit#(1) tdi);
	(*always_enabled,always_ready*)
	method Action tck_i(Bit#(1) tck);
	(*always_enabled,always_ready*)
	method Action trst_i(Bit#(1) trst);
	/*==== inputs from Sub-modules === */
	method Action debug_tdi_i(Bit#(1) debug_tdi);
	method Action bs_chain_i(Bit#(1) bs_chain);
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
	module mkjtagdtm(Ifc_jtagdtm);
	Reg#(Bit#(1)) prv_clk <-mkReg(0);	
	PulseWire posedge_clk <-mkPulseWire();
	PulseWire negedge_clk <-mkPulseWire();
	/*========= FIFOs to communicate with the DM==== */
	FIFOF#(Bit#(40)) request_to_DM <-mkUGFIFOF1();
	FIFOF#(Bit#(34)) response_from_DM <-mkUGFIFOF1();
	/*================================================ */

	/*=== Wires to capture the input pins === */
	Wire#(Bit#(1)) wr_tms<-mkDWire(0);
	Wire#(Bit#(1)) wr_tdi<-mkDWire(0);
	Wire#(Bool) wr_trst<-mkDWire(False);
	Wire#(Bit#(1)) wr_debug_tdi<-mkDWire(0);
	Wire#(Bit#(1)) wr_bs_chain_tdi<-mkDWire(0);
	/*======================================== */
	
	Reg#(TapStates) tapstate<-mkRegA(TestLogicReset);
	Reg#(Bit#(5)) instruction_shiftreg<-mkRegA(0);
	Reg#(Bit#(5)) instruction<-mkRegA(`IDCODE); // clock this by the inverted clock
	Reg#(Bit#(1)) bypass_sr<-mkRegA(0);
	Reg#(Bit#(32)) idcode_sr<-mkRegA(`IDCODEVALUE);

	Wire#(Bool) wr_dmihardreset_generated<-mkDWire(False);
	Reg#(Bit#(1)) rg_dmihardreset<-mkRegA(0);
	Reg#(Bit#(1)) dmihardreset=condwriteSideEffect(rg_dmihardreset,wr_dmihardreset_generated._write(True));
	Wire#(Bool) wr_dmireset_generated<-mkDWire(False);
	Reg#(Bit#(1)) rg_dmireset<-mkRegA(0);
	Reg#(Bit#(1)) dmireset=condwriteSideEffect(rg_dmireset,wr_dmireset_generated._write(True));
	Reg#(Bit#(3)) idle=readOnlyReg(3'd7);
	Reg#(Bit#(2)) dmistat<-mkRegA(0);
	Reg#(Bit#(6)) abits =readOnlyReg(6'd6);
	Reg#(Bit#(4)) version = readOnlyReg('d1);
	Reg#(Bit#(32)) dtmcontrol=concatReg8(readOnlyReg(14'd0),
		dmihardreset,dmireset,readOnlyReg(1'd0),
		idle,dmistat,abits,version);
	Reg#(Bit#(32)) dtmcontrol_shiftreg<-mkRegA({17'd0,3'd2,2'd0,6'd6,4'd1});

	Reg#(Bit#(40)) dmiaccess_shiftreg[2]<-mkCReg(2,'d0);
	Reg#(Bit#(2)) response_status<-mkReg(0);
	Reg#(Bool) rg_dmibusy<-mkRegA(False);


   Bit#(1) bypass_sel		= instruction == `BYPASS?1:0;
   Bit#(1) idcode_sel		= instruction == `IDCODE?1:0;
   Bit#(1) dbg_sel			= instruction == `DEBUG?1:0;
   Bit#(1) dtmcontrol_sel	= instruction == `DTMCONTROL?1:0;
   Bit#(1) dmi_sel			= instruction == `DMIACCESS?1:0;

	Bit#(1) instruction_tdo=instruction_shiftreg[0];
	Bit#(1) bypass_tdo=bypass_sr;
	Bit#(1) idcode_tdo=idcode_sr[0];
	Bit#(1) dtmcontrol_tdo=dtmcontrol_shiftreg[0];
	Bit#(1) dmiaccess_tdo=dmiaccess_shiftreg[0][0];

	Reg#(Bit#(1)) rg_tdo<-mkRegA(0);

	/*== This rule implements the TAPs STATE MACHINE====== */
	`ifdef verbose
	rule just_display;
		$display($time,"\tTAPSTATE: ",fshow(tapstate),"\tINSTRUCTION: %h",instruction_shiftreg, " DMIACCESS: %h",dmiaccess_shiftreg[1]);
	endrule
	`endif

	rule reset_tap(wr_trst);
		tapstate<=TestLogicReset;
		instruction<=`IDCODE;
		instruction_shiftreg<=0; 
		dtmcontrol<=0;
		rg_dmibusy<=False;
		dmiaccess_shiftreg[1]<=0;
		dtmcontrol_shiftreg<=({17'd0,3'd2,2'd0,6'd6,4'd1});
		idcode_sr<=`IDCODEVALUE;
		bypass_sr<=0;
	endrule
	rule tap_state_machine(posedge_clk && !wr_trst);
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

	rule dmireset_generated(wr_dmireset_generated && !wr_trst);
		dmiaccess_shiftreg[1][1:0]<='d0;
		response_status<=0;
	endrule
	rule dmihardreset_generated(wr_dmihardreset_generated && !wr_trst);
		request_to_DM.deq;
		response_from_DM.deq;
		rg_dmibusy<=False;
	endrule

	/*======= perform dtmcontrol shifts ======== */
	rule shift_dtm(posedge_clk && !wr_trst);
		case(tapstate)
			TestLogicReset: dtmcontrol<={17'd0,idle,2'b0,abits,version};
			CaptureDR:	if(dtmcontrol_sel==1) dtmcontrol_shiftreg<=dtmcontrol;
			ShiftDR:		if(dtmcontrol_sel==1) dtmcontrol_shiftreg<={wr_tdi,dtmcontrol_shiftreg[31:1]};
			UpdateDR:	if(dtmcontrol_sel==1) dtmcontrol<=dtmcontrol_shiftreg;
		endcase
	endrule
	/*========================================== */
	/*======= perform dmiaccess shifts ======== */
	rule shift_dmiaccess(posedge_clk && !wr_dmihardreset_generated && !wr_trst && !wr_dmireset_generated);
		case(tapstate)
			TestLogicReset: dmiaccess_shiftreg[0]<='d0;
			CaptureDR:	if(dmi_sel==1) 
				if(response_from_DM.notEmpty)begin 
					let x=response_from_DM.first[33:0];
					$display($time,"\tDTM: Getting response: data %h op: %h",x[33:2],x[1:0]);
					x[1:0]=x[1:0]|response_status;// keeping the lower 2 bits sticky
					dmiaccess_shiftreg[0][33:0]<=x; 
					response_status<=x[1:0];
					response_from_DM.deq; 
					$display($time,"\tDTM: New DMIACCESS value: %h",x);
					rg_dmibusy<=False;
				end
			ShiftDR:		if(dmi_sel==1) dmiaccess_shiftreg[0]<={wr_tdi,dmiaccess_shiftreg[0][39:1]};
			UpdateDR:	if(dmi_sel==1) 
				if(request_to_DM.notFull && dmiaccess_shiftreg[0][1:0]!=0 && rg_dmibusy==False)begin
					request_to_DM.enq(dmiaccess_shiftreg[0]);
					rg_dmibusy<=True;
					$display($time,"\tDTM: Sending request to Debug: %h",dmiaccess_shiftreg[0]);
				end
				else if(rg_dmibusy) begin
					response_from_DM.enq('d3);
				end
		endcase
	endrule
	/*========================================== */

	/*== perform instruction register shifts === */
	rule shift_reg(posedge_clk && !wr_trst);
		case(tapstate)
			CaptureIR:	instruction_shiftreg<='b10101;
			ShiftIR  :	instruction_shiftreg<= {wr_tdi,instruction_shiftreg[4:1]};
		endcase
	endrule
	rule transfer_instruction_on_nedge(negedge_clk && !wr_trst); // TODO negedge here
		case(tapstate)
			TestLogicReset	:instruction<=`IDCODE;
			UpdateIR			:instruction<=instruction_shiftreg;
		endcase
	endrule

	/*==== Bypass Section === */
	rule bypass_logic(posedge_clk && !wr_trst);
		case(tapstate)
			TestLogicReset: bypass_sr<=1'b0;
			CaptureDR	  : if(bypass_sel==1) bypass_sr<=1'b0;
			ShiftDR		  : if(bypass_sel==1) bypass_sr<=wr_tdi;
		endcase
	endrule

	/*======= IDCODE section === */
	rule idcode_logic(posedge_clk && !wr_trst);
		case(tapstate)
			TestLogicReset:idcode_sr<=`IDCODEVALUE;
			CaptureDR:	if(idcode_sel==1) idcode_sr<=`IDCODEVALUE;
			ShiftDR :   if(idcode_sel==1) idcode_sr<={wr_tdi,idcode_sr[31:1]};
		endcase
	endrule

	rule generate_tdo_outputpin(negedge_clk && !wr_trst);
		if(tapstate==ShiftIR)
			rg_tdo<=instruction_tdo;
		else
			case(instruction)
				`IDCODE: rg_tdo<=idcode_tdo;
				`DEBUG : rg_tdo<=wr_debug_tdi;
				`EXTEST: rg_tdo<=wr_bs_chain_tdi;
				`SAMPLE_PRELOAD: rg_tdo<=wr_bs_chain_tdi;
				`BYPASS: rg_tdo<=bypass_tdo;
				`DTMCONTROL: rg_tdo<=dtmcontrol_tdo;
				`DMIACCESS: rg_tdo<=dmiaccess_tdo;
				default:	rg_tdo<=bypass_tdo;
			endcase
	endrule

	/*======== JTAG input pins ===== */
	method Action tms_i(Bit#(1) tms);
		wr_tms<=tms;
	endmethod
	method Action tdi_i(Bit#(1) tdi);
		wr_tdi<=tdi;
	endmethod
	method Action tck_i(Bit#(1) tck);
		prv_clk<=tck;
		if(prv_clk==0 && tck==1)
			posedge_clk.send;
		else if(prv_clk==1 && tck==0)
			negedge_clk.send;
	endmethod
	method Action trst_i(Bit#(1) trst);
		wr_trst<=unpack(trst);
	endmethod
	/*============================= */
	method Action debug_tdi_i(Bit#(1) debug_tdi);
		wr_debug_tdi<=debug_tdi;
	endmethod
	method Action bs_chain_i(Bit#(1) bs_chain);
		wr_bs_chain_tdi<=bs_chain;
	endmethod
	/*======== TAP States ============= */
	method shift_dr=tapstate==ShiftDR?1:0;
	method pause_dr=tapstate==PauseDR?1:0;
	method update_dr=tapstate==UpdateDR?1:0;
	method capture_dr=tapstate==CaptureDR?1:0;
	/*=================================== */
	/*=========== Output for BS Chain ==== */
	method extest_select				=instruction==`EXTEST?1:0;
	method sample_preload_select	=instruction==`SAMPLE_PRELOAD?1:0;
	method debug_select				=instruction==`DEBUG?1:0;
	/*================================ */
	/*======= JTAG Output Pins ====== */
	method tdo = rg_tdo;
	method debug_tdo = wr_tdi;
	method Bit#(1) tdo_oe = ((tapstate == ShiftIR) || (tapstate == ShiftDR))?1:0;
	method Action response_from_dm(Bit#(34) responsedm) if(response_from_DM.notFull);
		response_from_DM.enq(responsedm);
	endmethod
	method ActionValue#(Bit#(40)) request_to_dm if(request_to_DM.notEmpty);
		request_to_DM.deq;
		return request_to_DM.first;
	endmethod
	endmodule

endpackage
