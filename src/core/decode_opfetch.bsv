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
package decode_opfetch;
	/*============= package imports ========== */
	import FIFOF::*;
	import TxRx:: *;
	import DReg::*;
	/* ======================================= */

	/* ============== project imports ======= */
	import registerfile::*;
	import decoder::*;
	import defined_types::*;
	`include "defined_parameters.bsv"
	/* ======================================= */

	interface Ifc_decode_opfetch;
		method Action write_rd (Bit#(5)r, Bit#(`Reg_width) d, Operand_type rdtype);
		/* ====================== pipe connections ========= */
		interface RXe#(IF_ID_type) rx_in;
		interface TXe#(ID_IE_type) tx_out;
		/*================================================== */
		`ifdef Debug
			method Bit#(`Reg_width)			read_debug_igpr (Bit#(5) r);				 // Read a General-Purpose Register
			method Action write_debug_igpr (Bit#(5) r, Bit#(`Reg_width) d);				 // Write a General-Purpose Register
			method Bit#(`Reg_width)			read_debug_fgpr (Bit#(5) r);				 // Read a General-Purpose Register
			method Action write_debug_fgpr (Bit#(5) r, Bit#(`Reg_width) d);				 // Write a General-Purpose Register
		`endif
		method Action flush();
		method Action trap_from_csr(Tuple2#(Bit#(3),Trap_type) tt);
		method Action misa(Bit#(`Reg_width) val);
		method Action update_eEpoch;
		method Action update_wEpoch;
	endinterface:Ifc_decode_opfetch

		function Bool isNone(Trap_type trap);
			if(trap matches tagged None)
				return True;
			else 
				return False;
		endfunction

	(*synthesize*)
	module mkdecode_opfetch(Ifc_decode_opfetch);
		Reg#(Bit#(`PERFMONITORS)) rg_decode_perfmon<-mkDReg(0);
		Wire#(Tuple2#(Bit#(3),Trap_type)) wr_trap_type<-mkDWire(tuple2(0,tagged None));
		Wire#(Bit#(`Reg_width)) wr_misa<-mkWire();
		Reg#(Bit#(1)) eEpoch <-mkReg(0);
		Reg#(Bit#(1)) wEpoch <-mkReg(0);
		// this is used to ensure that when a trap is 
		// taken no other instruction fills the pipe unless wb generates a flush. Stores are avoided using this mechanism
		Reg#(Bool) rg_flush_ahead <-mkReg(False); 
																

		Ifc_registerfile registerfile <-mkregisterfile();
		RX#(IF_ID_type) rx <-mkRX;
		TX#(ID_IE_type) tx <-mkTX;
		/*=================================== Decode and Operand Fetch ======================================================================*/
    // This rule decodes the instruction and provides necessary info for the execution units.
		rule rl_operand_fetch(rx.u.notEmpty && tx.u.notFull && !rg_flush_ahead);
			`ifdef verbose
				$display($time,"\t********** DECODE STAGE FIRING ************ PC: %h EPOCHS: %b Instr-EPOCHS: %b",rx.u.first.program_counter,{eEpoch,wEpoch}, rx.u.first.epochs)  ;
			`endif
			if({eEpoch,wEpoch}!=rx.u.first.epochs)begin
				`ifdef verbose $display($time,"\tDECODE: PC: %h Dropping Instruction since EPOCSH do not match",rx.u.first.program_counter); `endif
				rx.u.deq();
			end
			else begin
				let x = fn_decode(rx.u.first().instruction,rx.u.first.exception, wr_misa, rx.u.first.perfmonitors);
				let pc=rx.u.first.program_counter;
				let dest=x.rd;
				let {debugcause,csr_ex}=wr_trap_type;	
				Bit#(`PERFMONITORS) perfmonitor_incr=x.perf;
       			Trap_type exception=x.exception;
                Bool trap_on_wfi = False;
				if(exception matches tagged None) 
					exception = csr_ex;
				Bool dnwfi=True;
                if(x.immediate_value[2:0]=='b101 && x.immediate_value[5]==0 && x.funct3==0 && x.inst_type==SYSTEM_INSTR) begin
					dnwfi=False;
                    trap_on_wfi = True;
                end
				if(exception matches tagged Interrupt .i) 
					dnwfi=True;

                if(trap_on_wfi && dnwfi)
                    pc=pc+4;

				Bit#(`VADDR) nextpc=rx.u.first.nextpc;

				if(x.inst_type==NOP)begin
					`ifdef verbose $display($time,"DECODE: NOP Instruction"); `endif
					rx.u.deq();	
				end
				else begin
					Bool choose_rs3=`ifdef spfpu ( `ifdef dpfpu x.inst_type==DFLOATING || `endif x.inst_type==FLOATING) && (rx.u.first.instruction[6:4]=='b100) `else False `endif ;
					let operands<- registerfile._inputs_from_decode_stage(x.rs1,x.rs1type,x.rs2,x.rs2type,pc,x.immediate_value  `ifdef spfpu ,choose_rs3, x.rs3 `endif );
					if(dnwfi)begin
						Bool e = isNone(exception);
						if(!e || x.inst_type==SYSTEM_INSTR)
							rg_flush_ahead<=True;
						rx.u.deq();
						tx.u.enq(ID_IE_type{
							rs1:operands.rs1,
							rs2:(x.inst_type==MEMORY && (x.mem_access!=Load))?x.immediate_value:operands.rs2,
							rs3_imm:`ifdef spfpu (choose_rs3)?operands.rs3: `endif (x.inst_type==MEMORY && x.mem_access!=Load)?operands.rs2:x.immediate_value,
							rdtype:x.rdtype,
							inst_type:x.inst_type,
							destination:x.rd,
							program_counter:pc,
							exception:exception,
							fn:x.fn,
							mem_access:x.mem_access,
							word32:x.word32,
							funct3:x.funct3,
							nextpc:rx.u.first.nextpc,
							debugcause:debugcause,
							perfmonitors:perfmonitor_incr,
							prediction:rx.u.first.prediction,
							epochs:rx.u.first.epochs,
							rs1_type:x.rs1type,
							rs2_type:(x.inst_type==MEMORY && (x.mem_access!=Load))?Immediate:x.rs2type,
							rs3_type:(x.inst_type==MEMORY && (x.mem_access!=Load))?x.rs2type:`ifdef spfpu choose_rs3?FloatingRF: `endif Immediate,
							rs1addr:x.rs1,
							rs2addr:x.rs2,
							rs3addr:(x.inst_type==MEMORY && (x.mem_access!=Load))?x.rs2:choose_rs3?x.rs3:0
							`ifdef simulate ,instruction:rx.u.first.instruction `endif });
					end
					else begin
						`ifdef verbose $display($time,"\tWaiting for interrupt"); `endif
					end
					`ifdef verbose
				 	$display($time,"\tDECODE:	Instruction : %h",rx.u.first().instruction," ",fshow(x.inst_type)," FN: %b",x.fn," ",fshow(x.mem_access)); 
					$display($time,"\tRs1: %d",x.rs1," ",fshow(x.rs1type));
				 	$display($time,"\tRs2: %d",x.rs2," ",fshow(x.rs2type)); 
				 	`ifdef spfpu   $display($time,"\tRs3: %d",x.rs3); `endif
				 	$display($time,"\tRd: %d",x.rd," ",fshow(x.rdtype)); 
				 	$display($time,"\tImmediate Value: %h",x.immediate_value); 
				 	$display($time,"\tException: ",fshow(exception)); 
					$display($time,"\t*****************************************************");
					`endif
				end
			end
	   endrule
		/* ============================== method and interface definitions ========================= */
		method tx_out=tx.e;
		method rx_in=rx.e;
		method Action write_rd (Bit#(5)r, Bit#(`Reg_width) d, Operand_type rdtype)=registerfile.write_rd(r,d,rdtype);
		method Action flush();
			`ifdef verbose $display($time,"\tDECODE: Flushing"); `endif
			rg_flush_ahead<=False;
		endmethod
		method Action trap_from_csr(Tuple2#(Bit#(3),Trap_type) tt);
			wr_trap_type<=tt;
		endmethod
		`ifdef Debug
			method read_debug_igpr (Bit#(5) r) = registerfile.read_debug_igpr(r);				 // Read a General-Purpose Register
			method Action write_debug_igpr (Bit#(5) r, Bit#(`Reg_width) d)=registerfile.write_debug_igpr(r,d);				 // Write a General-Purpose Register
			method read_debug_fgpr (Bit#(5) r)=registerfile.read_debug_fgpr(r);				 // Read a General-Purpose Register
			method Action write_debug_fgpr (Bit#(5) r, Bit#(`Reg_width) d)=registerfile.write_debug_fgpr(r,d);				 // Write a General-Purpose Register
		`endif
		method Action misa(Bit#(`Reg_width) val);
			wr_misa<=val;
		endmethod
		method Action update_eEpoch;
			`ifdef verbose $display($time,"\tDECODE: updating eEpoch"); `endif
			eEpoch<=~eEpoch;
		endmethod
		method Action update_wEpoch;
			`ifdef verbose $display($time,"\tDECODE: updating wEpoch"); `endif
			wEpoch<=~wEpoch;
		endmethod
//		method init_complete=registerfile.init_complete;
	endmodule
endpackage:decode_opfetch
