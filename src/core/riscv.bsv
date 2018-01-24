/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author Names : Neel Gala
Email ID : neelgala@gmail.com

Description : 
This is the 64-bit core of the c_class processor. It containes rules for each stage. The description of each stage 
is given in the respective rules.
*/
package riscv;

/*===== Package imports === */
import SpecialFIFOs::*;
import FIFO::*;
import FIFOF::*;
import DReg::*;
import Vector ::*;
import TxRx::*;
import Connectable::*;
import GetPut::*;
/*========================= */

/*=== Project imports -===== */
import fetch_stage::*;
import decode_opfetch::*;
import execute_stage::*;
import memory_stage::*;
import csr::*;
`include "defined_parameters.bsv"
import defined_types::*;
/*========================= */

	interface Ifc_riscv;
		interface Get#(Tuple5#(Bit#(2),Bit#(`VADDR),Bit#(`VADDR),Bool,Bit#(3))) request_to_imem;
		method Action instruction_response_from_imem(Maybe#(Tuple7#(Bit#(`VADDR),Bit#(2),Bit#(`VADDR),Bit#(32), Trap_type, Bit#(`PERFMONITORS),Bit#(3))) x);
		interface Put#(Tuple4#(Bit#(3),Bit#(`VADDR),Bit#(`VADDR),Bit#(2))) prediction_response;
		interface Get#(Tuple2#(Bit#(3),Bit#(`VADDR))) send_prediction_request;
		method Maybe#(Training_data#(`VADDR)) training_data;
		interface Get#(Tuple2#(Memout,Bit#(1))) request_to_dmem;
		interface Put#(Maybe#(Tuple4#(Bit#(`Reg_width), Trap_type, Bit#(`PERFMONITORS),Bit#(1)))) response_from_dmem;
		method Bool flush_dmem;
		method Action set_external_interrupt(Tuple2#(Bool,Bool) i);
		`ifdef MMU
			method Bit#(`Reg_width) send_satp;
			method Chmod perm_to_TLB;
			method Bit#(`Reg_width) mmu_cache_disable;
			method Fence_VMA_type#(`VADDR) fence_tlbs;
		`endif
		(*always_ready,always_enabled*)
		method Action boot_sequence(Bit#(1) bootseq);
		/* =========================== Debug Interface ===================== */
		`ifdef Debug
			method Bit#(`Reg_width)			read_debug_igpr (Bit#(5) r);				 // Read a General-Purpose Register
			method Action write_debug_igpr (Bit#(5) r, Bit#(`Reg_width) d);				 // Write a General-Purpose Register
			method Bit#(`Reg_width)			read_debug_fgpr (Bit#(5) r);				 // Read a General-Purpose Register
			method Action write_debug_fgpr (Bit#(5) r, Bit#(`Reg_width) d);				 // Write a General-Purpose Register
			method Action reset;
			method Action						run_continue; 							 // Execute all instructions until the end of instruction stream
			method Bool							reset_complete;
   		method Action						stop;										 // Stop CPU
   		method Bool halted ();
   		method ActionValue#(Bit#(`Reg_width))			rw_csr (Bit#(12) r, Bool write, Bit#(`Reg_width) data);				 // Read a General-Purpose Register
		`endif
		`ifdef CLINT
			method Action clint_msip(Bit#(1) intrpt);
			method Action clint_mtip(Bit#(1) intrpt);
			method Action clint_mtime(Bit#(`Reg_width) c_mtime);
		`endif
		/*-========================================================================== */
	endinterface
	
	(*synthesize*)
	(*conflict_free="get_trap_data_from_csr,rl_write_back"*)
	module mkriscv#(Bit#(`VADDR) reset_vector)(Ifc_riscv);
	
		function Bool checkloadtrigger(TriggerData tdata, Bit#(`Reg_width) lsdata);
			if(tdata.ttype matches tagged Data .data)
				if(tdata.matchscheme==0 && data==lsdata)
					return True;
				else if(tdata.matchscheme==2 && data>=lsdata)
					return True;
				else if(tdata.matchscheme==3 && data<=lsdata)
					return True;
				else if(tdata.matchscheme==4 && data[31:0]==(data[63:32]&lsdata[31:0]))
					return True;
				else if(tdata.matchscheme==5 && data[31:0]==(data[63:32]&lsdata[63:32]))
					return True;
				else
					return False;
			else 
				return False;
		endfunction
 
		/* =================== PIPELINE FIFOS ======================================================================== */
		FIFOF#(IF_ID_type) ff_if_id <-mkSizedFIFOF(2); // instantiating ISB between fetch and decode
   	FIFOF#(ID_IE_type) ff_id_ie <-mkSizedFIFOF(2); // instantiating ISB between decode and exec
		FIFOF#(IE_IMEM_type) ff_ie_imem <-mkSizedFIFOF(2); // instantiating ISB between exec and memory
		FIFOF#(IMEM_IWB_type) ff_imem_iwb <-mkLFIFOF(); // instantiating ISB between memory and write-back
		Wire#(Maybe#(Operand_forwading_type)) wr_forward_from_WBS <-mkDWire(tagged Invalid);// holds the forwarded data from the memory stage
		Wire#(Maybe#(Tuple3#(Bit#(`Reg_width), Trap_type,Bit#(`PERFMONITORS)))) wr_response_to_cpu <- mkDWire(tagged Invalid);
		/*============================================================================================================*/

		/* ============================================ Flushing related regs/wires=================================== */
   	Reg#(Bool) wb_stage_flush <-mkDWire(False);	// if true inidicates that the entire pipe needs to be flushed
   	Reg#(Bool) dmem_flush <-mkDWire(False);	// if true inidicates that the entire pipe needs to be flushed
		Reg#(Bit#(`VADDR)) wr_effective_address1 <-mkDWire(0); // captures the new pc when an trap is taken.// captures the new pc when an trap is taken.
		Wire#(Bool) wr_change_wEpoch<-mkDWire(False);
		Wire#(Fence_VMA_type#(`VADDR)) wr_sfence	<- mkWire();
		Wire#(Bit#(`PERFMONITORS)) wr_dcache_perfmon <- mkDWire(0);
		Reg#(Bit#(1)) wEpoch <-mkReg(0);
		/*============================================================================================================*/
   
	 	/*========================================== Debug related registers/wires ================================== */
	 	Reg #(Bool)      rg_stop_requested[2] <- mkCReg(2, False);						// True if a stop was requested by the previous instruction
	 	Reg #(Bool)      rg_resume_requested[2] <- mkCReg(2, False);						// True if a stop was requested by the previous instruction
	 	Reg #(Bool)      rg_reset_requested[2] <- mkCReg(2, False);						// True if a stop was requested by the previous instruction
		/*============================================================================================================*/

		/* ================= Instantiating all the modules required in the pipe =================================== */
		Ifc_fetch fetch <-mkfetch(reset_vector);
		Ifc_decode_opfetch decode <-mkdecode_opfetch;
		Ifc_execute_stage execute_stage <-mkexecute_stage;
		Ifc_memory_stage memory_stage <-mkmemory_stage;
		Ifc_csr csr <-mkcsr();
		/*============================================================================================================*/

		/* ================== flushing the core partially or completely =================================================== */
		let {flush_from_execute,effective_address}=execute_stage.generate_flush; // capture the flush generated by the decode stage.

		/* this rule flushes the insruction fetch stage since the decode stage has generated a branch misprediction.
		The flush can also be generated is the instruction in the decode stage is a Fence.I operation. This difference
		in the misprediction and fence is captured in flush_from_decode wire.*/
		rule rl_flush_first_two_stages(flush_from_execute!=None && !wb_stage_flush);
			`ifdef verbose $display($time,"\tFlushing the fetch and Decode stage alone"); `endif
			fetch.flush(effective_address,flush_from_execute);
		endrule

		/*=== These rules are fired when an exception or interrupt or ECALL has been generated/taken at the write-back stage===*/
		rule rl_flush_fetch(wb_stage_flush);
			`ifdef verbose $display($time,"\tFLUSH FIRING TO ALL STAGES"); `endif
			fetch.flush(wr_effective_address1,AccessFlush);
			execute_stage.flush_prf;
		endrule
		rule rl_flush_decode_stage(wb_stage_flush||flush_from_execute!=None);
			decode.flush();
		endrule
		rule rl_change_eEpoch(flush_from_execute!=None && !wr_change_wEpoch);
				fetch.update_eEpoch;
				decode.update_eEpoch;
		endrule
		rule ras_push_connect;
			fetch.push_ras(execute_stage.ras_push);
		endrule
		rule rl_change_wEpoch(wr_change_wEpoch);
			fetch.update_wEpoch;
			decode.update_wEpoch;
			memory_stage.update_wEpoch;
			execute_stage.update_wEpoch;
		endrule
		/*=============================================================================================================*/

		/*============= Make PIPE Connections =========== */
		mkConnection(fetch.tx_out,ff_if_id);
		mkConnection(ff_if_id,decode.rx_in);

		mkConnection(decode.tx_out,ff_id_ie);
		mkConnection(ff_id_ie,execute_stage.rx_in);

		mkConnection(execute_stage.tx_out,ff_ie_imem);
		mkConnection(ff_ie_imem,memory_stage.rx_in);

		RX#(IMEM_IWB_type) rx <-mkRX();
		mkConnection(memory_stage.tx_out, ff_imem_iwb);
		mkConnection(ff_imem_iwb,rx.e);
		/*===================================================== */
		/* This rule connects the forwarding data from the memory and execution
		stages to the operand fetch stage */
		rule connect_forwarding_data1;
			execute_stage._forwarding_from_memory(memory_stage.forwarding_data); // forwarding from memory unit.
		endrule
		rule send_misa_to_decode;
			decode.misa(csr.misa);
		endrule
		rule get_trap_data_from_csr(ff_if_id.notEmpty);
			let {y,x}<-csr.check_for_trap(`ifdef Debug rg_stop_requested[1],rg_resume_requested[1],rg_reset_requested[1], `endif ff_if_id.first.program_counter,ff_if_id.first.instruction);
			decode.trap_from_csr(tuple2(y,x));
		endrule
		`ifdef Debug
		rule disable_debug_requests;
			if(csr.halted && rg_stop_requested[1])
				rg_stop_requested[1]<=False;
			if(!csr.halted && rg_resume_requested[1])
				rg_resume_requested[1]<=False;
			if(csr.reset_mode && rg_reset_requested[1])
				rg_reset_requested[1]<=False;
		endrule
		`endif
		/* This rule is used to transfer the updated value
		of the FCSR register to the floating point units */
		rule get_data_from_csr;
			execute_stage.roundingmode(csr.roundingmode);
		endrule
		`ifdef Debug
		rule connect_trigger_info_memorystage;
			memory_stage.storetrigger_info(csr.store_triggerdata);
			memory_stage.loadtrigger_info(csr.load_triggerdata);
		endrule
		`endif

		/* ==================================================================================================================================*/


		/* Modularizing this stage is too cumbersome since it will encapsulate the debug registers and the CSR
		which communicate with the rest of the pipe in adhoc fashion. */
		rule rl_write_back;
			WriteBackType info=rx.u.first().commit_data;
			Bool start_write=True;
			Bit#(`VADDR) memaddress=0;
			if(info matches tagged RESULT .arith_data)
				memaddress=truncate(arith_data.aluresult);
			let exception=rx.u.first.exception;
			Bit#(`PERFMONITORS) pm=rx.u.first.perfmonitors;
			/*========================================================= */
			`ifdef verbose $display($time,"\t*****************WRITE BACK STAGE*************************\t PC: %h PID: %d PERF: %h Instr-Epochs: %b Epochs: %b",rx.u.first.program_counter,rx.u.first.pid,pm,rx.u.first.epochs,wEpoch); `endif
			rx.u.deq(); // release the previous FIFO
			Bit#(3) debugcause=rx.u.first.debugcause;
			`ifdef Debug
			if(csr.step_now)begin
				exception=tagged Interrupt DebugInterrupt;
				debugcause=4;
			end
			`endif
			if(rx.u.first.epochs[0]!=wEpoch)begin
				`ifdef verbose $display($time,"\tWRITEBACK: Dropping instruction"); `endif
			end
			else if(exception matches tagged None)begin
				`ifdef MMU
				if(info matches tagged SYSTEM .priv) begin
					if(priv.csr_address[11:5]=='b0001001)
						wr_sfence <= Fence_VMA_type{rs1:truncate(priv.rs1),rs2:truncate(priv.rs2)};
				end
				`endif
				let {flush,ea,destination_value,commit}<-csr.system_instruction(info,rx.u.first.program_counter,pm `ifdef simulate ,rx.u.first.instruction,rx.u.first.rd_type,rx.u.first.destination `endif );
				if(commit)begin
					`ifdef verbose $display($time,"\tWRITEBACK: Writing into register: %d with value: %h",rx.u.first.destination,destination_value); `endif
					decode.write_rd(rx.u.first.destination,destination_value,rx.u.first.rd_type);
				end
				wb_stage_flush<=flush;
				if(info matches tagged SYSTEM .x)
					dmem_flush<=True;
				
				if(flush) begin
					wEpoch<=~wEpoch;
					wr_change_wEpoch<=True;
				end
				wr_effective_address1<=ea;
				`ifdef verbose $display($time,"\tWRITEBACK: Flush: ",fshow(flush)," EA: %h",ea); `endif
			end
			else begin 
				let {ea,flush}<-csr.take_trap(exception,debugcause,rx.u.first.program_counter,truncate(memaddress)); 
				`ifdef verbose  $display($time,"\tWRITEBACK: Taking trap ea: %h",ea); `endif
				if(flush) begin
					wEpoch<=~wEpoch;
					wr_change_wEpoch<=True;
				end
				if(exception matches tagged Interrupt .in &&& flush)
					dmem_flush<=True;
				if(exception matches tagged Exception .cause)begin
					let ex=pack(cause);
					if(ex!=4 && ex!=5 && ex!=6 && ex!=7 && ex!=13 && ex!=15)
						dmem_flush<=True;
				end
				wb_stage_flush<=flush;
				wr_effective_address1<=ea;
			end
		endrule
		/*====================================== END OF PIPE STAGES ============================================= */
		//////////////////////////// definition of methods /////////////////////////////////////////////////////////////////////////////////
		interface request_to_imem=fetch.request_to_imem;
		method Action instruction_response_from_imem(Maybe#(Tuple7#(Bit#(`VADDR),Bit#(2),Bit#(`VADDR),Bit#(32), Trap_type, Bit#(`PERFMONITORS),Bit#(3))) x)=fetch.instruction_response_from_imem(x);
		interface prediction_response =fetch.prediction_response;
		interface send_prediction_request=fetch.send_prediction_request;
		method training_data=execute_stage.training_data;
		interface request_to_dmem = execute_stage.to_dmem;
		interface response_from_dmem = memory_stage.response_from_dmem;
		method Bool flush_dmem;
			return dmem_flush;
		endmethod
		method Action set_external_interrupt(Tuple2#(Bool,Bool) i) = csr.set_external_interrupt(i);

		`ifdef MMU
		method Bit#(`Reg_width) send_satp = csr.send_satp;
		method Chmod perm_to_TLB = csr.perm_to_TLB;
		method Bit#(`Reg_width) mmu_cache_disable = csr.mmu_cache_disable;
		method Fence_VMA_type#(`VADDR) fence_tlbs;
			return wr_sfence;
		endmethod

		`endif
		
		//interface ifc_riscv_interrupt_pins = memory_stage.plic_ifc_external_irq;
		//method Bit#(TLog#(`INTERRUPT_PINS)) intrpt_completion;
		//	return memory_stage.plic_intrpt_completion;
		//endmethod
		/* ================================== Debug related methods ======================= */
   	`ifdef Debug 
			method Action reset;
				`ifdef verbose $display($time,"\tsetting the reset request"); `endif
				rg_reset_requested[0]<=True;
			endmethod
			method Action run_continue();
				`ifdef verbose $display($time,"\tsetting the resume reqiest to True"); `endif
				rg_resume_requested[0]<=True;
			endmethod
			method Bool reset_complete;
			 return !csr.reset_mode ;
			endmethod
   		method Action stop;
				`ifdef verbose $display($time,"RISCV: REQUESTING HALT"); `endif
				rg_stop_requested[0]<=True;
   		endmethod
   		method Bool halted ();
   		   return csr.halted;
   		endmethod
			method read_debug_igpr (Bit#(5) r) = decode.read_debug_igpr(r);				 // Read a General-Purpose Register
			method Action write_debug_igpr (Bit#(5) r, Bit#(`Reg_width) d)=decode.write_debug_igpr(r,d);				 // Write a General-Purpose Register
			method read_debug_fgpr (Bit#(5) r)=decode.read_debug_fgpr(r);				 // Read a General-Purpose Register
			method Action write_debug_fgpr (Bit#(5) r, Bit#(`Reg_width) d)=decode.write_debug_fgpr(r,d);				 // Write a General-Purpose Register
			method ActionValue#(Bit#(`Reg_width))	rw_csr (Bit#(12) r, Bool write, Bit#(`Reg_width) data) =csr.rw_debug_csr(r,write,data); // TODO
		`endif
		`ifdef CLINT
			method Action clint_msip(Bit#(1) intrpt)=csr.clint_msip(intrpt);
			method Action clint_mtip(Bit#(1) intrpt)=csr.clint_mtip(intrpt);
			method Action clint_mtime(Bit#(`Reg_width) c_mtime)=csr.clint_mtime(c_mtime);
		`endif
	 /* ==================================================================================== */
		method Action boot_sequence(Bit#(1) bootseq)=csr.boot_sequence(bootseq);
	endmodule
endpackage


