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
package core;

	/* ======== Package imports ======= */
	import Vector							:: *;
	import FIFO								:: *;
	import ConfigReg					::*;
	import Connectable 				:: *;
	/*================================== */

	/*========= Project imports ======== */
	`include "defined_parameters.bsv"
	import defined_types			::*;
	import Semi_FIFOF					::*;
	import AXI4_Types		:: *;
	import AXI4_Fabric		:: *;
	import riscv					:: *;
	import imem				::*;
	import dmem				::*;
	import GetPut			::*;
	import PTWalk			::*;
	/*================================== */


	interface Ifc_external_interrupt;
		method Action enqueueInterrupts;
	endinterface

	interface Ifc_core_AXI4;
		interface AXI4_Master_IFC#(`PADDR, `Reg_width, `USERSPACE) imem_master;
		interface AXI4_Master_IFC#(`PADDR, `Reg_width, `USERSPACE) dmem_master;
		method Action set_external_interrupt(Tuple2#(Bool,Bool) i);
		method Action boot_sequence(Bit#(1) bootseq);
		/* =========================== Debug Interface ===================== */
		`ifdef Debug
			method Action reset;
			method Action								run_continue; 							 // Execute all instructions until the end of instruction stream
			method Bool									reset_complete;
   		method Action								stop;										 // Stop CPU
   		method Bool halted ();
   		method Bit#(`Reg_width)			read_igpr (Bit#(5) r);				 // Read a General-Purpose Register
   		method Action								write_igpr (Bit#(5) r, Bit#(`Reg_width) d);	 // Write into a General-Purpose Register
			`ifdef spfpu
   		method Bit#(`Reg_width)			read_fgpr (Bit#(5) r);				 // Read a General-Purpose Register
   		method Action								write_fgpr (Bit#(5) r, Bit#(`Reg_width) d);	 // Write into a General-Purpose Register
			`endif
   		method ActionValue#(Bit#(`Reg_width))			rw_csr (Bit#(12) r, Bool write, Bit#(`Reg_width) data);				 // Read a General-Purpose Register
		`endif
		`ifdef CLINT
			method Action clint_msip(Bit#(1) intrpt);
			method Action clint_mtip(Bit#(1) intrpt);
			method Action clint_mtime(Bit#(`Reg_width) c_mtime);
		`endif
		/*-========================================================================== */
	endinterface

	typedef enum {Handling_Dcache,Handling_Icache,Idle,Handling_Uncacheable} Controller_State deriving (Bits, Eq, FShow);

	//(*mutually_exclusive="mkConnectionGetPut_7,mkConnectionGetPut_8")
	`ifdef MMU (*preempts="dtlb_to_ptw,itlb_to_ptw"*) `endif
	//`ifdef MMU (*preempts="mkConnectionGetPut_4,mkConnectionGetPut_3"*) `endif
	(*synthesize*)
	module mkcore_AXI4#(Bit#(`VADDR) reset_vector)(Ifc_core_AXI4);
	  	Ifc_riscv riscv <-mkriscv(reset_vector);
		AXI4_Master_Xactor_IFC #(`PADDR,`Reg_width,`USERSPACE) imem_xactor <- mkAXI4_Master_Xactor;
		AXI4_Master_Xactor_IFC #(`PADDR,`Reg_width,`USERSPACE) dmem_xactor <- mkAXI4_Master_Xactor;
		Ifc_imem imem <-mkimem();
		Ifc_dmem dmem <- mkdmem;
		Ifc_PTWalk#(`ADDR, `VADDR, 56, `PADDR, `ASID, `OFFSET) ptw <- mkPTWalk;
		Wire#(Bit#(`Reg_width)) wr_pte <- mkWire();
		Reg#(Bool) rg_serve_dTLB <- mkReg(False);

		mkConnection(riscv.request_to_imem,imem.request_from_core);
		mkConnection(imem.instruction_response_to_core,riscv.instruction_response_from_imem);
		mkConnection(riscv.request_to_dmem, dmem.request_from_cpu);
		mkConnection(dmem.response_to_cpu, riscv.response_from_dmem);
		`ifdef MMU
			rule itlb_to_ptw;
				let x <- imem.to_PTW.get;
				ptw.frm_TLB.put(x);
				rg_serve_dTLB <=False;
			endrule
			rule dtlb_to_ptw;
				let x <- dmem.to_PTW.get;
				ptw.frm_TLB.put(x);
				rg_serve_dTLB <=True;
			endrule
			rule ptw_to_itlb(!rg_serve_dTLB);
				let x <- ptw.to_TLB.get;
				imem.refill_TLB.put(x);
			endrule
			rule ptw_to_dtlb(rg_serve_dTLB);
				let x <- ptw.to_TLB.get;
				dmem.refill_TLB.put(x);
			endrule
			//mkConnection(imem.to_PTW, ptw.frm_TLB);
			//mkConnection(dmem.to_PTW, ptw.frm_TLB);
			//mkConnection(ptw.to_TLB, imem.refill_TLB);
			//mkConnection(ptw.to_TLB, dmem.refill_TLB);
		`endif
		mkConnection(imem.prediction_response,riscv.prediction_response);
		mkConnection(riscv.send_prediction_request,imem.send_prediction_request);
		rule connect_training;
			imem.training(riscv.training_data);
		endrule
//		(*conflict_free="connect_flush_to_imem,connect_flush_to_dmem"*)
//		rule connect_flush_to_imem;
//			Translation_type page_type = Execution;
//			if(riscv.flush_imem!=None) begin
//				ptw.flush(page_type);
//			end
//		endrule
		rule connect_flush_to_dmem;
			Translation_type page_type = Load;
			if(riscv.flush_dmem)
				dmem.flush();
		//	if(riscv.flush_dmem) begin
		//		ptw.flush(page_type);
		//	end
		endrule

		`ifdef MMU
			//rule fence_iTLB;
			//	`ifdef verbose $display($time ,"\tCORE: iTLB is being flushed"); `endif
			//	imem.fence_itlb(dmem.fence_itlb);
			//endrule
			rule send_permissions_to_tlb;
				ptw.satp_frm_csr(riscv.send_satp);
				imem.translation_protection_frm_csr(riscv.mmu_cache_disable[0],
																riscv.perm_to_TLB,
																riscv.send_satp[`Reg_width-1:`Reg_width-`ASID-4]);
				dmem.translation_protection_frm_csr(riscv.mmu_cache_disable[0],
																riscv.perm_to_TLB,
																riscv.send_satp[`Reg_width-1:`Reg_width-`ASID-4]);
			endrule
			rule send_pte_pointer;
				let x <- ptw.ifc_memory.send_PTE_pointer;
				dmem.get_pte_pointer(x);
			endrule	
			rule get_pte_entry;
				let x <- dmem.send_pte;
				wr_pte <= x;
			endrule	
			rule send_pte_entry;
				ptw.ifc_memory.get_PTE(wr_pte);
			endrule
			rule fence_tlbs;
				dmem.fence_dtlb(riscv.fence_tlbs);
				imem.fence_itlb(riscv.fence_tlbs);
			endrule
		`endif
		rule fence_stall_icache;
			imem.stall_fetch(dmem.stall_fetch);
		endrule
		Reg#(Bool) rg_update_a_bit <- mkReg(False);
		Reg#(Bool) rg_update_b_bit <- mkReg(False);
		Reg#(Maybe#(Bit#(TMul#(8,TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE))))) rg_data_line <-mkReg(tagged Invalid);
		Reg#(Bit#(8)) rg_burst_count<-mkReg(0);
		Reg#(Bool) rg_wait_for_response[2]<-mkCReg(2,False);
		rule check_read_request_to_memory_from_dcache;
			let info<-dmem.request_to_memory_read;
			Bit#(2) arburst=2;
			if(info.burst_length==1)
				arburst=1;
		 	let read_request = AXI4_Rd_Addr {araddr: truncate(info.address), aruser: 0, arlen: info.burst_length-1, arsize: zeroExtend(info.transfer_size), arburst: arburst, arid:'d0}; // arburst: 00-FIXED 01-INCR 10-WRAP
   	   		dmem_xactor.i_rd_addr.enq(read_request);	
			`ifdef verbose $display($time,"\tCORE: Sending Read Request from DCACHE for Address: %h Burst Length: %h",info.address,info.burst_length); `endif
		endrule
		rule check_write_request_to_memory_from_dcache(rg_data_line matches tagged Invalid &&& !rg_wait_for_response[1]);
			let info<-dmem.request_to_memory_write;
			/*=== Need to shift the data apprpriately while sending write requests===== */
			Bit#(`Reg_width) actual_data=info.data_line[`Reg_width-1:0];
			Bit#(8) write_strobe=info.transfer_size==0?8'b1:info.transfer_size==1?8'b11:info.transfer_size==2?8'hf:8'hff;
			if(info.transfer_size!=3)begin			// 8-bit write;
				write_strobe=write_strobe<<(info.address[`byte_offset:0]);
			end
//			info.address[2:0]=0; // also make the address 64-bit aligned
			/*========================================================================= */
			let aw = AXI4_Wr_Addr {awaddr: truncate(info.address), awuser:0, awlen: info.burst_length-1, awsize: zeroExtend(info.transfer_size), awburst: 'b01, awid:'d0}; // arburst: 00-FIXED 01-INCR 10-WRAP
			let w  = AXI4_Wr_Data {wdata:  actual_data, wstrb: write_strobe, wlast:info.burst_length>1?False:True, wid:'d0};
			dmem_xactor.i_wr_addr.enq(aw);
			dmem_xactor.i_wr_data.enq(w);
	 	  	`ifdef verbose $display($time,"\tCORE: Sending Write Request from DCACHE for  Address: %h BurstLength: %h Data: %h WriteStrobe: %b",info.address,info.burst_length,info.data_line, write_strobe); `endif
			if(info.burst_length>1)begin // only enable the next rule when doing a line write in burst mode.
			rg_data_line<=tagged Valid (info.data_line>>`Reg_width);
			rg_burst_count<=rg_burst_count+1;
			end
			rg_wait_for_response[1]<=True;
		endrule
		rule send_burst_write_data(rg_data_line matches tagged Valid .data_line);
			/*==  Since this is going to always be a line write request in burst mode No need of shifting data and address=== */
			let w  = AXI4_Wr_Data {wdata:  truncate(data_line), wstrb: 8'hff , wlast:(rg_burst_count==`DCACHE_BLOCK_SIZE-1), wid:'d0};
			dmem_xactor.i_wr_data.enq(w);
			`ifdef verbose $display($time,"\tCORE: Sending DCACHE Write Data: %h Burst: %d",data_line,rg_burst_count); `endif
			if(rg_burst_count==`DCACHE_BLOCK_SIZE-1)begin
				rg_burst_count<=0;
				rg_data_line<=tagged Invalid;
			end
			else begin
				rg_data_line<=tagged Valid (data_line>>`Reg_width);
				rg_burst_count<=rg_burst_count+1;
			end
		endrule
		rule check_read_request_to_memory_from_icache;
			let info <-imem.request_to_memory;
			let read_request = AXI4_Rd_Addr {araddr: truncate(info.address), aruser: 0, arlen: info.burst_length-1, arsize: info.transfer_size, arburst: 'b10, arid:'d1}; // arburst: 00-FIXED 01-INCR 10-WRAP
			imem_xactor.i_rd_addr.enq(read_request);	
			`ifdef verbose $display($time,"\tCORE: Sending Read Request from ICACHE for Address: %h Burst Length: %h",info.address,info.burst_length); `endif
		endrule
		rule send_read_response_from_memory_to_dcache(dmem_xactor.o_rd_data.first.rid == 'd0); 
			let response <- pop_o (dmem_xactor.o_rd_data);	
			let bus_error_from_memory = (response.rresp==AXI4_OKAY) ? 0 : 1;
			`ifdef verbose $display($time,"\tCORE: Sending Response to DCACHE: ",fshow(response)); `endif
			dmem.response_from_memory_read(From_Memory{data_line:response.rdata,bus_error:bus_error_from_memory,last_word:response.rlast});
		endrule
		rule send_read_response_from_memory_to_icache(imem_xactor.o_rd_data.first.rid == 'd1); 
			let response <- pop_o (imem_xactor.o_rd_data);	
			let bus_error_from_memory = (response.rresp==AXI4_OKAY) ? 0 : 1;
			`ifdef verbose $display($time,"\tCORE: Sending Response to ICACHE: ",fshow(response)); `endif
			imem.response_from_memory(From_Memory{data_line:response.rdata,bus_error:bus_error_from_memory, last_word:response.rlast});
		endrule
		rule send_write_response_to_dcache(rg_wait_for_response[0] && dmem_xactor.o_wr_resp.first.bid == 'd0);
			let response<-pop_o(dmem_xactor.o_wr_resp);
	 	  	let bus_error_from_memory = (response.bresp==AXI4_OKAY) ? 0 : 1;
			`ifdef verbose $display($time,"\tCORE: Received Write Response:",fshow(response)); `endif
			dmem.response_from_memory_write(From_Memory{data_line:0,bus_error:bus_error_from_memory,last_word:True});
			rg_wait_for_response[0]<=False;
		endrule
		
		interface imem_master = imem_xactor.axi_side;
		interface dmem_master = dmem_xactor.axi_side;
		`ifdef Debug
			method run_continue=riscv.run_continue;
			method reset_complete=riscv.reset_complete;
			method stop=riscv.stop;
			method halted=riscv.halted;
			method Bit#(`Reg_width)read_igpr(Bit#(5) r);
				return riscv.read_debug_igpr(r);
			endmethod
			method Action write_igpr(Bit#(5) r, Bit#(`Reg_width)d);
				riscv.write_debug_igpr(r,d);
			endmethod
			`ifdef spfpu
			method Bit#(`Reg_width) read_fgpr(Bit#(5) r);
				return riscv.read_debug_fgpr(r);
			endmethod
			method Action write_fgpr(Bit#(5) r, Bit#(`Reg_width)d);
				riscv.write_debug_fgpr(r,d);
			endmethod
			`endif
   		method ActionValue#(Bit#(`Reg_width))	rw_csr (Bit#(12) r, Bool write, Bit#(`Reg_width) data)=riscv.rw_csr(r,write,data);
			method Action reset=riscv.reset;
		`endif
		method Action boot_sequence(Bit#(1) bootseq)=riscv.boot_sequence(bootseq);
		method Action set_external_interrupt(Tuple2#(Bool,Bool) i)=riscv.set_external_interrupt(i);
		`ifdef CLINT
			method Action clint_msip(Bit#(1) intrpt)=riscv.clint_msip(intrpt);
			method Action clint_mtip(Bit#(1) intrpt)=riscv.clint_mtip(intrpt);
			method Action clint_mtime(Bit#(`Reg_width) c_mtime)=riscv.clint_mtime(c_mtime);
		`endif
	endmodule
endpackage
