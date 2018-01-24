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
package imem;

	/*========= package imports========== */
	import FIFOF::*;
	import SpecialFIFOs::*;
	import Connectable::*;
	import GetPut::*;
	/*==================================== */
	/* ======== project imports ========= */
	import TxRx	::*;
	import icache	::*;
	import defined_types::*;
	import MemoryMap::*;
	`ifdef bpu
		import branchpredictor::*;
	`endif
	`include "defined_parameters.bsv"
	`ifdef MMU import iTLB::*; `endif
	/* ================================== */

interface Ifc_imem;
	/*======= Mandatory Interface to the core ================ */
	interface Put#(Tuple5#(Bit#(2),Bit#(`VADDR),Bit#(`VADDR),Bool,Bit#(3))) request_from_core;
	method Maybe#(Tuple7#(Bit#(`VADDR),Bit#(2),Bit#(`VADDR),Bit#(32), Trap_type, Bit#(`PERFMONITORS), Bit#(3))) instruction_response_to_core;
	method Action flush(Flush_type _flush);
	method Bit#(`PERFMONITORS) imem_perfmon;
//	method Bool init_complete;
	method Action stall_fetch(Bool stall);
	/*=============================================== */
	/*====== Madatory Interface to the external Bus ======= */
	method Action response_from_memory(From_Memory#(`DCACHE_WORD_SIZE) mem_data);
	method ActionValue#(To_Memory#(`PADDR)) request_to_memory;
	/*============================================ */
	`ifdef bpu
		interface Get#(Tuple4#(Bit#(3),Bit#(`VADDR),Bit#(`VADDR),Bit#(2))) prediction_response;
		method Action training (Maybe#(Training_data#(`VADDR)) training_data);
		interface Put#(Tuple2#(Bit#(3),Bit#(`VADDR))) send_prediction_request;
	`endif
	`ifdef MMU
		method Action translation_protection_frm_csr(bit tlb_disable, Chmod per_bits, Bit#(TAdd#(4,`ASID)) asid);
		interface Get#(Request_PPN_PTW#(`VADDR,`OFFSET)) to_PTW; 
		interface Put#(Tuple2#(Bool,To_TLB#(`PADDR,`OFFSET,`ASID))) refill_TLB;
		method Action fence_itlb(Fence_VMA_type#(`VADDR) rsdata);
	`endif	
	`ifdef prefetch
		method Action prefetch();
	`endif
endinterface
(*synthesize*)
(*mutually_exclusive="request_from_core.put,send_cache_request_to_memory"*)
//(*mutually_exclusive="request_from_core.put,send_translated_vaddress"*)
module mkimem(Ifc_imem);
	Ifc_icache icache <- mkicache;
	Reg#(Bool) io_access_started<-mkReg(False);

	Wire#(Bit#(`VADDR)) wr_address_from_core <-mkWire();
	Wire#(Bool) wr_flush <-mkDWire(False);

	`ifdef bpu
		Ifc_branchpredictor bpu<-mkbranchpredictor();
	`endif

   
	Wire#(Maybe#(Tuple7#(Bit#(`VADDR),Bit#(2),Bit#(`VADDR), Bit#(32), Trap_type, Bit#(`PERFMONITORS),Bit#(3)))) wr_response_to_cpu<-mkDWire(tagged Invalid);
	FIFOF#(To_Memory#(`PADDR)) ff_request_to_memory <-mkSizedBypassFIFOF(1);
	Reg#(Trap_type)	rg_exception <- mkReg(tagged None);
	Reg#(Bit#(3)) epochs <-mkReg(0);
	Reg#(Bit#(`VADDR)) pc<-mkReg(0);
	Reg#(Bit#(`VADDR)) npc<-mkReg(0);
	Reg#(Bit#(2)) prediction <-mkReg(0);

	`ifdef MMU
		Ifc_iTLB itlb <- mkiTLB;
		rule send_translated_paddress(rg_exception matches tagged None);
			let x <- itlb.send_ppn;
			//if(x.exception matches tagged None) begin
				icache.physical_address(truncate(x.address),x.exception); //TODO vaddr
			//end
		endrule
	`endif	
	rule send_cache_request_to_memory;
		let x<-icache.request_to_memory;
		ff_request_to_memory.enq(x);
	endrule
	/*======= collect responses from the cache and store them in the FIFOs============ */

	// This rule collects the prediction information from the branchpredictor and stores in them 
	// FIFO. The request to the predictor is provided as part of the request to the cache itself.
	// This the branchpredictor will not work for io accesses.
	// This rule collects the instructions provided by the cache. The cache can provide a one or two instructions. If there is a hit
	// in the cache then a single instruction is provided. On a miss in the cache the instruction are supplied as part of the cache-line-fill
	// which is 2 instructions at a time. Whethere to enque one or two or no instructions into the core pipe is indicated by the variable
	// singledual. A value of 00 indicates that none of the instructions provided by the memory (as part of the line) need to be enqued
	// into the cache. A value of 01 means the lower instruction should be enqued, 'b10 means the upper instruction only needs to be enqued
	// and 'b11 means both the instructions need to be enqueud.
	rule collect_first_instruction_from_icache;
		let x=icache.response_to_core;
		if(x matches tagged Valid .resp)begin
			let {instr,trap,perf}=resp;
			wr_response_to_cpu<=tagged Valid tuple7(pc,prediction,npc,instr,trap,perf,epochs);
		end
	endrule
	/*======================================================================================= */


	/*==== prefetch should start as soon as the cache is idle and a previous line was a miss=== */
	`ifdef prefetch
		rule perform_prefetch;
			let x<-icache.prefetch;
			`ifdef MMU itlb.get_vaddr(signExtend(x)); `endif
		endrule
	`endif
	/*===================================================================== */


	/*===================== Interface to the Core ======== */
	interface request_from_core = interface Put
		method Action put(Tuple5#(Bit#(2),Bit#(`VADDR),Bit#(`VADDR),Bool,Bit#(3)) request);
			let {pred,nextpc,instr_addr,fence,epoch_req}=request;
			epochs<=epoch_req;
			pc<=instr_addr;
			prediction<=pred;
			npc<=nextpc;
			`ifdef verbose $display($time,"\tIMEM: Sending request to cache for address: %h prediction: %b",instr_addr,prediction); `endif
			`ifdef MMU 
				itlb.get_vaddr(signExtend(instr_addr)); 
			`endif
			icache.virtual_address(instr_addr,fence);
		endmethod
	endinterface;
	method Maybe#(Tuple7#(Bit#(`VADDR), Bit#(2),Bit#(`VADDR),Bit#(32), Trap_type, Bit#(`PERFMONITORS),Bit#(3))) instruction_response_to_core=wr_response_to_cpu;
	/*==================================================== */

	`ifdef bpu
		method Action training (Maybe#(Training_data#(`VADDR)) training_data)=bpu.training(training_data);
		interface send_prediction_request=bpu.send_prediction_request;
		interface prediction_response=bpu.prediction_response;
	`endif

	method Action flush(Flush_type _flush);
		`ifdef MMU
		`endif	
	endmethod
	method Bit#(`PERFMONITORS) imem_perfmon=icache.icache_perfmon;
	method Action stall_fetch(Bool stall) = icache.stall_fetch(stall);
	/*===================================================== */

	/*======= Interface to the external Memory =========== */
	method Action response_from_memory(From_Memory#(`DCACHE_WORD_SIZE) mem_data)=icache.response_from_memory(mem_data);
	method ActionValue#(To_Memory#(`PADDR)) request_to_memory;
		ff_request_to_memory.deq;
		return ff_request_to_memory.first;
	endmethod
	/*===================================================== */
	`ifdef MMU
		method Action translation_protection_frm_csr(bit tlb_disable, Chmod per_bits, Bit#(TAdd#(4,`ASID)) asid);
				itlb.translation_protection_frm_csr(tlb_disable, per_bits, asid);
		endmethod
		interface  to_PTW = itlb.to_PTW; 
		interface  refill_TLB = itlb.refill_TLB;
		method Action fence_itlb(Fence_VMA_type#(`VADDR) rsdata);
			itlb.fence_TLB(rsdata);
		endmethod
	`endif	
endmodule

endpackage
