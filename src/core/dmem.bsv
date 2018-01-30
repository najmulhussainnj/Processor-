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
package dmem;

import defined_types :: *;
import dTLB					 :: *;
import dcache_asic	 :: *;
import GetPut				 :: *;
import ConfigReg		 :: *;
import FIFOF				 :: *;
import SpecialFIFOs	 :: *;
import MemoryMap		 :: *;
import DReg::*;
import ConfigReg::*;
`include "defined_parameters.bsv"

interface Ifc_dmem;
	/*======= Mandatory Interface to the core ================ */
	interface Put#(Tuple2#(Memout,Bit#(1))) request_from_cpu;
	interface Get#(Maybe#(Tuple4#(Bit#(`Reg_width), Trap_type, Bit#(`PERFMONITORS),Bit#(1)))) response_to_cpu;
	method Action flush( );
//	method Bool reset_complete;
	method Bool stall_fetch;
	/*=============================================== */
	/*======= Mandatory Interface to the external bus ================ */
  method ActionValue#(To_Memory#(`PADDR)) request_to_memory_read;
  method ActionValue#(To_Memory_Write) request_to_memory_write;
  method Action response_from_memory_read(From_Memory#(`DCACHE_WORD_SIZE) resp);
  method Action response_from_memory_write(From_Memory#(`DCACHE_WORD_SIZE) resp);
	/*=============================================== */
	/*======= Interface to the DTLB ================ */
	`ifdef MMU
		method Action translation_protection_frm_csr(bit tlb_disable, Chmod per_bits, Bit#(TAdd#(4,`ASID)) asid);
		interface Get#(Request_PPN_PTW#(`VADDR,`OFFSET)) to_PTW; 
		interface Put#(Tuple2#(Bool,To_TLB#(`PADDR,`OFFSET,`ASID))) refill_TLB;
		method Action get_pte_pointer(Request_PTE_memory#(`Reg_width) pte);
		method Action fence_dtlb(Fence_VMA_type#(`VADDR) rsdata);						
		method ActionValue#(Bit#(`Reg_width)) send_pte;
	//method Action fence_TLB(Fence_VMA_type#(`ADDR) rsdata);
	`endif
	/*=============================================== */
endinterface
//(*conflict_free="request_from_cpu_put, response_to_cpu_get"*)
(*synthesize*)
module mkdmem(Ifc_dmem);
	Ifc_dcache dcache <- mkdcache;
	`ifdef MMU
		Ifc_dTLB dtlb <- mkdTLB;
		Reg#(Maybe#(Tuple2#(Memout,Bit#(1)))) rg_dtlb_metadata[2] <- mkCReg(2,tagged Invalid);
	`endif	
	ConfigReg#(Bool) rg_serve_ptw <- mkConfigReg(False);
	Reg#(Maybe#(Request_PTE_memory#(`Reg_width))) rg_pte_pointer[2] <- mkCReg(2,tagged Invalid);
	FIFOF#(Maybe#(Tuple2#(Bit#(`Reg_width), Maybe#(Exception_cause)))) ff_response_to_cpu <- mkSizedBypassFIFOF(1);
	Wire#(From_Memory#(`DCACHE_WORD_SIZE)) wr_read_response_from_memory <- mkWire(); 
	Reg#(Maybe#(Tuple4#(Bit#(`Reg_width), Trap_type,Bit#(`PERFMONITORS),Bit#(1)))) wr_response_to_cpu[3] <-mkCReg(3,tagged Invalid);
	Reg#(Bit#(1)) epochs[2] <-mkCReg(2,0);
	Reg#(Bit#(1)) wb_epochs[2] <-mkCReg(2,0);
	Reg#(Bool) request_taken <-mkDReg(False);
	Reg#(Bool) drop_request[2] <-mkCReg(2,False);
	rule display_stuff;
		`ifdef verbose $display($time,"\tDMEM: request_taken: %b drop_request: %b",request_taken,drop_request[0]); `endif
	endrule
	`ifdef MMU 
		(*conflict_free="ptw_to_dcache, send_translated_address"*)
		(*conflict_free="send_translated_address,request_from_cpu.put"*)
		(*conflict_free="send_translated_address,send_cache_index"*)
		rule send_translated_address(rg_dtlb_metadata[1] matches tagged Valid .z);
			rg_dtlb_metadata[1] <= tagged Invalid;
			let x <- dtlb.send_ppn;
			//From_Cpu_D#(`Reg_width,`DCACHE_WORD_SIZE) z = rg_dtlb_metadata;
			let {y,epoch} = z;
			y.address = x.address;
			`ifdef verbose $display($time,"\tDMEM: physical address %h to DCACHE and is cacheable %b drop_request: %b", x.address, x.cacheable,drop_request[1]); `endif
				dcache.physical_address(truncate(y.address), x.exception);//, y.mem_type, y.memory_data, y.transfer_size, y.atomic_op, unpack(y.signextend));
		endrule

		rule send_cache_index(rg_dtlb_metadata[1] matches tagged Valid .t);
			let {z,epoch} = t;
			let x <- dtlb.send_vaddress_for_cache_index;
				dcache.virtual_address(x, z.mem_type, z.memory_data, z.transfer_size, `ifdef atomic z.atomic_op, `endif unpack(z.signextend),epoch);
		endrule

		rule ptw_to_dcache(rg_pte_pointer[1] matches tagged Valid .ptw_request);
			`ifdef verbose $display($time,"\tDMEM: ptw request to DCACHE for address %h epochs: %b", ptw_request.address,epochs[1]); `endif
			rg_serve_ptw <= True;
			Bit#(`VADDR) addr = truncate(ptw_request.address);
			Access_type page_access_type = Load;
			Bit#(TMul#(`DCACHE_WORD_SIZE,8)) data = 0;
			dcache.virtual_address(addr,Load, data, 'd3 `ifdef atomic , 5'b00100 `endif , True,epochs[1]);
			dcache.physical_address(truncate(addr), tagged None);
			rg_pte_pointer[1] <= tagged Invalid;
		endrule

	`endif	

		rule send_response_to_core;
			wr_response_to_cpu[0] <= dcache.response_to_core;
		endrule

	/*======= Mandatory Interface to the core ================ */
	interface request_from_cpu = interface Put
		method Action put(Tuple2#(Memout,Bit#(1)) request) ;
				let {req,epoch}=request;
					`ifdef MMU 
					rg_dtlb_metadata[0] <= tagged Valid (tuple2(req,epoch));
					dtlb.get_vaddr(DTLB_access{vaddr : req.address, ld_st_atomic : req.mem_type} `ifdef atomic ,req.atomic_op `endif ); 
				`endif
				dcache.virtual_address(truncate(req.address),req.mem_type, req.memory_data, req.transfer_size, `ifdef atomic  req.atomic_op, `endif unpack(req.signextend),epoch);
				`ifdef verbose $display($time,"\tDMEM: Taking request from CPU: ",fshow(request)); `endif
				request_taken<=True;
		endmethod
	endinterface;

	interface response_to_cpu = interface Get
		method ActionValue#(Maybe#(Tuple4#(Bit#(`Reg_width), Trap_type,Bit#(`PERFMONITORS),Bit#(1)))) get if(!rg_serve_ptw); 
			Maybe#(Tuple4#(Bit#(`Reg_width), Trap_type,Bit#(`PERFMONITORS),Bit#(1))) response=tagged Invalid;
			if(wr_response_to_cpu[2] matches tagged Valid .resp)begin
				let {x,trap,y,epoch}=resp;
				response=tagged Valid tuple4(x,trap,y,epoch);
				if(trap matches tagged None)begin
				end
				else
					epochs[0]<=~epochs[0];
			end
			return response;
		endmethod
	endinterface;

	method Action flush();
		dcache.flush_from_wb;
		epochs[1]<=~epochs[1];
	endmethod


	method Bool stall_fetch =!dcache.init_complete;
	/*=============================================== */
	
	/*======= Mandatory Interface to the external bus ================ */
  method ActionValue#(To_Memory#(`PADDR)) request_to_memory_read=dcache.read_request_to_memory;
  method ActionValue#(To_Memory_Write) request_to_memory_write=dcache.write_request_to_memory; 
  method Action response_from_memory_read(From_Memory#(`DCACHE_WORD_SIZE) resp)=dcache.read_response_from_memory(resp); 
  method Action response_from_memory_write(From_Memory#(`DCACHE_WORD_SIZE) resp)=dcache.write_response_from_memory(resp);
	/*=============================================== */
	/*======= Interface to the DTLB ================ */
	`ifdef MMU
		method Action translation_protection_frm_csr(bit tlb_disable, Chmod per_bits, Bit#(TAdd#(4,`ASID)) asid);
		  dtlb.translation_protection_frm_csr(tlb_disable, per_bits, asid);
		endmethod
		interface  to_PTW = dtlb.to_PTW; 
		interface  refill_TLB = dtlb.refill_TLB;
		method Action get_pte_pointer(Request_PTE_memory#(`Reg_width) pte); 
			rg_pte_pointer[0] <= tagged Valid pte;
		endmethod
		method Action fence_dtlb(Fence_VMA_type#(`VADDR) rsdata)  = dtlb.fence_TLB(rsdata);
		method ActionValue#(Bit#(`Reg_width)) send_pte if(isValid(dcache.response_to_core) && rg_serve_ptw);
			rg_serve_ptw <= False;
		  Bit#(`Reg_width) data = 0;
			if(dcache.response_to_core matches tagged Valid .resp) begin
				let {x,y,perf,epoch} = resp;
				data = x;
		  end
			return data;
		endmethod
	`endif
	/*=============================================== */
endmodule

endpackage
