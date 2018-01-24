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
package iTLB;
import defined_types::*;
import FIFO::*;
import SpecialFIFOs::*;
import GetPut::*;
import ConfigReg::*;
import MemoryMap:: *;

`include "defined_parameters.bsv"

`define TLB_entries	16	
	
interface Ifc_TLB#(numeric type data_width, numeric type vaddr, numeric type paddr, numeric type page_size, numeric type asid_width);
	method Action get_vaddr(Bit#(data_width) addr);
	method ActionValue#(From_TLB#(data_width)) send_ppn;
	//method Bit#(vaddr) send_vaddress_for_cache_index;
	method Action translation_protection_frm_csr(bit tlb_disable, Chmod per_bits, Bit#(TAdd#(4,asid_width)) asid);
	interface Get#(Request_PPN_PTW#(vaddr,page_size)) to_PTW; 
	interface Put#(Tuple2#(Bool,To_TLB#(paddr,page_size,asid_width))) refill_TLB;
	method Action fence_TLB(Fence_VMA_type#(vaddr) rsdata);
	//method ActionValue#(Bool) page_fault;
	//method Action page_fault_frm_PTW;
endinterface

module mkTLB(Ifc_TLB#(data_width,vaddr,paddr,page_size,asid_width))
provisos( Add#(vpn, page_size, vaddr),
					Mul#(8, num_bytes, data_width),
					Log#(num_bytes, byte_addressable_bits),
					Add#(vpn_split,byte_addressable_bits, page_size),
					Mul#(2,vpn_split,intermediate1),
					Mul#(3,vpn_split,intermediate2),
					Add#(a_, paddr, data_width),
					Add#(b_, vaddr, data_width),
					Add#(c_, vpn_split, vpn),
					Add#(d_, intermediate1, vpn),
					Add#(e_, intermediate2, vpn),
					Add#(ppn, page_size, paddr));
	
	let v_vaddr = valueOf(vaddr);
	let v_vpn = valueOf(vpn);
	let v_ppn = valueOf(ppn);
	let v_page_offset = valueOf(page_size);
	let v_asid_width = valueOf(asid_width);
	let v_vpn_split = valueOf(vpn_split);
	let v_intermediate1 = valueOf(intermediate1);

	Reg#(Bit#(vpn)) tlb_vpn[`TLB_entries];
	Reg#(Bit#(ppn)) tlb_ppn[`TLB_entries];
	Reg#(TLB_permissions) tlb_permissions[`TLB_entries];
	Reg#(Bit#(asid_width)) tlb_asid[`TLB_entries];
	Reg#(Bool)				tlb_cacheable[`TLB_entries];
	Reg#(Bit#(2)) tlb_levels[`TLB_entries];
		for(Integer i = 0; i < `TLB_entries; i=i+1) begin
			tlb_vpn[i] <- mkReg(0);
			tlb_ppn[i] <- mkReg(0);
			tlb_permissions[i] <- mkReg(TLB_permissions{v:0,r:0,w:0,x:0,u:0,g:0,a:0,d:0});
			tlb_levels[i] <- mkReg(0);
			tlb_cacheable[i] <- mkReg(True);
		end
	FIFO#(Bit#(vpn)) ff_vpn <- mkBypassFIFO();
	FIFO#(Bit#(page_size)) ff_page_offset <- mkBypassFIFO();
	Reg#(Chmod) rg_chmod[2] <- mkCReg(2,Chmod { mprv : 0, mxr : 0, sum : 0, mpp : unpack(0), prv : unpack(0)});
	Reg#(Bool) rg_page_fault[2] <- mkCReg(2,False);
	Reg#(Bool) rg_hit[2] <- mkCReg(2,False);
	Reg#(Bit#(2)) rg_levels[2] <- mkCReg(2,0);
	Reg#(Bool) rg_handling_PTW[2] <- mkCReg(2,False);
	Reg#(Bool) rg_tlb_disable <- mkConfigReg(False);
	//Reg#(Bool) rg_frm_ptw[2] <- mkCReg(2,False);
	Reg#(Bit#(ppn)) rg_ppn[2] <- mkCReg(2,0);
	Reg#(Bool) rg_cacheable[2] <- mkCReg(2,True);
	Reg#(Bit#(asid_width)) rg_asid[2] <- mkCReg(2,0);
	Reg#(Bit#(4)) rg_translation_mode[2] <- mkCReg(2,0);
	Reg#(Bit#(TLog#(`TLB_entries))) rg_slot_to_replace <- mkReg(0);


	rule rl_translation(!rg_handling_PTW[0] && !rg_tlb_disable && !(rg_chmod[1].prv==Machine)
																						&& (rg_translation_mode[1]!=0));
		Bit#(ppn) ppn = 0;
		TLB_permissions perm_bits = TLB_permissions{v:0,r:0,w:0,x:0,u:0,g:0,a:0,d:0};
		Bool hit = False;
		Bool page_fault = False;
		Bool cacheable = False;
		Bit#(vpn) vpn_bits = ff_vpn.first;
		Bit#(vpn_split) lv_vpn_split= 0;
		Bit#(intermediate1) lv_intermediate1 = 0;
		Bit#(intermediate2) lv_intermediate2= 0;
		Bit#(vpn) mask1 = {'1,lv_vpn_split};
		Bit#(vpn) vpnmask1 = vpn_bits & mask1;
		Bit#(vpn) mask2 = {'1,lv_intermediate1};
		Bit#(vpn) vpnmask2 = vpn_bits & mask2;
		Bit#(2) pg_levels = 0;
		Integer slot = 0;
		`ifdef verbose $display($time, "\tThe acquired VPN in iTLB %h", ff_vpn.first); `endif
		for(Integer i = 0; i < `TLB_entries; i = i + 1) begin
			if((vpn_bits==tlb_vpn[i] && tlb_levels[i]==0 
						|| ((vpnmask1==(tlb_vpn[i] & mask1)) && tlb_levels[i]==1)
						|| ((vpnmask2==(tlb_vpn[i] & mask2)) && tlb_levels[i]==2))
						&& (rg_asid[1]==tlb_asid[i] || tlb_permissions[i].g==1) && tlb_permissions[i].v==1) begin
					ppn = tlb_ppn[i];
					perm_bits = tlb_permissions[i];
					pg_levels = tlb_levels[i];
					hit = True;
					slot = i;
					cacheable = tlb_cacheable[i];
			end
		end
		rg_levels[0] <= pg_levels;
		if(hit) begin
			if(rg_chmod[1].sum==0) begin
				if(rg_chmod[1].mprv==1) begin
					if(rg_chmod[1].mpp==unpack(1) && perm_bits.u==1) begin
						page_fault=True;
					end
				end
				else if(rg_chmod[1].prv==unpack(1) && perm_bits.u==1) begin
					page_fault=True;
				end
			end
			else begin
				if(perm_bits.x!=1)
					page_fault=True;
			end
			rg_ppn[0] <= ppn;
			rg_cacheable[0] <= cacheable;
			`ifdef verbose $display($time, "\t hit in iTLB"); `endif
		end
		else begin
			rg_handling_PTW[0] <= True;
			`ifdef verbose $display($time, "\t iTLB: miss"); `endif
		end
		rg_page_fault[0]<=page_fault;
		if(!page_fault) 
			rg_hit[0]<=hit;
		else begin 
			perm_bits.v = 0;
			tlb_permissions[slot] <= perm_bits;
			`ifdef verbose $display($time, "\t page fault in iTLB"); `endif
		end
	endrule

	method Action get_vaddr(Bit#(data_width) vaddr);
			`ifdef verbose $display($time, "\t vpn obtained in TLB"); `endif
			ff_vpn.enq(vaddr[v_vaddr-1: v_page_offset]);
			ff_page_offset.enq(vaddr[v_page_offset-1:0]);
	endmethod

	method ActionValue#(From_TLB#(data_width)) send_ppn if(rg_hit[1] || rg_tlb_disable 
																			|| (rg_chmod[1].prv==Machine) || rg_page_fault[1] || (rg_translation_mode[1]==0));
		Trap_type e = tagged None;
		Bit#(data_width) final_address;
		Bit#(ppn) p_ppn = 0;
		if(rg_levels[1]==0) begin
			p_ppn = rg_ppn[1];
		end
		else if(rg_levels[1]==1) begin
			Bit#(TSub#(ppn,vpn_split)) lv_ppn_split = rg_ppn[1][v_ppn-1:v_vpn_split]; 
			Bit#(vpn_split) lv_vpn_split = ff_vpn.first[v_vpn_split-1:0];
			p_ppn = {lv_ppn_split,lv_vpn_split};
		end
		else if(rg_levels[1]==2) begin
			Bit#(TSub#(ppn,intermediate1)) lv_ppn_split = rg_ppn[1][v_ppn-1:v_intermediate1]; 
			Bit#(intermediate1) lv_vpn_split = ff_vpn.first[v_intermediate1-1:0];
			p_ppn = {lv_ppn_split,lv_vpn_split};
		end
		if(rg_hit[1]) begin
			rg_hit[1] <= False;
			Bit#(paddr) paddress = {p_ppn,ff_page_offset.first()};
			final_address = zeroExtend(paddress);
			//rg_frm_ptw[1] <= False;
		end
		else if(rg_page_fault[1]) begin
			`ifdef verbose $display($time, "\t Instruction Page Fault"); `endif
			e = tagged Exception Inst_pagefault;	
			Bit#(vaddr) paddress = {ff_vpn.first(),ff_page_offset.first()};
			final_address = zeroExtend(paddress);
			rg_page_fault[1] <= False;
			//rg_frm_ptw[1] <= False;
		end
		else begin
			`ifdef verbose $display($time, "\t Bypass iTLB"); `endif
			Bit#(vaddr) paddress = {ff_vpn.first(),ff_page_offset.first()};
			final_address = zeroExtend(paddress);
		end
		ff_page_offset.deq;
		ff_vpn.deq;
		return From_TLB{exception : e, address : final_address, cacheable : rg_cacheable[1]};
	endmethod

	//method Bit#(vaddr) send_vaddress_for_cache_index if(rg_frm_ptw[0]);
	//	return {ff_vpn.first, ff_page_offset.first};
	//endmethod


	method Action translation_protection_frm_csr(bit tlb_disable, Chmod per_bits, Bit#(TAdd#(4,asid_width)) asid);
		rg_tlb_disable <= unpack(tlb_disable);
		rg_asid[0] <= asid[v_asid_width-1:0];
		rg_translation_mode[0] <= asid[v_asid_width+3:v_asid_width];
		rg_chmod[0] <= per_bits;
		`ifdef verbose $display($time, "\t ITLB: mprv %b mxr %b sum %b mpp %b prv %b", per_bits.mprv, per_bits.mxr, per_bits.sum, pack(per_bits.mpp), pack(per_bits.prv)); `endif
	endmethod
		
	interface to_PTW = interface Get
		method ActionValue#(Request_PPN_PTW#(vaddr,page_size)) get if(rg_handling_PTW[1] && !rg_page_fault[1]); 
			return Request_PPN_PTW{ vpn : ff_vpn.first(), page_type : Execution};
		endmethod
	endinterface;

	interface refill_TLB = interface Put
		method Action put(Tuple2#(Bool, To_TLB#(paddr,page_size,asid_width)) tlb_fill) if(rg_handling_PTW[0]);
			let {x,tlb_structure} = tlb_fill;
			rg_page_fault[0] <= x;
			Bit#(paddr) paddress= {tlb_structure.ppn,ff_page_offset.first};
			Bit#(data_width) new_address = zeroExtend(paddress);
			Bool cacheable = True; //!is_IO_Addr(new_address);
			if(!x) begin
				rg_slot_to_replace <= rg_slot_to_replace + 1;
				tlb_vpn[rg_slot_to_replace] <= ff_vpn.first();
				tlb_ppn[rg_slot_to_replace] <= tlb_structure.ppn;
				tlb_permissions[rg_slot_to_replace] <= tlb_structure.tlb_perm;
				tlb_levels[rg_slot_to_replace] <= tlb_structure.levels;
				tlb_asid[rg_slot_to_replace] <= tlb_structure.asid;
				tlb_cacheable[rg_slot_to_replace] <= cacheable;
			end
			rg_handling_PTW[0] <= False;
			//rg_frm_ptw[0] <= True;
			`ifdef verbose $display($time, "\t Filling TLB in slot %d with vpn %h with page levels i %d", rg_slot_to_replace, ff_vpn.first(), tlb_structure.levels); `endif
		endmethod
	endinterface;

	method Action fence_TLB(Fence_VMA_type#(vaddr) rsdata);
		Bool flush_address = False;
		Bool flush_address_space = False;
		Bit#(vpn_split) lv_vpn_split= 0;
		Bit#(intermediate1) lv_intermediate1 = 0;
		Bit#(intermediate2) lv_intermediate2= 0;
		Bit#(vpn) mask1 = {'1,lv_vpn_split};
		Bit#(vpn) vpnmask1 = rsdata.rs1[v_vaddr-1:v_page_offset] & mask1;
		Bit#(vpn) mask2 = {'1,lv_intermediate1};
		Bit#(vpn) vpnmask2 = rsdata.rs1[v_vaddr-1:v_page_offset] & mask2;
		if(rsdata.rs1!=0) begin
			flush_address = True;
			`ifdef verbose $display($time, "\t iTLB address flush %h", rsdata.rs1); `endif
		end
		if(rsdata.rs2!=0) begin
			flush_address_space = True;
			`ifdef verbose $display($time, "\t iTLB address space flush %h", rsdata.rs2); `endif
		end
		for(Integer i = 0; i < `TLB_entries; i = i+1) begin
			if(((flush_address && ((rsdata.rs1[v_vaddr-1:v_page_offset] == tlb_vpn[i] && tlb_levels[i]==0)
							|| (vpnmask1 == (tlb_vpn[i] & mask1) && tlb_levels[i]==1)
									|| (vpnmask2 == (tlb_vpn[i] & mask2) && tlb_levels[i]==2)))
											|| (flush_address_space && rsdata.rs2[v_asid_width-1:0] == tlb_asid[i]))
													|| (!flush_address && !flush_address_space)) begin
				`ifdef verbose $display($time, "\t iTLB entry %d with vpn %h removed",i, tlb_vpn[i]); `endif
				tlb_permissions[i] <= TLB_permissions{v : 0, r : 0, w : 0, x : 0, u : 0, g : 0, a : 0, d : 0};
			end
		end
	endmethod

endmodule

interface Ifc_iTLB;
	method Action get_vaddr(Bit#(`ADDR) addr);
	method ActionValue#(From_TLB#(`ADDR)) send_ppn;
	//method Bit#(`VADDR) send_vaddress_for_cache_index;
	method Action translation_protection_frm_csr(bit tlb_disable, Chmod per_bits, Bit#(TAdd#(4,`ASID)) asid);
	interface Get#(Request_PPN_PTW#(`VADDR,`OFFSET)) to_PTW; 
	interface Put#(Tuple2#(Bool,To_TLB#(`PADDR,`OFFSET,`ASID))) refill_TLB;
	method Action fence_TLB(Fence_VMA_type#(`VADDR) rsdata);
	//method ActionValue#(Bool) page_fault;
	//method Action page_fault_frm_PTW;
endinterface

(*synthesize*)
module mkiTLB(Ifc_iTLB);

Ifc_TLB#(`ADDR,`VADDR,`PADDR,`OFFSET,`ASID) itlb <- mkTLB();
	method Action get_vaddr(Bit#(`ADDR) addr);
		itlb.get_vaddr(addr);
	endmethod
	method ActionValue#(From_TLB#(`ADDR)) send_ppn = itlb.send_ppn;
	//method Bit#(`VADDR) send_vaddress_for_cache_index = itlb.send_vaddress_for_cache_index;
	method Action translation_protection_frm_csr(bit tlb_disable, Chmod per_bits, Bit#(TAdd#(4,`ASID)) asid);
		itlb.translation_protection_frm_csr(tlb_disable,per_bits,asid);
	endmethod
	interface  to_PTW = itlb.to_PTW; 
	interface  refill_TLB = itlb.refill_TLB;
	method Action fence_TLB(Fence_VMA_type#(`VADDR) rsdata);
		itlb.fence_TLB(rsdata);
	endmethod
	//method ActionValue#(Bool) page_fault = itlb.page_fault;
	//method Action page_fault_frm_PTW = itlb.page_fault_frm_PTW;
endmodule
endpackage
