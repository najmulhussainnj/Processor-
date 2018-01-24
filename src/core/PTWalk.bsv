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
package PTWalk;

import ConfigReg:: *;
import defined_types::*;
import GetPut::*;


	function TLB_permissions ptw_to_tlb_perms(Bit#(10) perms);
		return TLB_permissions { v : perms[0],
														 r : perms[1],
														 w : perms[2],
														 x : perms[3],
														 u : perms[4],
														 g : perms[5],
														 a : perms[6],
														 d : perms[7]};
	endfunction
	
	//interface Ifc_TLB#(numeric type vaddr, numeric type page_size, numeric type paddr, numeric type asid_width);
	//	method Action get_vpn(Request_PPN_PTW#(vaddr,page_size) req);
	//	method ActionValue#(Response_PPN_TLB#(paddr, page_size, asid_width)) send_ppn;
	//endinterface

	interface Ifc_memory#(numeric type data_width);
		method ActionValue#(Request_PTE_memory#(data_width)) send_PTE_pointer;
		method Action get_PTE(Bit#(data_width) pte);
	endinterface

	interface Ifc_PTWalk#(numeric type data_width, 
												numeric type vaddr, 
												numeric type paddr, 
												numeric type real_paddr, 
												numeric type asid_width,
												numeric type page_size);
		interface Put#(Request_PPN_PTW#(vaddr,page_size)) frm_TLB;
		interface Get#(Tuple2#(Bool,To_TLB#(real_paddr,page_size,asid_width))) to_TLB;
		interface Ifc_memory#(data_width) ifc_memory;
		method Action satp_frm_csr(Bit#(data_width) satp);
		method Action flush(Translation_type _flush);
		//method Bool ptwalkdone;
		//method Maybe#(Translation_type) page_fault;
	endinterface

	module mkPTWalk(Ifc_PTWalk#(data_width,vaddr_width,paddr_width, real_paddr, asid_width,page_size_bits))
		provisos(
			Mul#(8,no_of_bytes, data_width),
			Log#(no_of_bytes, addressable_bits),
			Log#(data_width, data_width_bits),
			Add#(vpn_width, page_size_bits, vaddr_width),
			Add#(vpn_split, addressable_bits, page_size_bits),
			Add#(ppn_width, page_size_bits, paddr_width),
			Add#(real_ppn_width, page_size_bits, real_paddr),
			Mul#(vpn_split, levels, vpn_width),
			Add#(10, ppn_width, ppn_perm),
			Add#(y_, ppn_width, data_width),
			Add#(x_, real_ppn_width, data_width),
			Add#(a_, ppn_perm, data_width),
			Add#(b_, paddr_width, data_width),
			Add#(c_, 10, data_width),
			Add#(d_, vpn_width, data_width),
			Add#(1, sub_levels, levels)
			);

		let v_data_width = valueOf(data_width);
		let v_data_width_bits = valueOf(data_width_bits);
		let v_vpn_width = valueOf(vpn_width);
		let v_vpn_split = valueOf(vpn_split);
		let v_ppn_width = valueOf(ppn_width);
		let v_real_ppn_width = valueOf(real_ppn_width);
		let v_asid_width = valueOf(asid_width);
		let v_levels = valueOf(levels);
		let v_sub_levels = valueOf(sub_levels);
		let v_addressable_bits = valueOf(addressable_bits);

		function Tuple2#(Bool, Bit#(ppn_perm)) fn_super_page_physical_address(Bit#(vpn_width) vpn,
																																					 Bit#(data_width) pte,
																																					 Int#(32) levels);
				Bool page_fault = !unpack(pte[0]);
				Bit#(ppn_width) step_ppn = 0;
				if(levels==3) begin
					Bit#(TMul#(3,vpn_split)) offset_3 = pte[3*v_vpn_split+9:10];
					Bit#(TSub#(ppn_width,TMul#(3,vpn_split))) ppn= pte[v_ppn_width+9:3*v_vpn_split+10];
					Bit#(TMul#(3,vpn_split)) vpn_offset = vpn[3*v_vpn_split-1:0];
					step_ppn = {ppn,vpn_offset};
					if(offset_3!=0)
						page_fault = True;
				end
				else if(levels==2) begin
					Bit#(TMul#(2,vpn_split)) offset_2 = pte[2*v_vpn_split+9:10];
					Bit#(TSub#(ppn_width,TMul#(2,vpn_split))) ppn= pte[v_ppn_width+9:2*v_vpn_split+10];
					Bit#(TMul#(2,vpn_split)) vpn_offset = vpn[2*v_vpn_split-1:0];
					step_ppn = {ppn,vpn_offset};
					if(offset_2!=0)
						page_fault = True;
				end
				else if(levels==1) begin
					Bit#(TMul#(1,vpn_split)) offset_1 = pte[1*v_vpn_split+9:10];
					Bit#(TSub#(ppn_width,TMul#(1,vpn_split))) ppn= pte[v_ppn_width+9:1*v_vpn_split+10];
					Bit#(TMul#(1,vpn_split)) vpn_offset = vpn[1*v_vpn_split-1:0];
					step_ppn = {ppn,vpn_offset};
					if(offset_1!=0)
						page_fault = True;
				end
			return tuple2(page_fault,{step_ppn,pte[9:0]});
		endfunction
							 
		Reg#(Bit#(vpn_width)) rg_vpn <- mkReg(0);
		Reg#(Int#(32))	rg_levels <- mkConfigReg(fromInteger(v_sub_levels));
		Reg#(Bit#(data_width)) rg_ppn[2] <- mkCReg(2,0);
		Reg#(Bit#(asid_width)) rg_asid[2] <- mkCReg(2,0);
		Reg#(Bit#(data_width)) rg_pte <- mkReg(0);
		Reg#(Bit#(data_width)) rg_pte_pointer <- mkReg(0);
		Reg#(PTW_state) rg_ptw_state[2] <- mkCReg(2,PTW_ready);
		Reg#(Bit#(data_width)) rg_satp <- mkReg(0);
		Reg#(Bool) rg_page_fault <- mkReg(False);
		Reg#(Bit#(10)) rg_permission_bits <- mkReg(0);
		Reg#(Translation_type) rg_page_type <- mkConfigReg(Load);
		Wire#(Bool) wr_flush <- mkDWire(False);
		
		//(*conflict_free="rl_computer_next_pointer, rl_return_from_page_fault"*)
		rule rl_computer_next_pointer(rg_ptw_state[1] == Handling_PTW && !wr_flush);
				Int#(32) vpn_trnct = (rg_levels+1)*fromInteger(v_vpn_split);
				Bit#(vpn_split) lv_vpn_split = rg_vpn[vpn_trnct-1:vpn_trnct-fromInteger(v_vpn_split)];
				Bit#(page_size_bits) vpn_addr = zeroExtend(lv_vpn_split);
				vpn_addr = vpn_addr << v_addressable_bits;
				`ifdef verbose $display($time, "\tPTW: The VPN split bits are  %d split is %h", vpn_trnct, vpn_addr); `endif
				Int#(32) ppn_trnct = (rg_levels)*fromInteger(v_vpn_split) + 10;
				Bit#(ppn_width) p_pte= rg_pte[v_ppn_width+9:10];
				Bit#(data_width) lv_zeros = 0;
				rg_permission_bits <= rg_pte[9:0];
				`ifdef verbose $display($time, "\tPTW: page table entry %h and page level is %d", rg_pte, rg_levels); `endif
				if(rg_pte[0]==0 || (rg_pte[1]==0 && rg_pte[2]==1)) begin
					rg_ptw_state[1] <= PTW_done;
					`ifdef verbose 	$display($time,"\tPTW: Page Fault due to reason1 "); `endif
					rg_page_fault <= True;
				end
				else if((rg_pte[3]==1 || rg_pte[1]==1)) begin //if executable and read permission bits are 1 then it is a super page
					rg_ptw_state[1] <= PTW_done;
					match{.x,.y} = fn_super_page_physical_address(rg_vpn,rg_pte,rg_levels+1);
					if(x) begin
						rg_page_fault <= True;
						`ifdef verbose 	$display($time,"Page Fault due to reason2 "); `endif
					end
					else begin
						rg_pte <= zeroExtend(y);
						`ifdef verbose 	$display($time,"Superpage has been found"); `endif
						if(rg_pte[6]==0 || (rg_page_type==Store && rg_pte[7]==0))
							rg_page_fault <= True;
					end
					//if(rg_pte[ppn_trnct-1:10]!=0) begin TODO
						//Bit#(data_width) pte_paddr = rg_pte << ppn_trnct;
						//Bit#(vpn_width) pte_vpn = rg_vpn << rg_levels*(fromInteger(v_vpn_split));
						//Bit#(data_width) pte_vaddr = zeroExtend(pte_vpn);
						//pte_vaddr = pte_vaddr >> rg_levels*fromInteger(v_vpn_split);
						//rg_pte_pointer <= pte_paddr | pte_vaddr;
						//`ifdef verbose $display($time, "\t It's is a superpage %h", pte_paddr | pte_vaddr); `endif
					//end
					//else
					//	rg_page_fault <= True;
				end
				else begin/* if(rg_levels == 0) begin
					rg_ptw_state[1] <= Wait_for_memory;
					rg_pte_pointer <= {rg_pte[v_data_width-1:10],lv_zeros[9:0]};
					`ifdef verbose 
					Bit#(data_width) pte_pointer = {rg_pte[v_data_width-1:10],lv_zeros[9:0]};
					$display($time, "\t  %h", pte_pointer); `endif
				end
				else if(rg_levels != 0) begin*/
					Bit#(paddr_width) lv_pte_pointer = {p_pte,vpn_addr}; 
					rg_pte_pointer <= zeroExtend(lv_pte_pointer);
					rg_ptw_state[1] <= Send_to_memory;
					`ifdef verbose $display($time, "\t next page table pointer %h", lv_pte_pointer); `endif
				end
						
		endrule	

		rule rl_return_from_page_fault(wr_flush);
			`ifdef verbose $display($time, "\tPTW: Flushed page table walk"); `endif
			rg_ptw_state[1] <= PTW_ready;
			rg_page_fault <= False;
			rg_levels <= fromInteger(v_sub_levels);
		endrule

		interface frm_TLB = interface Put
													method Action put(Request_PPN_PTW#(vaddr_width,page_size_bits) req) if(rg_ptw_state[1]==PTW_ready && !wr_flush);
														rg_vpn <= req.vpn;	
														rg_page_type <= req.page_type;
														`ifdef verbose $display($time, "\tPTW: vpn obtained is %h", req.vpn);  `endif
														rg_ptw_state[1] <= Handling_PTW;
													endmethod
												endinterface;

		interface to_TLB = interface Get
													method ActionValue#(Tuple2#(Bool,To_TLB#(real_paddr, page_size_bits, asid_width))) get if(rg_ptw_state[1]==PTW_done && !wr_flush);
														rg_page_fault <= False;
														rg_ptw_state[1] <= PTW_ready;
														Bit#(2) pg_levels = truncate(pack(rg_levels) + 1);
														rg_levels <= fromInteger(v_sub_levels);
														if(rg_levels==fromInteger(v_sub_levels))
															pg_levels=0;
														`ifdef verbose $display($time, "\tPTW: physical page number %h with permission bits  %b", rg_pte, rg_page_fault);  `endif
														return tuple2(rg_page_fault, To_TLB { ppn : rg_pte[v_real_ppn_width+9:10],
																										  tlb_perm : ptw_to_tlb_perms(rg_pte[9:0]),	
																										  asid : rg_asid[1],
																										  levels : pg_levels});
													endmethod
												endinterface;

		interface ifc_memory = interface Ifc_memory
													method ActionValue#(Request_PTE_memory#(data_width)) send_PTE_pointer if(!wr_flush && rg_ptw_state[1]== Send_to_memory);
														`ifdef verbose $display($time, "\tPTW: Sending from PTW to dcache for address %h", rg_pte_pointer); `endif
															rg_ptw_state[1] <= Wait_for_memory;
															return Request_PTE_memory{ptwdone : (rg_ptw_state[1]==PTW_done), address : rg_pte_pointer, page_type : rg_page_type};
														endmethod
														method Action get_PTE(Bit#(data_width) pte) if(rg_ptw_state[1]== Wait_for_memory && !wr_flush);
														`ifdef verbose $display($time, "\tPTW: pte obtained from memory %h", pte); `endif
															rg_pte <= pte;
															if(rg_levels == 0) begin	
																`ifdef verbose $display($time, "\tPTW: Last level PTW with pte %h and page fault", pte, rg_page_fault); `endif
																rg_ptw_state[1] <= PTW_done;
																if(pte[0]==0 || pte[6]==0 || (rg_page_type==Store && pte[7]==0)) begin
																	`ifdef verbose $display($time, "\tPTW: Access and dirty page fault %h", pte); `endif
																	rg_page_fault <= True;
																end
																rg_levels <= fromInteger(v_sub_levels);
															end
															else begin
																rg_ptw_state[1] <= Handling_PTW;
																rg_levels <= rg_levels-1;
															end
														endmethod
													 endinterface;

		method Action satp_frm_csr(Bit#(data_width) satp) if(rg_ptw_state[1] == PTW_ready);
			Bit#(ppn_width) p_pte = satp[v_ppn_width-1:0];
			//Bit#(data_width) pte = zeroExtend(p_pte);
			Bit#(10) perm_bits = 'b1;
			Bit#(ppn_perm) pte ={p_pte,perm_bits}; 
			rg_pte <= zeroExtend(pte);
			//`ifdef verbose $display($time, "\t page table pointer %h", p_pte); `endif
			rg_asid[0] <= satp[v_asid_width+v_ppn_width-1:v_ppn_width];
		endmethod

		method Action flush(Translation_type _flush);
			if((rg_page_type==Execution && _flush==Execution) || (rg_page_type!=Execution && _flush!=Execution))
			  wr_flush <= True;
		endmethod

		//method Bool ptwalkdone;
		//	Bool done = False; 
		//	if(rg_ptw_state[1]==PTW_done)
		//		done = True;
		//	return done;
		//endmethod

		//method Maybe#(Translation_type) page_fault /*if(rg_ptw_state[1]==PTW_done)*/;
		//	if(rg_page_fault)
		//		return tagged Valid rg_page_type;
		//	else 
		//		return tagged Invalid;
		//endmethod

	endmodule
endpackage
