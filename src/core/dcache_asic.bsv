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
package dcache_asic;
	/*===== Pacakge imports ===== */
	import BRAMCore::*;
	import FIFO::*;
	import FIFOF::*;
	import SpecialFIFOs::*;
	import LFSR::*;
	import ConfigReg::*;
	import DReg::*;
	import BUtils::*;
	import MemoryMap::*;
	import mem_config1::*;
	import Vector::*;
	/*===== project imports==== */
	import defined_types::*;
	`include "defined_parameters.bsv"
	import QuadMem::*;
	import Assert::*;
	/*========================= */
	interface Ifc_dcache;
		method Action virtual_address(Bit#(`VADDR) vaddress, Access_type load_store, Bit#(TMul#(`DCACHE_WORD_SIZE,8)) writedata, Bit#(3) transfer_size, `ifdef atomic Bit#(5) atomic_op, `endif Bool signextend, Bit#(1) insnepoch);
		method Maybe#(Tuple4#(Bit#(`Reg_width), Trap_type,Bit#(`PERFMONITORS),Bit#(1))) response_to_core;
		method ActionValue#(To_Memory#(`PADDR)) read_request_to_memory;
		method ActionValue#(To_Memory_Write) write_request_to_memory;
		method Action read_response_from_memory(From_Memory#(`DCACHE_WORD_SIZE) resp);
		method Action write_response_from_memory(From_Memory#(`DCACHE_WORD_SIZE) resp);
		method Bool init_complete;
		method Action flush_from_wb;
		`ifdef MMU
			method Action physical_address(Bit#(`PADDR) paddr, Trap_type exception);
		`endif
	endinterface

	typedef enum {Idle,Dummy,KeepPolling,Stall1,ReadingCache,Initialize,Fence,FenceStart,IOReadResp,IOWriteResp} DcacheState deriving (Bits,Eq,FShow);

	(*synthesize*)
	(*conflict_free="virtual_address,pre_fence_updating"*)
	(*conflict_free="virtual_address,handle_fence"*)
	(*conflict_free="keep_polling_on_stall,handle_fence"*)
	(*conflict_free="keep_polling_on_stall,pre_fence_updating"*)
	(*conflict_free="keep_polling_on_stall,wait_for_ioread_response"*)
	(*conflict_free="keep_polling_on_stall,wait_for_iowrite_response"*)
	(*conflict_free="dummy_cycle,read_from_lbdata_into_hold_reg"*)
	(*preempts="virtual_address,read_from_lbdata_into_hold_reg"*)
//	(*preempts="keep_polling_on_stall,read_from_lbdata_into_hold_reg"*)
	(*preempts="stall_the_next_request_by_one_cycle,read_from_lbdata_into_hold_reg"*)
	(*preempts="read_from_lbdata_into_hold_reg,keep_polling_on_stall"*)
	module mkdcache(Ifc_dcache);
		/* VAddr = [tag_bits|set_bits|word_bits|byte_bits] */
		let byte_bits=valueOf(TLog#(`DCACHE_WORD_SIZE));	// number of bits to select a byte within a word. = 2
		let word_bits=valueOf(TLog#(`DCACHE_BLOCK_SIZE));	// number of bits to select a word within a block. = 4
		let set_bits=valueOf(TLog#(`DCACHE_SETS));			// number of bits to select a set from the cache. = 
		Reg#(Maybe#(Tuple2#(Bit#(1),Bit#(`PADDR)))) rg_lr_paddress<-mkReg(tagged Invalid);
		`ifdef atomic
		function ActionValue#(Tuple3#(Maybe#(Bit#(1)),Bool, Bit#(TMul#(`DCACHE_WORD_SIZE,8)))) atomic_operation(Bit#(TMul#(`DCACHE_WORD_SIZE,8)) loaded_value, Bit#(TMul#(`DCACHE_WORD_SIZE,8)) rs2, Bit#(5) atomic_op, Bit#(`PADDR) addr);
		return (
		actionvalue 
			Bit#(TMul#(`DCACHE_WORD_SIZE,8)) atomic_result=rs2;
			Bit#(TMul#(`DCACHE_WORD_SIZE,8)) op1;
			Maybe#(Bit#(1)) sc_done=tagged Invalid;
			if(atomic_op[4]==1)
				op1=signExtend(loaded_value[31:0]);
			else
				op1=loaded_value;
			Bit#(TMul#(`DCACHE_WORD_SIZE,8)) op2=(atomic_op[4]==1)?signExtend(rs2[31:0]):rs2;
			Int#(TMul#(`DCACHE_WORD_SIZE,8)) s_op1=unpack(op1);
			Int#(TMul#(`DCACHE_WORD_SIZE,8)) s_op2 = unpack(op2);
			Bool store_result = True;
			`ifdef verbose $display($time,"\tDCACHE: atomic instruction atomic op %b op1: %h op2: %h", atomic_op,op1,op2); `endif
			case (atomic_op[3:0])
				'b0011:atomic_result=op2;
				'b0000:atomic_result= (op1+op2);
				'b0010:atomic_result= (op1^op2);
				'b0110:atomic_result= (op1&op2);
				'b0100:atomic_result= (op1|op2);
				'b1100:atomic_result= min(op1,op2);
				'b1110:atomic_result= max(op1,op2);
				'b1000:atomic_result= pack(min(s_op1,s_op2));
				'b1010:atomic_result= pack(max(s_op1,s_op2));
				default:	begin atomic_result= op1; end
			endcase	
			case (atomic_op[3:0])
				'b0101:	begin 
								rg_lr_paddress <= tagged Valid tuple2(atomic_op[4],addr);
								atomic_result=loaded_value; // LR
								store_result = False;
							end
				'b0111:	begin
								rg_lr_paddress <= tagged Invalid;
								atomic_result=rs2;			  // SC
								sc_done = tagged Valid 1;
								store_result = False;
								`ifdef verbose $display($time,"\tDCACHE: store condition instruction"); `endif
								if(rg_lr_paddress matches tagged Valid .lr) begin
									let {x,y} = lr;
									if(x==atomic_op[4] && addr== y) begin
										`ifdef verbose $display($time,"\tDCACHE: store condition satisfied"); `endif
										sc_done = tagged Valid 0;
										store_result = True;
								  end
								end
							end
				default:	begin rg_lr_paddress<=tagged Invalid ;end
			endcase	
			if(atomic_op[4]==1)
					atomic_result=duplicate(atomic_result[31:0]);

			return tuple3(sc_done,store_result,atomic_result);
			endactionvalue );
		endfunction
		`endif
		function Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) update_line (Bit#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE)) we, Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) data, Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) data_reg);
         Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) mask=0;
         for(Integer i=0;i<32;i=i+1)begin
            Bit#(8) ex_we=duplicate(we[i]);
            mask[(i*8)+7:i*8]=ex_we;
         end
         Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) x = mask& data;
         Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) y = ~mask& data_reg;
         data_reg=x|y;
			return data_reg;
      endfunction
		
		Ifc_dcache_data data [`DCACHE_WAYS];
		Ifc_dcache_tag	 tag  [`DCACHE_WAYS];
		for(Integer i=0;i<`DCACHE_WAYS;i=i+1)begin
			tag[i] <- mkdcache_tag;		
			data[i] <-mkdcache_data;
		end

		/*====== Hit buffer data structur======*/
		Reg#(Bool) hb_valid <-mkReg(False);
		Reg#(Bit#(`DCACHE_WAYS)) hb_way <-mkReg(0);
		Reg#(Bit#(`DCACHE_TAG_BITS)) hb_tag <-mkReg(0);
		Reg#(Bit#(TLog#(`DCACHE_SETS))) hb_setindex <- mkReg(0);
		Ifc_QuadMem hb_data <-mkQuadMem;
		/*=====================================*/

		/*-====== Line buffer data structure ====*/
		Ifc_QuadMem lb_data <-mkQuadMem;
		FIFOF#(Tuple4#(Bit#(20),Bit#(TLog#(`DCACHE_SETS)),Bit#(`DCACHE_WAYS),Bit#(TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE)))) memoperation <-mkUGSizedFIFOF(2);
		Reg#(Bit#(1)) lb_dirty <-mkReg(0);
		Reg#(Bit#(TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE))) line_bytes_written<-mkReg(0);
		/*=====================================*/

		/*======= Request Capture =========*/
		Reg#(Bit#(`VADDR)) rg_vaddress <-mkReg(0);
		Reg#(Bit#(`PADDR)) rg_paddress <-mkReg(0);
		Reg#(Bit#(`PADDR)) rg_poll_address <-mkReg(0);
		Reg#(Bit#(3))		rg_transfer_size <-mkReg(0);
		`ifdef atomic Reg#(Bit#(5))		rg_atomic_op <-mkReg(0); `endif
		Reg#(Access_type) rg_access_type <-mkReg(Load);
		Reg#(Bit#(TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE))) rg_writeenable<-mkReg(0);
		Reg#(Bool) rg_signextend<-mkReg(False);
		Reg#(Bool)		misaligned_addr <-mkReg(False);
		Reg#(Bit#(1))		rg_insn_epoch <-mkReg(0);
		Reg#(Bit#(TMul#(`DCACHE_WORD_SIZE,8))) rg_writedata<-mkReg(0);
		/*=================================*/
		/* storage for physical translation */
		Reg#(Bool)	rg_trnslte_done[2] <- mkCReg(2,`ifdef MMU False `else True `endif );
		Reg#(Trap_type) rg_tlb_exception[2]<-mkCReg(2,tagged None);
		/*==================================== */

		/*===== registers for fencing/initializing ====*/
		Reg#(Bit#(TLog#(`DCACHE_SETS))) fence_set <-mkReg(0);
		Reg#(Bit#(TLog#(`DCACHE_WAYS))) fence_way <-mkReg(0);
		/*==============================================*/
		
		/*========= FIFO for interfaces ================*/
		FIFOF#(To_Memory#(`PADDR)) ff_read_request_to_memory <-mkLFIFOF();
		FIFOF#(To_Memory_Write) ff_write_request_to_memory <-mkLFIFOF();
		FIFOF#(From_Memory#(`DCACHE_WORD_SIZE)) ff_read_response_from_memory <-mkSizedBypassFIFOF(1);
		FIFOF#(From_Memory#(`DCACHE_WORD_SIZE)) ff_write_response_from_memory <-mkSizedBypassFIFOF(1);
		/*===============================================*/

		/*===== State Registers========*/
		Reg#(Bit#(1)) wbEpoch [3]  <-mkCReg(3,0);
		Reg#(DcacheState)	  rg_state[3]	<-mkCReg(3,Initialize);
		/*============================*/

		/*============ globals =========*/
		Reg#(Bool) rg_global_dirty <-mkReg(False);
		Wire#(Maybe#(Tuple2#(Bit#(20),Bit#(TLog#(`DCACHE_SETS))))) wr_write_info<-mkDWire(tagged Invalid);	
		Wire#(Maybe#(Tuple4#(Bit#(`Reg_width), Trap_type, Bit#(`PERFMONITORS),Bit#(1)))) wr_response_to_cpu<-mkDWire(tagged Invalid);
		Reg#(Bit#(`PERFMONITORS)) rg_perf_monitor<-mkReg(0);
		LFSR#(Bit#(2)) random_line<-mkRCounter(3);								// for random line replacement
		Reg#(Bool) pending_write_response[3]<-mkCReg(3,False);
		Reg#(Bool) capture_counters <-mkDReg(False);
		Reg#(Bool) rg_initialize <-mkReg(True);
		Reg#(Bit#(TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE))) rg_we<-mkReg(0);
		Reg#(Bool) rg_bus_error<-mkReg(False);
		/*==============================*/
		rule display_state;
			`ifdef verbose $display($time,"\tDCACHE: state ",fshow(rg_state[0])," wbEpoch: %b",wbEpoch[0]); `endif
		endrule

		rule dummy_cycle(rg_state[1]==Dummy);
			rg_state[1]<=Idle;
		endrule

		rule deq_write_response_during_fence(pending_write_response[2]);
			ff_write_response_from_memory.deq;
			pending_write_response[2]<=False;
		endrule
		rule pre_fence_updating(rg_state[0]==FenceStart && !memoperation.notEmpty && !pending_write_response[2]);
			if(wbEpoch[0]==rg_insn_epoch)begin
				if(hb_valid)begin
					for(Integer i=0;i<`DCACHE_WAYS;i=i+1)begin	
						tag[i].write_request(unpack(hb_way[i]),hb_setindex,{2'b11,hb_tag});
						data[i].write_request(duplicate(hb_way[i]),hb_setindex,hb_data.response_portA);
					end
					hb_valid<=False;
				end
				else begin
					rg_state[0]<=Fence;
					tag[0].read_request(0);
					data[0].read_request(0);
					fence_set<=0;
					fence_way<=0;
				end
			end
			else begin
				wr_response_to_cpu<=tagged Valid (tuple4(0,tagged None, 0, rg_insn_epoch));
			end
		endrule
		/*====== Invalidate all the entries in the cache on startup or during Fence ==== */
		rule fencing_the_cache(rg_state[0]==Initialize && !memoperation.notEmpty && !pending_write_response[2]);
			`ifdef verbose $display($time,"\tDCACHE: Initializing index: %d",fence_set," ",fshow(rg_access_type)); `endif
			for(Integer i=0;i<`DCACHE_WAYS;i=i+1)begin
				tag[i].write_request(True,truncate(fence_set),0);
			end
			if(fence_set==fromInteger(`DCACHE_SETS-1)) begin
				rg_state[0]<=Dummy;
				fence_set<=0;
				fence_way<=0;
				random_line.seed('d3);
				rg_global_dirty<=False;
				rg_trnslte_done[0]<=False;
				if(rg_access_type==Fence)
					wr_response_to_cpu<= tagged Valid (tuple4(0,tagged None,0,rg_insn_epoch));
			end
			else
				fence_set<=fence_set+1;
		endrule
		/*=============================================================================== */
		rule handle_fence(rg_state[0]==Fence &&!memoperation.notEmpty);
			Bit#(20) tag_values=tag[fence_way].read_response[20-1:0];	// hold the tag values
			Bit#(1) dirty_value=tag[fence_way].read_response[20+1];		// holds the dirty bits
			Bit#(1) valid_value=tag[fence_way].read_response[20];		// holds the dirty bits
			Bit#(TMul#(8,TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE))) data_values; // holds the cache lines.
			Bit#(TAdd#(TLog#(`DCACHE_WORD_SIZE),TLog#(`DCACHE_BLOCK_SIZE))) p_offset =0;
			data_values=data[fence_way].read_response;

			Bit#(`PADDR) write_addr={tag_values,truncate(fence_set),p_offset};
			`ifdef verbose $display($time,"\tDCACHE: Handling Fence.tag %h setindex: %d fence_way: %d Dirty: %b Valid: %b",tag_values,fence_set,fence_way,dirty_value,valid_value); `endif
			`ifdef verbose $display($time,"\tDCACHE: Fence addr: %h line: %h ",write_addr,data_values); `endif 
			Bit#(TLog#(`DCACHE_SETS)) new_set=fence_set;
			Bit#(TLog#(`DCACHE_SETS)) old_set=fence_set;
			Bit#(TLog#(`DCACHE_WAYS)) next_way=fence_way;
			if(!pending_write_response[1])begin
				if(dirty_value==1 && valid_value==1)begin // valid and dirty
					ff_write_request_to_memory.enq(To_Memory_Write { // send the request to memory to 
	            	 address:write_addr,  data_line:data_values,
							burst_length:`DCACHE_BLOCK_SIZE,  transfer_size:3, ld_st:Store});
					pending_write_response[1]<=True;
				end
				if(fence_way==fromInteger(`DCACHE_WAYS-1))begin
					new_set=fence_set+1;
					if(fence_set==fromInteger(`DCACHE_SETS-1))begin
						rg_state[0]<=Dummy;
						rg_global_dirty<=False;
						wr_response_to_cpu<= tagged Valid (tuple4(0,tagged None,0,rg_insn_epoch));
						rg_trnslte_done[0]<=False;
						fence_set<=0;
					end
					else
						fence_set<=new_set;
				end
				next_way=fence_way+1;
				tag[fence_way].write_request(True,old_set,0);
			end
			`ifdef verbose $display($time,"\tDCACHE: FENCE: sending request to setindex: %d way: %d",new_set,next_way); `endif
			tag[next_way].read_request(new_set);
			data[next_way].read_request(new_set);
			fence_way<=next_way;
		endrule

		rule read_from_lbdata_into_hold_reg(line_bytes_written=='1 && memoperation.notEmpty);
			let lb_hold_reg=lb_data.response_portB;
			let {cputag,setindex,replaceblock,writeenable}=memoperation.first;
			for(Integer i=0;i<`DCACHE_WAYS;i=i+1)begin	
				tag[i].write_request((unpack(replaceblock[i])&&True),setindex,{lb_dirty,1'b1,cputag});
				data[i].write_request(duplicate(replaceblock[i]),setindex,lb_hold_reg);
			end
			line_bytes_written<=0;
			lb_dirty<=0;
			memoperation.deq;
			rg_bus_error<=False;
			`ifdef verbose $display($time,"\tDCACHE: capturing lbdata cpu_tag: %h setindex: %d addr: %h linenum: %b data: %h",cputag, setindex,{cputag,setindex,6'd0}, replaceblock,lb_hold_reg); `endif
			if(rg_state[1]==KeepPolling)
				rg_state[1]<=Stall1;
		endrule
		
		rule fillcache(memoperation.notEmpty && line_bytes_written!='1); // need to check line_bytes_written to ensure the same response is being served.
			let memresp=ff_read_response_from_memory.first;
			ff_read_response_from_memory.deq;
			rg_bus_error<=unpack(memresp.bus_error)||rg_bus_error;
			let {cpu_tag,setindex,replaceblock,writeenable}=memoperation.first;
			`ifdef verbose $display($time,"\tDCACHE: Response from Memory: %h setindex: %d cpu_tag: %h replaceblock: %b",memresp.data_line,setindex,cpu_tag,replaceblock); `endif 
			let we=writeenable;
			if(|line_bytes_written!=0)begin
				we=rg_we;
			end
			Bit#(TMul#(2,TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE))) extended_mask=zeroExtend(we)<<8;
			lb_data.write_portB(we,duplicate(memresp.data_line));
			`ifdef verbose $display($time,"\tDCACHE: linebytes: %h currently writing into: %h",line_bytes_written,we); `endif
			if(memresp.last_word)begin // if all the data words have been fetched exit	
				`ifdef verbose $display($time,"\tDCACHE: Received Last response from Memory set: %d ",setindex); `endif
			end
			line_bytes_written<=line_bytes_written|we;
			rg_we<=extended_mask[2*`DCACHE_BLOCK_SIZE*`DCACHE_WORD_SIZE-1:`DCACHE_BLOCK_SIZE*`DCACHE_WORD_SIZE]|extended_mask[`DCACHE_BLOCK_SIZE*`DCACHE_WORD_SIZE-1:0];
		endrule

		rule drop_incoming_request(rg_state[0]==ReadingCache && memoperation.notFull && wbEpoch[0]!=rg_insn_epoch);
			if(rg_trnslte_done[0])
				wr_response_to_cpu<=tagged Valid (tuple4(0,tagged None, 0, rg_insn_epoch));
			`ifdef verbose $display($time,"\tDCACHE: Dropping incoming request wbEpoch: %b rg_insn_epoch: %b",wbEpoch[0],rg_insn_epoch); `endif
			rg_trnslte_done[0]<=False;
			rg_state[0]<=Idle;
		endrule
		/*============== One cycle delay to ensure the write is reflected in the BRAM ========= */
		rule stall_the_next_request_by_one_cycle(rg_state[1]==Stall1);
			Bit#(TLog#(`DCACHE_SETS)) setindex=rg_vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits];
			for(Integer i=0;i<`DCACHE_WAYS;i=i+1)begin // send address to the Block_rams
				tag[i].read_request(setindex);
				data[i].read_request(setindex);
			end
			rg_state[1]<=ReadingCache;
		endrule
		/*===================================================================================== */
		
		rule keep_polling_on_stall(rg_state[1]==KeepPolling);
			Bit#(`PERFMONITORS) perf_monitor=rg_perf_monitor;
			if(capture_counters)begin
				`ifdef verbose $display($time,"\tDCACHE: Miss during polling for ",fshow(rg_access_type)); `endif
				if(rg_access_type==Load)begin
					perf_monitor[`DCACHE_LOAD_MISS]=1;
					perf_monitor[`DCACHE_CACHEABLE_LOAD]=1;
				end
				else if(rg_access_type==Store)begin
					perf_monitor[`DCACHE_STORE_MISS]=1;
					perf_monitor[`DCACHE_CACHEABLE_STORE]=1;
				end
				else if(rg_access_type==Atomic) begin
					perf_monitor[`DCACHE_ATOMIC_MISS]=1;
					perf_monitor[`DCACHE_CACHEABLE_ATOMIC]=1;
				end
				rg_perf_monitor<=perf_monitor;
			end
			Bit#(TLog#(`DCACHE_SETS)) setindex=rg_vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits];
			Bit#(20) cpu_tag=rg_poll_address[`PADDR-1:`PADDR-20];
			let {lbtag,lbset,lbreplaceblock,lbwriteenable}=memoperation.first;
			if((line_bytes_written & rg_writeenable) == rg_writeenable && (lbset==setindex && lbtag==cpu_tag))begin
				`ifdef verbose $display($time,"\tDCACHE: Accessing LB"); `endif
				rg_state[1]<=ReadingCache; 
				for(Integer i=0;i<`DCACHE_WAYS;i=i+1)begin // send address to the Block_rams
					tag[i].read_request(setindex);
					data[i].read_request(setindex);
				end
			end
			`ifdef verbose $display($time,"\tDCACHE: Polling on LB. cpu_tag: %h lbtag: %h required: %h bytes in Buffer: %h",cpu_tag,lbtag,rg_writeenable,line_bytes_written); `endif
		endrule


		rule read_from_memory_structures(rg_state[0]==ReadingCache && memoperation.notFull && wbEpoch[0]==rg_insn_epoch);
			Bool cache_enabled = !is_IO_Addr(rg_paddress);
			Trap_type exception = misaligned_addr?((rg_access_type==Load)?
					tagged Exception Load_addr_misaligned:tagged Exception Store_addr_misaligned): rg_bus_error?
						(rg_access_type==Load?tagged Exception Load_access_fault:tagged Exception Store_access_fault):rg_tlb_exception[0];
			/*====== Get the states of the request ======*/
			Bit#(20) cpu_tag=rg_paddress[`PADDR-1:`PADDR-20];
			Bit#(TLog#(`DCACHE_BLOCK_SIZE))	word_offset=rg_vaddress[word_bits+byte_bits-1:byte_bits];
			Bit#(TLog#(`DCACHE_WORD_SIZE))	byte_offset=rg_vaddress[byte_bits-1:0];
			Bit#(TLog#(`DCACHE_SETS))			setindex=rg_vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits];

			Bit#(TMul#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE),8)) hbdataline=0;
			Bit#(TMul#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE),8)) lbdataline=0;
						
			/*========== Check hit on Hit buffer =======*/
			Bool hb_hit = False;
			if(hb_valid && (hb_setindex==setindex) && (hb_tag==cpu_tag) && !misaligned_addr)begin
				hb_hit=True;
				hbdataline=hb_data.response_portA;
			end
			/*==========================================*/

			/*========= Check Line buffer ==============*/
			Bool stall_on_lb=((line_bytes_written & rg_writeenable) != rg_writeenable) && memoperation.notEmpty;
			Bool lb_valid=memoperation.notEmpty;
			let {lb_tag,lb_setindex,lb_way,lb_we}=memoperation.first;
			Bool lb_hit = False;
			if(lb_valid && (lb_setindex==setindex) && (lb_tag==cpu_tag) && !misaligned_addr)begin
				lb_hit=True;
				lbdataline=lb_data.response_portA;
			end
			/*===========================================*/

			/*======= Check SRAMS ==============*/
			Bit#(`DCACHE_WAYS) tag_hit=0;

			Bit#(TMul#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE),8)) dataline0=data[0].read_response;
			Bit#(TMul#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE),8)) dataline1=data[1].read_response;
			Bit#(TMul#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE),8)) dataline2=data[2].read_response;
			Bit#(TMul#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE),8)) dataline3=data[3].read_response;

			Bit#(`DCACHE_WAYS) valid_values={tag[3].read_response[20],tag[2].read_response[20],tag[1].read_response[20],tag[0].read_response[20]};
			Bit#(`DCACHE_WAYS) dirty_values={tag[3].read_response[21],tag[2].read_response[21],tag[1].read_response[21],tag[0].read_response[21]};
						
			if(cpu_tag==tag[0].read_response[19:0] && valid_values[0]==1) tag_hit[0]=1; 
			if(cpu_tag==tag[1].read_response[19:0] && valid_values[1]==1) tag_hit[1]=1; 
			if(cpu_tag==tag[2].read_response[19:0] && valid_values[2]==1) tag_hit[2]=1; 
			if(cpu_tag==tag[3].read_response[19:0] && valid_values[3]==1) tag_hit[3]=1; 
			
			Bool hit=False;
			hit=unpack(|(tag_hit)) && (!hb_hit) && (!lb_hit) && !misaligned_addr;
			// We are not invalidating a replaced line when enquing into the linebuffer.
			// So it is possible that the next request finds this to be a hit and proceeds to change the SRAM.
			// While the linebuffer, having received all the bytes from the memory will simply
			// go ahead and replace the dirty line without eviction. The following condition ensures
			// that for the same index if the SRAM hit is to the same line as the LB treat is as a miss.
			if(hit && tag_hit==lb_way && lb_valid && lb_setindex==setindex)
				hit=False;
			dynamicAssert(!(lb_hit&&hb_hit),"ASSERT: lb_hit and hb_hit are both 1");
			
			Bit#(TMul#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE),8)) temp0=duplicate(tag_hit[0]&pack(hit));
			Bit#(TMul#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE),8)) temp1=duplicate(tag_hit[1]&pack(hit));
			Bit#(TMul#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE),8)) temp2=duplicate(tag_hit[2]&pack(hit));
			Bit#(TMul#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE),8)) temp3=duplicate(tag_hit[3]&pack(hit));
			Bit#(TMul#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE),8)) hitline0=temp0&dataline0;
			Bit#(TMul#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE),8)) hitline1=temp1&dataline1;
			Bit#(TMul#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE),8)) hitline2=temp2&dataline2;
			Bit#(TMul#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE),8)) hitline3=temp3&dataline3;
			Bit#(TMul#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE),8)) sram_dataline=hitline0|hitline1|hitline2|hitline3;
			`ifdef verbose $display($time,"\tDCACHE: valid_values: %b dirty_values: %b stall_on_lb: %b we: %h Access_type: ",valid_values,dirty_values,stall_on_lb,rg_writeenable,fshow(rg_access_type)); `endif
			/*================================================*/
			/*===== replacement line selection ==============*/
			Bit#(`DCACHE_WAYS) replace_vec=valid_values;
			if(&(valid_values)==1)
				replace_vec=dirty_values;
			case (replace_vec) matches
				'b???0:replace_vec='b0001;
				'b??01:replace_vec='b0010;
				'b?011:replace_vec='b0100;
				'b0111:replace_vec='b1000;
				default:begin 
					replace_vec=0;
	 				replace_vec[random_line.value]=1;
					random_line.next;
				end
			endcase
			if(replace_vec==lb_way && lb_setindex==setindex && lb_valid)
				replace_vec=rotateBitsBy(lb_way,1);
			if(replace_vec==hb_way && hb_valid && hb_setindex==setindex)
				replace_vec=rotateBitsBy(replace_vec,1);
			`ifdef verbose $display($time,"\tDCACHE: replacevec: %b hb_way: %b lb_way: %b",replace_vec,hb_way,lb_way);	`endif
			`ifdef verbose $display($time,"\tDCACHE: CPUTAG: %h lb_tag: %h hb_tag :%h",cpu_tag,lb_tag,hb_tag); `endif
			`ifdef verbose $display($time,"\tDCACHE: CPUIndex: %d lb_index: %d hb_inex :%d",setindex,lb_setindex,hb_setindex); `endif
			
			Bit#(TAdd#(TLog#(`DCACHE_WORD_SIZE),TLog#(`DCACHE_BLOCK_SIZE))) offset_zeros='d0;
			Bit#(`PADDR) r0=duplicate(replace_vec[0]);
			Bit#(`PADDR) r1=duplicate(replace_vec[1]);
			Bit#(`PADDR) r2=duplicate(replace_vec[2]);
			Bit#(`PADDR) r3=duplicate(replace_vec[3]);
			Bit#(`PADDR) write_address0=r0&{tag[0].read_response[20-1:0],setindex[6:0],offset_zeros};
			Bit#(`PADDR) write_address1=r1&{tag[1].read_response[20-1:0],setindex[6:0],offset_zeros};
			Bit#(`PADDR) write_address2=r2&{tag[2].read_response[20-1:0],setindex[6:0],offset_zeros};
			Bit#(`PADDR) write_address3=r3&{tag[3].read_response[20-1:0],setindex[6:0],offset_zeros};
			Bit#(`PADDR) write_address = write_address0 | write_address1 | write_address2 | write_address3;
			Bit#(TMul#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE),8)) replace_dataline =
				case(replace_vec)
					'b0001:dataline0; 
					'b0010:dataline1; 
					'b0100:dataline2;
					'b1000:dataline3;
					default:0;
				endcase;
			`ifdef verbose $display($time,"\tDCACHE: Replace vec: %h line: %h address :%h",replace_vec,replace_dataline, write_address); `endif
			/*==============================================*/

			/*==== capture the word to be operated on and perform the atomic operation as well on it=======*/
			Bit#(TMul#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE),8)) dataline=hbdataline|lbdataline|sram_dataline;
			Bit#(`Reg_width) data_word=(dataline>>{6'd0,word_offset}*64)[`Reg_width-1:0];
			data_word=data_word>>({4'b0,byte_offset}*8);

			if(!rg_signextend)
				data_word=rg_transfer_size==0?zeroExtend(data_word[7:0]):rg_transfer_size==1?zeroExtend(data_word[15:0]):rg_transfer_size==2?zeroExtend(data_word[31:0]):data_word;
			else
				data_word=rg_transfer_size==0?signExtend(data_word[7:0]):rg_transfer_size==1?signExtend(data_word[15:0]):rg_transfer_size==2?signExtend(data_word[31:0]):data_word;
			`ifdef atomic 
				let {success,storeResult,atomicdata} <- atomic_operation(data_word,rg_writedata,rg_atomic_op,rg_paddress); 
				if(rg_access_type==Load)
					storeResult=False;
				if(success matches tagged Valid .sc)
					data_word = zeroExtend(sc);
			`endif
			Bit#(`Reg_width) final_word = `ifdef atomic (rg_access_type==Atomic)?atomicdata: `endif (rg_access_type==Store)?rg_writedata:data_word;
			`ifdef verbose $display($time,"\tDCACHE: hbhit: %b hbdataline: %h",hb_hit,hbdataline); `endif
			`ifdef verbose $display($time,"\tDCACHE: lb_hit: %b lbdataline: %h",lb_hit,lbdataline); `endif
			`ifdef verbose $display($time,"\tDCACHE: tag_hit: %b hit  : %b srdataline: %h",tag_hit,hit , sram_dataline); `endif
			`ifdef verbose $display($time,"\tDCACHE: Sending to Core: %h Final line: %h",data_word,dataline); `endif
			`ifdef verbose $display($time,"\tDCACHE: translation done: %b tlb_exception: ",rg_trnslte_done[0],fshow(rg_tlb_exception[0])); `endif
			/*=============================================================================================*/
			/*============ perform Store/Atomic operations =============*/
			if(rg_trnslte_done[0] &&& rg_tlb_exception[0] matches tagged None)begin
			if(cache_enabled)begin
				/*======= Calculate the next state ===*/
				DcacheState nextstate=Idle;
				if(misaligned_addr || hit || hb_hit || (lb_hit && !stall_on_lb))
					nextstate=Idle;
				else if(lb_hit && stall_on_lb || (!hit && !lb_hit &&!hb_hit))begin
					nextstate=KeepPolling;
					rg_poll_address<=rg_paddress;
				end
				if(nextstate==Idle)
					rg_trnslte_done[0]<=False;
				rg_state[0]<=nextstate;
				`ifdef verbose $display($time,"\tDCACHE: NextState: ",fshow(nextstate)); `endif
				/*=====================================*/
				/*========= response to CPU =============*/
				if(rg_access_type!=Store && nextstate==Idle)
					wr_response_to_cpu<= tagged Valid (tuple4(data_word,exception,0,rg_insn_epoch));// TODO perf
				else if(nextstate==Idle)
					wr_response_to_cpu<= tagged Valid (tuple4(0,exception,0,rg_insn_epoch)); // TODO perf
				
				if(exception matches tagged None)
					wbEpoch[0]<=wbEpoch[0];
				else
					wbEpoch[0]<=~wbEpoch[0];
				/*=======================================*/
				
				if(rg_access_type==Store `ifdef atomic || storeResult `endif )
					rg_global_dirty<=True;
				/*=============== updated hit buffer on a write =========*/
				if(hb_hit && (rg_access_type==Store `ifdef atomic || storeResult `endif ))begin
					`ifdef verbose $display($time,"\tDCACHE: HB Hit. Writing Tag: %h Data: %h Way: %h",hb_tag,final_word,hb_way); `endif
					hb_data.write_portA(rg_writeenable,duplicate(final_word)); 
				end
				/*============================================*/
				/*=============== updated line buffer on a write =========*/
				if(lb_hit && !stall_on_lb && (rg_access_type==Store `ifdef atomic || storeResult `endif ))begin
					`ifdef verbose $display($time,"\tDCACHE: LB Hit. Writing Tag: %h Data: %h Way: %h setindex: %d",lb_tag,final_word,lb_way,lb_setindex); `endif
					lb_data.write_portA(rg_writeenable,duplicate(final_word)); 
					lb_dirty<=1;
				end
				if(hit && (rg_access_type==Store `ifdef atomic || storeResult `endif ) && !hb_hit)begin
					`ifdef verbose $display($time,"\tDCACHE: Hit in SRAMS and writing new value :%h to HB",update_line(rg_writeenable,duplicate(final_word),dataline)); `endif
					hb_tag<=cpu_tag;
					hb_setindex<=setindex;
					hb_data.write_portA('1,update_line(rg_writeenable,duplicate(final_word),dataline));
					hb_way<=tag_hit;
				end
				if(hit && (rg_access_type==Store `ifdef atomic || storeResult `endif ) && !hb_hit)
					hb_valid<=True;
				else if(hb_valid && !hb_hit)
					hb_valid<=False;
				/*=============== updated SRAM entries with Hit buffer when possible =========*/
				if(hb_valid &&!hb_hit)begin
					`ifdef verbose $display($time,"\tDCACHE: HB updating SRAM Tag: %h Data: %h Way: %h setindex: %d",hb_tag,hb_data.response_portA,hb_way,hb_setindex); `endif
					wr_write_info<=tagged Valid tuple2(hb_tag,hb_setindex);
					for(Integer i=0;i<`DCACHE_WAYS;i=i+1)begin	
						tag[i].write_request(unpack(hb_way[i]),hb_setindex,{2'b11,hb_tag});
						data[i].write_request(duplicate(hb_way[i]),hb_setindex,hb_data.response_portA);
					end
				end
				/*============================================================================*/
				if(!hit && !lb_hit && !hb_hit && !misaligned_addr)begin// a complete miss 
					`ifdef verbose $display($time,"\tDCACHE: A complete miss in Data Cache. Enquing into the memoperation FIFO"); `endif
					Bit#(TLog#(`DCACHE_BLOCK_SIZE)) val1=(rg_vaddress&'hfffffff8)[word_bits+byte_bits-1:byte_bits];
					Bit#(TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE)) writeenable='hFF;
					writeenable=writeenable<<{3'b0,val1}*8;
					memoperation.enq(tuple4(cpu_tag,rg_vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits],replace_vec,writeenable));
					ff_read_request_to_memory.enq(To_Memory {address:rg_paddress&'hfffffff8,burst_length:fromInteger(`DCACHE_BLOCK_SIZE),ld_st:Load, transfer_size:3});
					if((valid_values&dirty_values&replace_vec)==replace_vec)begin // if the replacing is dirty
						`ifdef verbose $display($time,"\tDCACHE: Line being replaced is dirty. Addr: %h Data: %h",write_address,replace_vec); `endif
						ff_write_request_to_memory.enq(To_Memory_Write {address:write_address,burst_length:fromInteger(`DCACHE_BLOCK_SIZE),ld_st:Load, transfer_size:3,
								data_line:replace_dataline });
						pending_write_response[0]<=True;
					end
				end
			end
			else if(rg_access_type==Load || rg_access_type==Atomic)begin
				ff_read_request_to_memory.enq(To_Memory {address:rg_paddress,burst_length:1,ld_st:Load,transfer_size:rg_transfer_size});
				rg_state[0]<=IOReadResp;
			end
			else if(rg_access_type==Store)begin
				ff_write_request_to_memory.enq(To_Memory_Write{address:rg_paddress,data_line:zeroExtend(rg_writedata),burst_length:1,transfer_size:rg_transfer_size,ld_st:Store});
				rg_state[0]<=IOWriteResp;
			end
			end
			else if(rg_trnslte_done[0])begin
				rg_state[0]<=Idle;
				wr_response_to_cpu<= tagged Valid (tuple4(0,rg_tlb_exception[0],0,rg_insn_epoch));//TODO perf
				wbEpoch[0]<=~wbEpoch[0];
				rg_tlb_exception[0]<=tagged None;
				rg_perf_monitor<=0;
				rg_trnslte_done[0]<=False;
				`ifdef verbose $display($time,"\tDCACHE: Exception from TLB taken"); `endif
			end
			else begin
				`ifdef verbose $display($time,"\tDCACHE: Translation not done"); `endif
				rg_state[0] <= Idle;
			end
			/*==========================================================*/
		endrule
		rule wait_for_ioread_response(rg_state[0]==IOReadResp && !memoperation.notEmpty);
			`ifdef verbose $display($time,"\tDCACHE: Received IO Read Response"); `endif
			Bit#(TLog#(`DCACHE_WORD_SIZE)) byte_offset=rg_vaddress[byte_bits-1:0];
			Bit#(`Reg_width) data_value=ff_read_response_from_memory.first.data_line;
			ff_read_response_from_memory.deq;
			data_value=data_value>>({4'b0,byte_offset}*8);
			if(!rg_signextend)
				data_value=rg_transfer_size==0?zeroExtend(data_value[7:0]):rg_transfer_size==1?zeroExtend(data_value[15:0]):rg_transfer_size==2?zeroExtend(data_value[31:0]):data_value;
			else
				data_value=rg_transfer_size==0?signExtend(data_value[7:0]):rg_transfer_size==1?signExtend(data_value[15:0]):rg_transfer_size==2?signExtend(data_value[31:0]):data_value;
			wr_response_to_cpu<=tagged Valid (tuple4(data_value,ff_read_response_from_memory.first.bus_error==1?tagged Exception Load_access_fault:tagged None,rg_perf_monitor,rg_insn_epoch));
			wbEpoch[0]<=ff_read_response_from_memory.first.bus_error==1?~wbEpoch[0]:wbEpoch[0];
			`ifdef atomic
			if(rg_access_type==Atomic)begin
				let {success,storeResult,atomicdata} <- atomic_operation(data_value,rg_writedata,rg_atomic_op,rg_paddress);
				`ifdef MMU
					ff_write_request_to_memory.enq(To_Memory_Write{address:rg_paddress,data_line:zeroExtend(atomicdata),burst_length:1,transfer_size:rg_transfer_size,ld_st:Store});
				`else
					ff_write_request_to_memory.enq(To_Memory_Write{address:truncate(rg_vaddress),data_line:zeroExtend(new_data),burst_length:1,transfer_size:rg_transfer_size,ld_st:Store});
				`endif
				rg_state[0]<=IOWriteResp;
			end
			else
			`endif
			begin
				rg_state[0]<=Idle;
			end
			rg_perf_monitor<=0;
		endrule
		rule wait_for_iowrite_response(rg_state[0]==IOWriteResp && !memoperation.notEmpty && !pending_write_response[2]);
			`ifdef verbose $display($time,"\tDCACHE: Received IO Write Response"); `endif
			ff_write_response_from_memory.deq;
			if(rg_access_type!=Atomic) begin
				wr_response_to_cpu<=tagged Valid (tuple4(0,ff_write_response_from_memory.first.bus_error==1?tagged Exception Store_access_fault:tagged None,rg_perf_monitor,rg_insn_epoch));
				wbEpoch[0]<=ff_write_response_from_memory.first.bus_error==1?~wbEpoch[0]:wbEpoch[0];
			end
			rg_perf_monitor<=0;
			rg_state[0]<=Idle;
		endrule
		method Action virtual_address(Bit#(`VADDR) vaddress, Access_type load_store, Bit#(TMul#(`DCACHE_WORD_SIZE,8)) writedata, Bit#(3) transfer_size, `ifdef atomic Bit#(5) atomic_op, `endif Bool signextend, Bit#(1) insnepoch) if(rg_state[1]==Idle);
			if((transfer_size=='b01 && vaddress[0]!='b0) || (transfer_size=='b10 && vaddress[1:0]!=0) || (transfer_size=='b11 && vaddress[2:0]!=0))
				misaligned_addr<=True;
			else
				misaligned_addr<=False;
			Bit#(`PERFMONITORS) perf_monitor=0;
			Bit#(TLog#(`DCACHE_SETS)) setindex=vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits];
			`ifdef verbose $display($time,"\tDCACHE: ",fshow(load_store)," Request of VAddr: %h transfersize: %d signextend: %b setindex: %d data:%h",vaddress,transfer_size, signextend,setindex,writedata); `endif
			Bit#(TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE)) we=transfer_size==0?'b1:transfer_size==1?'b11:transfer_size==2?'hF:'hFF;
			Bit#(TLog#(`DCACHE_BLOCK_SIZE)) word_offset= vaddress[word_bits+byte_bits-1:byte_bits];
			Bit#(TLog#(`DCACHE_WORD_SIZE)) byte_offset=vaddress[byte_bits-1:0];
			we=we<<{4'b0,word_offset}*8;
			we=we<<byte_offset;
			rg_access_type<=load_store;
			Bool proceed=True;
			rg_vaddress<=vaddress;
			rg_transfer_size<=transfer_size;
			`ifdef atomic rg_atomic_op<=atomic_op; `endif
			rg_writedata<=transfer_size==0?duplicate(writedata[7:0]):transfer_size==1?duplicate(writedata[15:0]):transfer_size==2?duplicate(writedata[31:0]):writedata;
			rg_writeenable<=we;
			rg_signextend<=signextend;
			rg_insn_epoch<=insnepoch;
			if(wr_write_info matches tagged Valid .x)begin
				let {newtag,newindex}=x;
				if(newindex==setindex)
					proceed=False;
			end
			if(load_store==Fence)begin
				rg_state[1]<=FenceStart;
			end
			else begin
				if(proceed)begin
					for(Integer i=0;i<`DCACHE_WAYS;i=i+1)begin // send address to the Block_rams
						tag[i].read_request(setindex);
						data[i].read_request(setindex);
					end
					rg_state[1]<=ReadingCache;
				end
				else begin
					capture_counters<=True;
					rg_state[1]<=Stall1;
				end
			end
		endmethod
		method Maybe#(Tuple4#(Bit#(`Reg_width), Trap_type,Bit#(`PERFMONITORS),Bit#(1))) response_to_core;
			return wr_response_to_cpu;
		endmethod
		`ifdef MMU
			method Action physical_address(Bit#(`PADDR) paddr, Trap_type exception);
				`ifdef verbose $display($time,"\tDCACHE: Sending physical address %h to dcache ",paddr); `endif
				rg_paddress<=paddr;
				rg_trnslte_done[1] <= True;
				rg_tlb_exception[1]<=exception;
			endmethod
		`endif
		method ActionValue#(To_Memory#(`PADDR)) read_request_to_memory;
			ff_read_request_to_memory.deq;
			return ff_read_request_to_memory.first;
		endmethod
		method ActionValue#(To_Memory_Write) write_request_to_memory;
			ff_write_request_to_memory.deq;
			return ff_write_request_to_memory.first;
			endmethod
		method Bool init_complete;
			return (rg_state[0]!=Fence);	
		endmethod
		method Action read_response_from_memory(From_Memory#(`DCACHE_WORD_SIZE) resp);
			`ifdef verbose $display($time,"\tDCACHE: Memory has responded"); `endif
			ff_read_response_from_memory.enq(resp);
		endmethod
		method Action write_response_from_memory(From_Memory#(`DCACHE_WORD_SIZE) resp);
			ff_write_response_from_memory.enq(resp);
		endmethod
		method Action flush_from_wb;
			`ifdef verbose $display($time,"\tDCACHE: Inverting the wbEPOCH due to WB stage flush"); `endif
			wbEpoch[1]<=~wbEpoch[1];
		endmethod
	endmodule
endpackage
