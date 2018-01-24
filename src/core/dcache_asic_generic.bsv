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
	/*===== project imports==== */
	import defined_types::*;
	`include "defined_parameters.bsv"
	import QuadMem::*;
	/*========================= */


	interface Ifc_dcache;
		method Action virtual_address(Bit#(`VADDR) vaddress, Access_type load_store, Bit#(TMul#(`DCACHE_WORD_SIZE,8)) writedata, Bit#(3) transfer_size, Bit#(5) atomic_op, Bool signextend);
		method Maybe#(Tuple3#(Bit#(`Reg_width), Trap_type,Bit#(`PERFMONITORS))) response_to_core;
		method ActionValue#(To_Memory#(`PADDR)) read_request_to_memory;
		method ActionValue#(To_Memory_Write) write_request_to_memory;
		method Action read_response_from_memory(From_Memory#(`DCACHE_WORD_SIZE) resp);
		method Action write_response_from_memory(From_Memory#(`DCACHE_WORD_SIZE) resp);
		method Bool init_complete;
		`ifdef MMU
			method Action physical_address(Bit#(`PADDR) paddr, Trap_type exception);
		`endif
	endinterface

	typedef enum {Idle,KeepPolling,Stall1,Initialize,ReadingCache,Fence,IOReadResp,IOWriteResp} DcacheState deriving (Bits,Eq,FShow);

	(*synthesize*)
	`ifdef MMU (*conflict_free ="virtual_address, physical_address"*) `endif
	(*preempts="virtual_address,read_from_lbdata_into_hold_reg"*)
	(*preempts="read_data_fromcache,read_from_lbdata_into_hold_reg"*)
	module mkdcache(Ifc_dcache);
		/* VAddr = [tag_bits|set_bits|word_bits|byte_bits] */
		let byte_bits=valueOf(TLog#(`DCACHE_WORD_SIZE));	// number of bits to select a byte within a word. = 2
		let word_bits=valueOf(TLog#(`DCACHE_BLOCK_SIZE));	// number of bits to select a word within a block. = 4
		let set_bits=valueOf(TLog#(`DCACHE_SETS));			// number of bits to select a set from the cache. = 
		Reg#(Maybe#(Tuple2#(Bit#(1),Bit#(`PADDR)))) rg_lr_paddress<-mkReg(tagged Invalid);
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
				'b0000: atomic_result= (op1+op2);
				'b0010:	atomic_result= (op1^op2);
				'b0110:	atomic_result= (op1&op2);
				'b0100: atomic_result= (op1|op2);
				'b1100:	atomic_result= min(op1,op2);
				'b1110:	atomic_result= max(op1,op2);
				'b1000:	atomic_result= pack(min(s_op1,s_op2));
				'b1010:	atomic_result= pack(max(s_op1,s_op2));
				'b0101: action begin 
									rg_lr_paddress <= tagged Valid tuple2(atomic_op[4],addr);
									atomic_result=loaded_value; // LR
									store_result = False;
								end
								endaction
				'b0111: begin
									atomic_result=rs2;			  // SC
									sc_done = tagged Valid 1;
									store_result = False;
									`ifdef verbose $display($time,"\tDCACHE: store condition instruction"); `endif
									if(rg_lr_paddress matches tagged Valid .lr) begin
										let {x,y} = lr;
										if(x==atomic_op[4] && addr== y) begin
											`ifdef verbose $display($time,"\tDCACHE: store condition satisfied"); `endif
											sc_done = tagged Valid 0;
											rg_lr_paddress <= tagged Invalid;
											store_result = True;
									  end
									end
								end
				default:		atomic_result= op1;
			endcase	
			if(atomic_op[4]==1)
					atomic_result=duplicate(atomic_result[31:0]);

			return tuple3(sc_done,store_result,atomic_result);
			endactionvalue );
		endfunction
//		BRAM_DUAL_PORT#(Bit#(TLog#(`DCACHE_SETS)),Bit#(TAdd#(20,2))) tag [`DCACHE_WAYS];
//		BRAM_DUAL_PORT_BE#(Bit#(TLog#(`DCACHE_SETS)),Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)),64) data [`DCACHE_WAYS];
		Ifc_dcache_data data [`DCACHE_WAYS];
		Ifc_dcache_tag	 tag  [`DCACHE_WAYS];
		for(Integer i=0;i<`DCACHE_WAYS;i=i+1)begin
			tag[i] <- mkdcache_tag;		
			data[i] <-mkdcache_data;
		end
		Ifc_QuadMem lbdata <-mkQuadMem;

		LFSR#(Bit#(2)) random_line<-mkRCounter(3);								// for random line replacement
		/* storage for requests from the cpu */
		Reg#(Bool) rg_global_dirty[2] <-mkCReg(2,False);
		Reg#(Bit#(`VADDR)) rg_vaddress<-mkReg(0);
		Reg#(Bit#(3)) rg_transfer_size<-mkReg(0);
		Reg#(Bit#(5)) rg_atomic_op<-mkReg(0);
		Reg#(Bit#(TMul#(`DCACHE_WORD_SIZE,8))) rg_writedata<-mkReg(0);
		Reg#(Access_type) rg_load_store<-mkReg(Load);
		Reg#(Bit#(TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE))) rg_writeenable<-mkReg(0);
		Reg#(Bool) rg_signextend<-mkReg(False);
		Reg#(Bool) update_data_from_lb[2]<-mkCReg(2,False);
		Reg#(Bool) hold_data_from_lb<-mkDReg(False);
		Reg#(Bit#(1)) lb_dirty <-mkReg(0);
		/*=================================== */
		/* storage for physical translation */
		`ifdef MMU
		Reg#(Bit#(`PADDR)) rg_paddress<-mkReg(0);
		Reg#(Bool)	rg_trnslte_done[2] <- mkCReg(2, `ifdef MMU False `else True `endif );
		Reg#(Trap_type) rg_tlb_exception<-mkReg(tagged None);
		`endif
		/*==================================== */

		Reg#(Bit#(`PERFMONITORS)) rg_perf_monitor<-mkReg(0);
		Reg#(DcacheState) rg_state[3]<-mkCReg(3,Initialize);				// this needs to be a CReg so that request can fire in the same cycle as response
		Reg#(Bit#(TLog#(`DCACHE_SETS))) set_index <-mkReg(0);
		Reg#(Bit#(TLog#(`DCACHE_WAYS))) way_index <-mkReg(0);
		Reg#(Bit#(TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE))) rg_we<-mkReg(0);
		Reg#(Bit#(TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE))) line_bytes_written<-mkReg(0);

		Wire#(Maybe#(Tuple2#(Bit#(20),Bit#(TLog#(`DCACHE_SETS))))) wr_write_info<-mkDWire(tagged Invalid);	
		Wire#(Maybe#(Tuple3#(Bit#(`Reg_width), Trap_type, Bit#(`PERFMONITORS)))) wr_response_to_cpu<-mkDWire(tagged Invalid);
		FIFOF#(To_Memory#(`PADDR)) ff_read_request_to_memory <-mkLFIFOF();
		FIFOF#(To_Memory_Write) ff_write_request_to_memory <-mkLFIFOF();
		FIFOF#(From_Memory#(`DCACHE_WORD_SIZE)) ff_read_response_from_memory <-mkSizedBypassFIFOF(1);
		FIFOF#(From_Memory#(`DCACHE_WORD_SIZE)) ff_write_response_from_memory <-mkSizedBypassFIFOF(1);
		FIFOF#(Tuple4#(Bit#(20),Bit#(TLog#(`DCACHE_SETS)),Bit#(TLog#(`DCACHE_WAYS)),Bit#(TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE)))) memoperation <-mkUGSizedFIFOF(2);
		Reg#(Bool) increment_counters <-mkReg(True);
		Reg#(Bool) capture_counters <-mkDReg(False);
		Reg#(Bool) pending_fence_write_response[2]<-mkCReg(2,False);

		rule display_state;
			`ifdef verbose $display($time,"\tDCACHE: state",fshow(rg_state[0])); `endif
		endrule

		/*====== Invalidate all the entries in the cache on startup or during Fence ==== */
		rule fencing_the_cache(rg_state[0]==Initialize && !memoperation.notEmpty);
			`ifdef verbose $display($time,"\tDCACHE: Initializing index: %d",set_index," ",fshow(rg_load_store)); `endif
			for(Integer i=0;i<`DCACHE_WAYS;i=i+1)begin
				tag[i].write_request(truncate(set_index),0);
			end
			if(set_index==fromInteger(`DCACHE_SETS-1)) begin
				rg_state[0]<=Idle;
				set_index<=0;
				way_index<=0;
				random_line.seed('d3);
				rg_global_dirty[0]<=False;
				rg_trnslte_done[1]<=False;
				if(rg_load_store==Fence)
					wr_response_to_cpu<= tagged Valid (tuple3(0,tagged None,0));
			end
			else
				set_index<=set_index+1;
		endrule
		rule deq_write_response_during_fence(pending_fence_write_response[0]);
			ff_write_response_from_memory.deq;
			pending_fence_write_response[0]<=False;
		endrule
		/*=============================================================================== */
		rule handle_fence(rg_state[0]==Fence &&!memoperation.notEmpty);
			Bit#(20) tag_values=tag[way_index].read_response[20-1:0];	// hold the tag values
			Bit#(1) dirty_value=tag[way_index].read_response[20+1];		// holds the dirty bits
			Bit#(1) valid_value=tag[way_index].read_response[20];		// holds the dirty bits
			Bit#(TMul#(8,TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE))) data_values; // holds the cache lines.
			Bit#(TAdd#(TLog#(`DCACHE_WORD_SIZE),TLog#(`DCACHE_BLOCK_SIZE))) p_offset =0;
			data_values=data[way_index].read_response;

			Bit#(`PADDR) write_addr={tag_values,truncate(set_index),p_offset};
			`ifdef verbose $display($time,"\tDCACHE: Handling Fence.tag %h setindex: %d way_index: %d Dirty: %b Valid: %b",tag_values,set_index,way_index,dirty_value,valid_value); `endif
			`ifdef verbose $display($time,"\tDCACHE: Fence addr: %h line: %h ",write_addr,data_values); `endif 
			Bit#(TLog#(`DCACHE_SETS)) new_set=set_index;
			Bit#(TLog#(`DCACHE_SETS)) old_set=set_index;
			if(!pending_fence_write_response[1])begin
				if(dirty_value==1 && valid_value==1)begin // valid and dirty
					ff_write_request_to_memory.enq(To_Memory_Write { // send the request to memory to 
	            	 address:write_addr,  data_line:data_values,
							burst_length:`DCACHE_BLOCK_SIZE,  transfer_size:3, ld_st:Store});
					pending_fence_write_response[1]<=True;
				end
				if(way_index==fromInteger(`DCACHE_WAYS-1))begin
					new_set=set_index+1;
					if(set_index==fromInteger(`DCACHE_SETS-1))begin
						rg_state[0]<=Idle;
						rg_global_dirty[0]<=False;
						wr_response_to_cpu<= tagged Valid (tuple3(0,tagged None,0));
						rg_trnslte_done[1]<=False;
						set_index<=0;
					end
					else
						set_index<=new_set;
				end
				way_index<=way_index+1;
			end
			tag[way_index+1].read_request(new_set);
			tag[way_index].write_request(old_set,0);
			data[way_index+1].read_request(new_set);
			
		endrule

		(*conflict_free="virtual_address,read_data_fromcache"*)
		rule read_data_fromcache(rg_state[0]==ReadingCache && memoperation.notFull);
			/*========== Check for hit or miss =================== */
			Bit#(TLog#(`DCACHE_WAYS)) linenum=0;
			Bit#(`PERFMONITORS) perf_monitor=rg_perf_monitor;
			Bit#(TMul#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE),8)) dataline=0;
			Bool hit=False;
			Bool lbhit=False;
			Bit#(`DCACHE_WAYS) valid_values=0;		// hold the valid and dirty bits
			Bit#(`DCACHE_WAYS) dirty_values=0;		// hold the valid and dirty bits
			Bit#(TLog#(`DCACHE_BLOCK_SIZE)) word_offset=rg_vaddress[word_bits+byte_bits-1:byte_bits];
			Bit#(TLog#(`DCACHE_WORD_SIZE)) byte_offset=rg_vaddress[byte_bits-1:0];
			Bit#(TLog#(`DCACHE_SETS)) setindex=rg_vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits];
			Bit#(TLog#(`DCACHE_WAYS)) replaceblock=0;
			`ifdef MMU
				Bit#(20) cpu_tag=rg_paddress[`PADDR-1:`PADDR-20];
			`else
				Bit#(20) cpu_tag=rg_vaddress[`PADDR-1:`PADDR-20];
			`endif
			`ifdef MMU
			if(increment_counters)begin
				if(rg_load_store==Load)
					perf_monitor[`TOTAL_LOADS]=1;
				else if(rg_load_store==Store)
					perf_monitor[`TOTAL_STORES]=1;
				else 
					perf_monitor[`TOTAL_ATOMIC]=1;
			end
			if(rg_trnslte_done[0] &&& rg_tlb_exception matches tagged None) begin
				if(!is_IO_Addr(rg_paddress))begin
			`else
				if(!is_IO_Addr(truncate(rg_vaddress)))begin
			`endif
					if(increment_counters)begin
						if(rg_load_store==Load)
							perf_monitor[`DCACHE_CACHEABLE_LOAD]=1;
						else if(rg_load_store==Store)
							perf_monitor[`DCACHE_CACHEABLE_STORE]=1;
						else if(rg_load_store==Atomic)
							perf_monitor[`DCACHE_CACHEABLE_ATOMIC]=1;
					end
					else
						increment_counters<=True;

					valid_values={tag[3].read_response[20],tag[2].read_response[20],tag[1].read_response[20],tag[0].read_response[20]};
					dirty_values={tag[3].read_response[21],tag[2].read_response[21],tag[1].read_response[21],tag[0].read_response[21]};
						
					for(Integer i=0;i<`DCACHE_WAYS;i=i+1)begin
						let stored_tag=tag[i].read_response[19:0];
						let stored_valid=tag[i].read_response[20];
						if(valid_values[i]==0)
							replaceblock=fromInteger(i);
						if(stored_valid==1 && stored_tag==cpu_tag)begin // if a tag matches capture the tag and data
							hit=True;	
							linenum=fromInteger(i);
							dataline=data[i].read_response;
						end
					end
					let linebuffer=lbdata.response_portA;	
					let {lbtag,lbset,lbreplaceblock,lbwriteenable}=memoperation.first;
					if(memoperation.notEmpty && lbset==setindex && lbtag==cpu_tag)begin
						dataline=linebuffer;
						lbhit=True;
					end
					`ifdef verbose $display($time,"DCACHE: DATALINE: %h",dataline); `endif
					Bit#(`Reg_width) data_value=(dataline>>{6'd0,word_offset}*64)[`Reg_width-1:0];
					data_value=data_value>>({4'b0,byte_offset}*8);
					if(!rg_signextend)
						data_value=rg_transfer_size==0?zeroExtend(data_value[7:0]):rg_transfer_size==1?zeroExtend(data_value[15:0]):rg_transfer_size==2?zeroExtend(data_value[31:0]):data_value;
					else
						data_value=rg_transfer_size==0?signExtend(data_value[7:0]):rg_transfer_size==1?signExtend(data_value[15:0]):rg_transfer_size==2?signExtend(data_value[31:0]):data_value;

					/*====================================================== */
					/*=========== Respond to Core ============================ */
					if((rg_transfer_size=='b01 && rg_vaddress[0]!='b0) || (rg_transfer_size=='b10 && rg_vaddress[1:0]!=0) || (rg_transfer_size=='b11 && rg_vaddress[2:0]!=0))begin // miss-aligned error.
						perf_monitor[`DCACHE_MISALIGNED]=1; // cache mis-aligned error.
						if(rg_load_store==Load)
							wr_response_to_cpu<= tagged Valid (tuple3(0,tagged Exception Load_addr_misaligned,perf_monitor));
						else 
							wr_response_to_cpu<=tagged Valid (tuple3(0,tagged Exception Store_addr_misaligned,perf_monitor));
						rg_state[0]<=Idle;
						rg_perf_monitor<=0;
						`ifdef MMU rg_trnslte_done[0] <= False; `endif
					end
					else if(hit||lbhit)begin // if there has been a hit.
						let {success,storeResult,newdata} <- atomic_operation(data_value,rg_writedata,rg_atomic_op,rg_paddress);
						if(rg_load_store==Load)
							storeResult=False;
						if(success matches tagged Valid .sc)
							data_value = zeroExtend(sc);
						if(lbhit && (line_bytes_written & rg_writeenable) != rg_writeenable)begin
							rg_state[0]<=KeepPolling;
							rg_perf_monitor<=perf_monitor;
						`ifdef verbose 	$display($time,"\tDCACHE: Going to poll LB: %h we: %h",line_bytes_written,rg_writeenable); `endif
						end
						else begin	
							if(rg_load_store==Store)
								data_value=0;
							`ifdef verbose $display($time,"\tDCACHE: Hit for ",fshow(rg_load_store)," address : %h data: %h line: %d rg_writedata: %h rg_writeenable: %h lbhit: %b atomic_data %h storeResult %b",rg_vaddress,data_value,linenum,rg_writedata,rg_writeenable, lbhit, newdata, storeResult); `endif
							wr_response_to_cpu<=tagged Valid (tuple3(data_value,tagged None,perf_monitor));
							rg_trnslte_done[0] <= False;
							rg_perf_monitor<=0;
							rg_state[0]<=Idle;
							if(rg_load_store==Store || storeResult)begin //Atomic but not LR
								`ifdef verbose $display("Store or atomic kuch toh ho raha hai"); `endif
								wr_write_info<=tagged Valid tuple2(cpu_tag,setindex);
								if(lbhit)begin
									if(rg_load_store==Store)
										lbdata.write_portA(rg_writeenable,duplicate(rg_writedata));
									else
										lbdata.write_portA(rg_writeenable,duplicate(newdata));
									`ifdef verbose if(line_bytes_written!='1)
										$display("WRITING ON BOTH PORTS OF LB"); `endif
									lb_dirty<=1;
								end
								else begin
									tag[linenum].write_request(rg_vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits],{2'b11,tag[linenum].read_response[19:0]});
									if(rg_load_store==Store)
										data[linenum].write_request(rg_writeenable,rg_vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits],duplicate(rg_writedata));
									else
										data[linenum].write_request(rg_writeenable,rg_vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits],duplicate(newdata));
								end
								rg_global_dirty[0]<=True;
							end
						end
					end
					/*====================================================== */
					/*==== Request to memory =============================== */
					else begin // miss
						rg_state[0]<=KeepPolling;
						if(rg_load_store==Load)
							perf_monitor[`DCACHE_LOAD_MISS]=1;
						else if(rg_load_store==Store)
							perf_monitor[`DCACHE_STORE_MISS]=1;
						else if(rg_load_store==Atomic)
							perf_monitor[`DCACHE_ATOMIC_MISS]=1;

						if(valid_values=='1)begin // if all the lines are valid and no match then replace line
							perf_monitor[`DCACHE_LINEREPLACE]=1; // cache line replacement increment.
							if(dirty_values[0]==0)
								replaceblock=0;
							else if(dirty_values[1]==0)
								replaceblock=1;
							else if(dirty_values[2]==0)
								replaceblock=2;
							else if(dirty_values[3]==0)
								replaceblock=3;
							else begin
								replaceblock=truncate(random_line.value);
								random_line.next;
							end
							`ifdef verbose $display($time,"\tDCACHE: Miss of ",fshow(rg_load_store)," address: %h Replacing line: %d valid: %b dirty_values: %b",rg_vaddress,replaceblock,valid_values,dirty_values); `endif
						end
						else begin
							`ifdef verbose $display($time,"\tDCACHE: Miss of ",fshow(rg_load_store)," address: %h Filling line: %d",rg_vaddress,replaceblock); `endif
						end
						if(memoperation.notEmpty && lbset==setindex && replaceblock==lbreplaceblock)begin
							replaceblock=replaceblock+1;
						end
							
						`ifdef MMU
							ff_read_request_to_memory.enq(To_Memory {address:rg_paddress&'hfffffff8,burst_length:fromInteger(`DCACHE_BLOCK_SIZE),ld_st:Load, transfer_size:3});
						`else
							ff_read_request_to_memory.enq(To_Memory {address:truncate(rg_vaddress&'hfffffff8),burst_length:fromInteger(`DCACHE_BLOCK_SIZE),ld_st:Load, transfer_size:3});
						`endif
						Bit#(TLog#(`DCACHE_BLOCK_SIZE)) val1=(rg_vaddress&'hfffffff8)[word_bits+byte_bits-1:byte_bits];
						Bit#(TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE)) writeenable='hFF;
						writeenable=writeenable<<{3'b0,val1}*8;
						if(dirty_values[replaceblock]==1)begin // if the replacing is dirty
							perf_monitor[`DCACHE_WRITEBACKS]=1;
							Bit#(TAdd#(TLog#(`DCACHE_WORD_SIZE),TLog#(`DCACHE_BLOCK_SIZE))) offset_zeros='d0;
							Bit#(`PADDR) write_address={tag[replaceblock].read_response[20-1:0],setindex[6:0],offset_zeros};
							`ifdef verbose $display($time,"\tDCACHE: Line being replaced is dirty. Addr: %h Data: %h",write_address,data[replaceblock].read_response); `endif
							ff_write_request_to_memory.enq(To_Memory_Write {address:write_address,burst_length:fromInteger(`DCACHE_BLOCK_SIZE),ld_st:Load, transfer_size:3,
									data_line:data[replaceblock].read_response });
								pending_fence_write_response[0]<=True;
						end
						memoperation.enq(tuple4(cpu_tag,rg_vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits],replaceblock,writeenable));
						`ifdef verbose $display($time,"\tDCACHE: mask: %h byteoffset: %h",writeenable,val1); `endif
						rg_perf_monitor<=perf_monitor;
					end
				end
				else begin
					if(rg_load_store==Load || rg_load_store==Atomic)begin
						`ifdef MMU
							ff_read_request_to_memory.enq(To_Memory {address:rg_paddress,burst_length:1,ld_st:Load,transfer_size:rg_transfer_size});
						`else
							ff_read_request_to_memory.enq(To_Memory {address:truncate(rg_vaddress),burst_length:1,ld_st:Load,transfer_size:rg_transfer_size});
						`endif
						rg_state[0]<=IOReadResp;
					end
					else if(rg_load_store==Store)begin
						`ifdef verbose $display($time,"\tDCACHE: Sending IO Write REQUEST"); `endif
						`ifdef MMU
							ff_write_request_to_memory.enq(To_Memory_Write{address:rg_paddress,data_line:zeroExtend(rg_writedata),burst_length:1,transfer_size:rg_transfer_size,ld_st:Store});
						`else
							ff_write_request_to_memory.enq(To_Memory_Write{address:truncate(rg_vaddress),data_line:zeroExtend(rg_writedata),burst_length:1,transfer_size:rg_transfer_size,ld_st:Store});
						`endif
						rg_state[0]<=IOWriteResp;
					end
				end
			`ifdef MMU
			end
			else if(rg_trnslte_done[0])begin
				rg_state[0]<=Idle;
				wr_response_to_cpu<= tagged Valid (tuple3(0,rg_tlb_exception,perf_monitor));
				rg_tlb_exception<=tagged None;
				rg_perf_monitor<=0;
				rg_trnslte_done[0]<=False;
				$display($time,"\tDCACHE: Exception from TLB taken");
			end
			else begin
				$display($time,"\tDCACHE: Translation not done");
				rg_state[0] <= Idle;
			end
			`endif
		endrule
		rule wait_for_ioread_response(rg_state[0]==IOReadResp && memoperation.notFull);
			`ifdef verbose $display($time,"\tDCACHE: Received IO Read Response"); `endif
			Bit#(TLog#(`DCACHE_WORD_SIZE)) byte_offset=rg_vaddress[byte_bits-1:0];
			Bit#(`Reg_width) data_value=ff_read_response_from_memory.first.data_line;
			ff_read_response_from_memory.deq;
			data_value=data_value>>({4'b0,byte_offset}*8);
			if(!rg_signextend)
				data_value=rg_transfer_size==0?zeroExtend(data_value[7:0]):rg_transfer_size==1?zeroExtend(data_value[15:0]):rg_transfer_size==2?zeroExtend(data_value[31:0]):data_value;
			else
				data_value=rg_transfer_size==0?signExtend(data_value[7:0]):rg_transfer_size==1?signExtend(data_value[15:0]):rg_transfer_size==2?signExtend(data_value[31:0]):data_value;
			wr_response_to_cpu<=tagged Valid (tuple3(data_value,ff_read_response_from_memory.first.bus_error==1?tagged Exception Load_access_fault:tagged None,rg_perf_monitor));
			if(rg_load_store==Atomic)begin
				let {success,storeResult,newdata} <- atomic_operation(data_value,rg_writedata,rg_atomic_op,rg_paddress);
				`ifdef MMU
					ff_write_request_to_memory.enq(To_Memory_Write{address:rg_paddress,data_line:zeroExtend(newdata),burst_length:1,transfer_size:rg_transfer_size,ld_st:Store});
				`else
					ff_write_request_to_memory.enq(To_Memory_Write{address:truncate(rg_vaddress),data_line:zeroExtend(new_data),burst_length:1,transfer_size:rg_transfer_size,ld_st:Store});
				`endif
				rg_state[0]<=IOWriteResp;
			end
			else begin
				rg_state[0]<=Idle;
			end
			rg_perf_monitor<=0;
		endrule
		rule wait_for_iowrite_response(rg_state[0]==IOWriteResp && !memoperation.notEmpty && !pending_fence_write_response[1]);
			`ifdef verbose $display($time,"\tDCACHE: Received IO Write Response"); `endif
			ff_write_response_from_memory.deq;
			if(rg_load_store!=Atomic)
				wr_response_to_cpu<=tagged Valid (tuple3(0,ff_write_response_from_memory.first.bus_error==1?tagged Exception Store_access_fault:tagged None,rg_perf_monitor));
			rg_perf_monitor<=0;
			rg_state[0]<=Idle;
		endrule
		/*============== One cycle delay to ensure the write is reflected in the BRAM ========= */
		rule stall_the_next_request_by_one_cycle(rg_state[0]==Stall1);
			Bit#(TLog#(`DCACHE_SETS)) setindex=rg_vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits];
			for(Integer i=0;i<`DCACHE_WAYS;i=i+1)begin // send address to the Block_rams
				tag[i].read_request(setindex);
				data[i].read_request(setindex);
			end
			rg_state[0]<=ReadingCache;
		endrule
		/*===================================================================================== */
		/*======= filling up the cache from the data recieved from the external memory ======= */
		(*conflict_free="virtual_address,fillcache"*)
		rule fillcache(memoperation.notEmpty && line_bytes_written!='1);
			let memresp=ff_read_response_from_memory.first;
			ff_read_response_from_memory.deq;
			let {cpu_tag,setindex,replaceblock,writeenable}=memoperation.first;
			`ifdef verbose $display($time,"\tDCACHE: Response from Memory: %h setindex: %d cpu_tag: %h replaceblock: %d",memresp.data_line,setindex,cpu_tag,replaceblock); `endif 
			let we=writeenable;
			if(|line_bytes_written!=0)begin
				we=rg_we;
			end
			Bit#(TMul#(2,TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE))) extended_mask=zeroExtend(we)<<8;
			lbdata.write_portB(we,duplicate(memresp.data_line));
			`ifdef verbose $display($time,"\tDCACHE: linebytes: %h currently writing into: %h",line_bytes_written,we); `endif
			if(memresp.last_word)begin // if all the data words have been fetched exit	
				`ifdef verbose $display($time,"\tDCACHE: Received Last response from Memory set: %d ",setindex); `endif
			end
			line_bytes_written<=line_bytes_written|we;
			rg_we<=extended_mask[2*`DCACHE_BLOCK_SIZE*`DCACHE_WORD_SIZE-1:`DCACHE_BLOCK_SIZE*`DCACHE_WORD_SIZE]|extended_mask[`DCACHE_BLOCK_SIZE*`DCACHE_WORD_SIZE-1:0];
		endrule
		rule read_from_lbdata_into_hold_reg(line_bytes_written=='1);
			let lb_hold_reg=lbdata.response_portB;
			let {cputag,setindex,replaceblock,writeenable}=memoperation.first;
			data[replaceblock].write_request('1,setindex,lb_hold_reg);
			tag[replaceblock].write_request(setindex,{lb_dirty,1'b1,cputag});
			line_bytes_written<=0;
			lb_dirty<=0;
			memoperation.deq;
			`ifdef verbose $display($time,"\tDCACHE: capturing lbdata cpu_tag: %h setindex: %d addr: %h linenum: %d data: %h",cputag, setindex,{cputag,setindex,6'd0}, replaceblock,lb_hold_reg); `endif
			if(rg_state[1]==ReadingCache)
				rg_state[1]<=Stall1;
		endrule
		/*===================================================================================== */
		/*===================================================================================== */
		rule keep_polling_on_stall(rg_state[0]==KeepPolling);
			Bit#(`PERFMONITORS) perf_monitor=rg_perf_monitor;
			if(capture_counters)begin
				$display($time,"\tDCACHE: Miss during polling for ",fshow(rg_load_store));
				if(rg_load_store==Load)begin
					perf_monitor[`DCACHE_LOAD_MISS]=1;
					perf_monitor[`DCACHE_CACHEABLE_LOAD]=1;
				end
				else if(rg_load_store==Store)begin
					perf_monitor[`DCACHE_STORE_MISS]=1;
					perf_monitor[`DCACHE_CACHEABLE_STORE]=1;
				end
				else if(rg_load_store==Atomic) begin
					perf_monitor[`DCACHE_ATOMIC_MISS]=1;
					perf_monitor[`DCACHE_CACHEABLE_ATOMIC]=1;
				end
				rg_perf_monitor<=perf_monitor;
			end
				
			Bit#(TLog#(`DCACHE_SETS)) setindex=rg_vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits];
			`ifdef MMU
				Bit#(20) cpu_tag=rg_paddress[`PADDR-1:`PADDR-20];
			`else
				Bit#(20) cpu_tag=rg_vaddress[`PADDR-1:`PADDR-20];
			`endif
			let {lbtag,lbset,lbreplaceblock,lbwriteenable}=memoperation.first;
			if((line_bytes_written & rg_writeenable) == rg_writeenable && (lbset==setindex && lbtag==cpu_tag))begin
				`ifdef verbose $display($time,"\tDCACHE: Accessing LB"); `endif
				rg_state[0]<=ReadingCache; 
				increment_counters<=False;
				for(Integer i=0;i<`DCACHE_WAYS;i=i+1)begin // send address to the Block_rams
					tag[i].read_request(setindex);
					data[i].read_request(setindex);
				end
			end
			`ifdef verbose $display($time,"\tDCACHE: Polling on LB. cpu_tag: %h lbtag: %h required: %h bytes in Buffer: %h",cpu_tag,lbtag,rg_writeenable,line_bytes_written); `endif
		endrule

		/*============= Prediction in burst mode ================================ */
		method Action virtual_address(Bit#(`VADDR) vaddress, Access_type load_store, Bit#(TMul#(`DCACHE_WORD_SIZE,8)) writedata, Bit#(3) transfer_size, Bit#(5) atomic_op, Bool signextend)if(rg_state[1]==Idle);
			Bit#(`PERFMONITORS) perf_monitor=0;
			Bit#(TLog#(`DCACHE_SETS)) setindex=vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits];
			`ifdef verbose $display($time,"\tDCACHE: ",fshow(load_store)," Request of VAddr: %h transfersize: %d signextend: %b setindex: %d",vaddress,transfer_size, signextend,setindex); `endif
			Bit#(TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE)) we=transfer_size==0?'b1:transfer_size==1?'b11:transfer_size==2?'hF:'hFF;
			Bit#(TLog#(`DCACHE_BLOCK_SIZE)) word_offset= vaddress[word_bits+byte_bits-1:byte_bits];
			Bit#(TLog#(`DCACHE_WORD_SIZE)) byte_offset=vaddress[byte_bits-1:0];
			we=we<<{4'b0,word_offset}*8;
			we=we<<byte_offset;
			rg_load_store<=load_store;
			Bool proceed=True;
			if(wr_write_info matches tagged Valid .x)begin
				let {newtag,newindex}=x;
				if(newindex==setindex && load_store!=Store)
					proceed=False;
			end
			if(load_store==Fence)begin
				if(!rg_global_dirty[1])begin
					rg_state[1]<=Initialize;
				end
				else begin
					tag[0].read_request(0);
					data[0].read_request(0);
					rg_state[1]<=Fence;
				end
			end
			else begin
				rg_vaddress<=vaddress;
				rg_transfer_size<=transfer_size;
				rg_atomic_op<=atomic_op;
				rg_writedata<=transfer_size==0?duplicate(writedata[7:0]):transfer_size==1?duplicate(writedata[15:0]):transfer_size==2?duplicate(writedata[31:0]):writedata;
				rg_writeenable<=we;
				rg_signextend<=signextend;
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
		method Maybe#(Tuple3#(Bit#(`Reg_width), Trap_type,Bit#(`PERFMONITORS))) response_to_core;
			return wr_response_to_cpu;
		endmethod
		`ifdef MMU
			method Action physical_address(Bit#(`PADDR) paddr, Trap_type exception);
				`ifdef verbose $display($time,"\tDCACHE: Sending physical address %h to icache ",paddr); `endif
				rg_paddress<=paddr;
				rg_trnslte_done[1] <= True;
				rg_tlb_exception<=exception;
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
			return (rg_state[1]!=Fence);	
		endmethod
		method Action read_response_from_memory(From_Memory#(`DCACHE_WORD_SIZE) resp);
			`ifdef verbose $display($time,"\tDCACHE: Memory has responded"); `endif
			ff_read_response_from_memory.enq(resp);
		endmethod
		method Action write_response_from_memory(From_Memory#(`DCACHE_WORD_SIZE) resp);
			ff_write_response_from_memory.enq(resp);
		endmethod

	endmodule

	module mkTb(Empty);
		Ifc_dcache dcache<-mkdcache;
		rule send_request;
			dcache.virtual_address('d4,Load,'h01234567ABCDEF89,'d2,'d0,False);
		endrule
		rule terminate;
			let x<-$stime;
			if(x>10)
				$finish(0);
		endrule
	endmodule
endpackage
