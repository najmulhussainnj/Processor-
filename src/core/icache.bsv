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
package icache;
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
	import QuadMem::*;
	import Assert::*;
	import defined_types::*;
	`include "defined_parameters.bsv"
	/*========================= */


	interface Ifc_icache;
		method Action virtual_address(Bit#(`VADDR) vaddress,Bool fence);
		method Maybe#(Tuple3#(Bit#(32), Trap_type, Bit#(`PERFMONITORS))) response_to_core;
		method Action response_from_memory(From_Memory#(`DCACHE_WORD_SIZE) resp);
		method ActionValue#(To_Memory#(`PADDR)) request_to_memory;
		method Action stall_fetch(Bool stall);
		
		`ifdef MMU
			method Action physical_address(Bit#(`PADDR) paddr, Trap_type ex);
		`endif

			method Bit#(`PERFMONITORS) icache_perfmon;
		`ifdef prefetch
			method ActionValue#(Bit#(`VADDR)) prefetch();
		`endif
	endinterface

	typedef enum {Idle,KeepPolling,Stall,ReadingCache,Fence,IOReadResp} IcacheState deriving (Bits,Eq,FShow);

	(*synthesize*)
	(*preempts="read_from_lbdata_into_hold_reg,keep_polling_on_stall"*)
	(*preempts="virtual_address,read_from_lbdata_into_hold_reg"*)
	(*preempts="read_data_fromcache,read_from_lbdata_into_hold_reg"*)
	module mkicache(Ifc_icache);
		/* VAddr = [tag_bits|set_bits|word_bits|byte_bits] */
		let byte_bits=valueOf(TLog#(`ICACHE_WORD_SIZE));	// number of bits to select a byte within a word. = 2
		let word_bits=valueOf(TLog#(`ICACHE_BLOCK_SIZE));	// number of bits to select a word within a block. = 4
		let set_bits=valueOf(TLog#(`ICACHE_SETS));			// number of bits to select a set from the cache. = 

		Ifc_dcache_data data [`ICACHE_WAYS];
		Ifc_dcache_tag	 tag  [`ICACHE_WAYS];
		for(Integer i=0;i<`ICACHE_WAYS;i=i+1)begin
			tag[i] <- mkdcache_tag;		
			data[i] <-mkdcache_data;
		end

		LFSR#(Bit#(2)) random_line<-mkRCounter(3);								// for random line replacement
		Reg#(Bit#(`VADDR)) rg_vaddress<-mkReg(0);
		Reg#(Bit#(`PADDR)) rg_paddress<-mkReg(0);
		Reg#(Trap_type) rg_tlb_exception[2]<-mkCReg(2,tagged None);
		Reg#(Bool)				 rg_trnslte_done[2] <- mkCReg(2, `ifdef MMU False `else True `endif );
		Reg#(Bool)				 rg_stall_fetch <- mkReg(False);

		Reg#(Bit#(`PERFMONITORS)) rg_perf_monitor<-mkReg(0);
		Reg#(IcacheState) rg_state[3]<-mkCReg(3,Fence);				// this needs to be a CReg so that request can fire in the same cycle as response
		Reg#(Bit#(TAdd#(1,TLog#(`ICACHE_SETS)))) rg_index <-mkReg(0);
		Reg#(Bit#(TMul#(`ICACHE_WORD_SIZE,`ICACHE_BLOCK_SIZE))) rg_we<-mkReg(0);
		Reg#(Bit#(TMul#(`ICACHE_WORD_SIZE,`ICACHE_BLOCK_SIZE))) line_bytes_written<-mkReg(0);
		Reg#(Bool) increment_counters <-mkReg(True);
		Reg#(Bool) capture_counters <-mkDReg(False);

		Wire#(Maybe#(Bit#(`VADDR))) wr_memoperation_address <-mkDWire(tagged Invalid);

		Reg#(Bool) ignore_memory_response<-mkReg(False);
		`ifdef prefetch 
			Reg#(Bool) prefetchmode<-mkReg(False); 
			Reg#(Maybe#(Bit#(`VADDR))) rg_prefetchpc<-mkReg(tagged Invalid);
		`endif
		
		Ifc_QuadMem lbdata <-mkQuadMem;
		Wire#(Maybe#(Tuple3#(Bit#(32), Trap_type,Bit#(`PERFMONITORS)))) wr_response_to_cpu<-mkDWire(tagged Invalid);
		FIFOF#(To_Memory#(`PADDR)) ff_request_to_memory <-mkSizedBypassFIFOF(1);
		FIFOF#(From_Memory#(`DCACHE_WORD_SIZE)) ff_response_from_memory <-mkSizedBypassFIFOF(1);
		FIFOF#(Tuple4#(Bit#(`PADDR),Bit#(`VADDR),Bit#(TLog#(`ICACHE_WAYS)),Bit#(TMul#(`ICACHE_WORD_SIZE,`ICACHE_BLOCK_SIZE)))) memoperation <-mkUGSizedFIFOF(2);

		Wire#(Maybe#(Bit#(TLog#(`ICACHE_SETS)))) wr_tag_read_index  <- mkDWire(tagged Invalid);
		Reg#(Maybe#(Bit#(TLog#(`ICACHE_SETS)))) wr_tag_write_index  <- mkDReg(tagged Invalid);

		Wire#(Maybe#(Bit#(TLog#(`ICACHE_SETS)))) wr_data_read_index  <- mkDWire(tagged Invalid);
		Reg#(Maybe#(Bit#(TLog#(`ICACHE_SETS)))) wr_data_write_index  <- mkDReg(tagged Invalid);


		rule display_state;
			`ifdef verbose $display($time,"\tICACHE: State: ",fshow(rg_state[2])); `endif
			`ifdef verbose $display($time,"\tICACHE: translation done %h tlbexception: ", rg_trnslte_done[1], fshow(rg_tlb_exception[1])); `endif
		endrule

		/*====== Invalidate all the entries in the cache on startup or during Fence ==== */
		rule fencing_the_cache(rg_state[0]==Fence && !memoperation.notEmpty);
				rg_we<=0;
			`ifdef verbose $display($time,"\tFencing icache of index %d", rg_index); `endif
			if(rg_index==fromInteger(`ICACHE_SETS)) begin
				if(!rg_stall_fetch) begin
			`ifdef verbose $display($time,"\tFencing icache of is over"); `endif
					rg_state[0]<=Idle;
					rg_index<=0;
					random_line.seed('d3);
				end
			end
			else begin
				for(Integer i=0;i<`ICACHE_WAYS;i=i+1)begin
					tag[i].write_request(True,truncate(rg_index),0);
				end
				rg_index<=rg_index+1;
			end
		endrule
		/*=============================================================================== */
		rule read_data_fromcache(rg_state[0]==ReadingCache && memoperation.notFull);
			/*========== Check for hit or miss =================== */
			Bit#(TLog#(`ICACHE_WAYS)) linenum=0;
			Bit#(`PERFMONITORS) perf_monitor=rg_perf_monitor;
			Bit#(TMul#(TMul#(`ICACHE_BLOCK_SIZE,`ICACHE_WORD_SIZE),8)) dataline=0;
			Bit#(TMul#(TMul#(`ICACHE_BLOCK_SIZE,`ICACHE_WORD_SIZE),8)) dataline_lb=0;
			increment_counters<=True;
			Bool hit=False;
			Bool lbhit=False;
			Bit#(`ICACHE_WAYS) valid_values=0;		// hold the valid and dirty bits
			Bit#(TLog#(`ICACHE_BLOCK_SIZE)) byteoffset=rg_vaddress[word_bits+byte_bits-1:byte_bits];
			Bit#(TLog#(`ICACHE_SETS)) setindex=rg_vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits];
			`ifdef MMU
				Bit#(`ICACHE_TAG_BITS) cpu_tag=rg_paddress[`PADDR-1:`PADDR-`ICACHE_TAG_BITS];
			`else
				Bit#(`ICACHE_TAG_BITS) cpu_tag=rg_vaddress[`PADDR-1:`PADDR-`ICACHE_TAG_BITS];
			`endif
			if(rg_trnslte_done[0]) begin
			`ifdef MMU
				if(rg_tlb_exception[0] matches tagged None)begin
				if(!is_IO_Addr(rg_paddress))begin
			`else
				if(!is_IO_Addr(truncate(rg_vaddress)))begin
			`endif
					`ifdef prefetch
						if(!prefetchmode && increment_counters)
							perf_monitor[`ICACHE_CACHEABLE]=1; // cacheable access increment
						`else
							perf_monitor[`ICACHE_CACHEABLE]=1; // cacheable access increment
					`endif
					for(Integer i=0;i<`ICACHE_WAYS;i=i+1)begin
						let stored_tag=tag[i].read_response[19:0];
						let stored_valid=tag[i].read_response[20];
						valid_values[i]=tag[i].read_response[20];
							
						if(stored_valid==1 && stored_tag==cpu_tag)begin // if a tag matches capture the tag and data
							hit=True;	
							linenum=fromInteger(i);
							dataline=data[i].read_response;
							`ifdef verbose $display($time,"ICACHE: DATALINE: %h",dataline); `endif
						end
					end
					//wr_tag_read_index <= tagged Valid setindex;
					//wr_data_read_index <= tagged Valid setindex;
					Bit#(32) data_value=(dataline>>{5'd0,byteoffset}*32)[31:0];

					let linebuffer=lbdata.response_portA;	
					let {lb_paddress,lb_vaddress,lbreplaceblock,lbwriteenable}=memoperation.first;
					Bit#(`ICACHE_TAG_BITS) lbtag=lb_paddress[`PADDR-1:`PADDR-`ICACHE_TAG_BITS];
					Bit#(TLog#(`ICACHE_SETS)) lbset=lb_vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits];
					if(memoperation.notEmpty && lbset==setindex && lbtag==cpu_tag)begin
						dataline_lb=linebuffer;
						`ifdef verbose $display($time,"\tICACHE: LB BUFFER HIT: data %h",dataline_lb); `endif
						lbhit=True;
						hit=False;
					end 
					Bit#(32) data_value_lb=(dataline_lb>>{5'd0,byteoffset}*32)[31:0];
					
					Bit#(TMul#(`ICACHE_WORD_SIZE,`ICACHE_BLOCK_SIZE)) requested_word=('hF<<({2'd0,byteoffset}*4));
					Bool polling_required= (line_bytes_written & requested_word) != requested_word;
					`ifdef verbose $display($time,"\tICACHE: DATAVALUE: %h DATAVALUELB: %h requested_word: %h line_bytes_written: %h",data_value,data_value_lb,requested_word, line_bytes_written); `endif
					/*====================================================== */
					/*=========== Respond to Core ============================ */
					if(rg_vaddress[1:0]!=0)begin // miss-aligned error.
						perf_monitor[`ICACHE_MISALIGNED]=1; // cache mis-aligned error.
						wr_response_to_cpu<=tagged Valid (tuple3(0,tagged Exception Inst_addr_misaligned,perf_monitor));
						rg_perf_monitor<=0;
						rg_state[0]<=Idle;
						`ifdef prefetch prefetchmode<=False; `endif
						`ifdef MMU rg_trnslte_done[0] <= False; `endif
					end
					else if(hit || (lbhit&&!polling_required))begin // if there has been a hit.
						if(lbhit ) 
							data_value = data_value_lb;
						`ifdef verbose $display($time,"\tICACHE: Hit for address : %h data: %h offset: %h line: %d hit: %b lbhit: %b, polling_required: %b",rg_vaddress,data_value,byteoffset,linenum,hit,lbhit, polling_required); `endif
						`ifdef prefetch
							rg_prefetchpc<=tagged Invalid;
							if(!prefetchmode)begin
								wr_response_to_cpu<=tagged Valid (tuple3(data_value,tagged None,perf_monitor));
								rg_perf_monitor<=0;
								`ifdef MMU rg_trnslte_done[0] <= False; `endif
							end
							else
								prefetchmode<=False;
						`else
							wr_response_to_cpu<=tagged Valid (tuple3(data_value,tagged None,perf_monitor));
							rg_perf_monitor<=0;
							`ifdef MMU rg_trnslte_done[0] <= False; `endif
						`endif
						rg_state[0]<=Idle;
					end
					else if(lbhit && polling_required)begin
						rg_state[0]<=KeepPolling;
					end
					/*====================================================== */
					/*==== Request to memory =============================== */
					else begin // miss
						`ifdef prefetch 
							if(!prefetchmode) begin
								if(rg_vaddress[11:5]!='1)begin // check that prefetch does not cross physical page boundary
									Bit#(`VADDR) mask='1<<(byte_bits+word_bits);
									rg_prefetchpc<=tagged Valid ((rg_vaddress&mask)+('d1<<(word_bits+byte_bits)));
									perf_monitor[`ICACHE_MISS]=1; // cache miss increment.
								end
								rg_state[0]<=KeepPolling;
							end
							else // in prefetch mode send memory request and leave
								rg_state[0]<=Idle;
						`else
								perf_monitor[`ICACHE_MISS]=1; // cache miss increment.
								rg_state[0]<=KeepPolling;
						`endif
						Bit#(TLog#(`ICACHE_WAYS)) replaceblock;
						if(valid_values=='1)begin // if all the lines are valid and no match then replace line
							perf_monitor[`ICACHE_LINEREPLACE]=1; // cache line replacement increment.
							replaceblock=truncate(random_line.value);
							random_line.next;
							`ifdef prefetch
								if(prefetchmode)begin
									`ifdef verbose $display($time,"\tICACHE: Prefetch Miss of address: %h Replacing line: %d valid: %b",rg_vaddress,random_line.value[1:0],valid_values); `endif
									perf_monitor[`ICACHE_PREFETCHMISS]=1;
								end
							`endif
							`ifdef verbose else
								$display($time,"\tICACHE: Miss of address: %h Replacing line: %d valid: %b",rg_vaddress,random_line.value[1:0],valid_values); `endif
						end
						else begin // find the line which is not valid and fill it
							let x=countZerosLSB(valid_values)-1;
							replaceblock=pack(truncate(x));
							`ifdef prefetch
								if(prefetchmode)begin
									`ifdef verbose $display($time,"\tICACHE: Prefetch Miss of address: %h Filling line: %d",rg_vaddress,x); `endif
									perf_monitor[`ICACHE_PREFETCHMISS]=1;
								end
							`endif
							`ifdef verbose else
							$display($time,"\tICACHE: Miss of address: %h Filling line: %d",rg_vaddress,x); `endif
						end
						`ifdef MMU
							ff_request_to_memory.enq(To_Memory {address:truncate(rg_paddress&'hfffffff8),burst_length:fromInteger(`ICACHE_BLOCK_SIZE/2),ld_st:Load, transfer_size:3});
						`else
							ff_request_to_memory.enq(To_Memory {address:truncate(rg_vaddress&'hfffffff8),burst_length:fromInteger(`ICACHE_BLOCK_SIZE/2),ld_st:Load, transfer_size:3});
						`endif
						Bit#(TLog#(`ICACHE_BLOCK_SIZE)) val1=(rg_vaddress&'hfffffff8)[word_bits+byte_bits-1:byte_bits];
						Bit#(TMul#(`ICACHE_WORD_SIZE,`ICACHE_BLOCK_SIZE)) writeenable='hFF;
						writeenable=writeenable<<{3'b0,val1}*4;
						memoperation.enq(tuple4(rg_paddress,rg_vaddress,replaceblock,writeenable));
						`ifdef verbose $display($time,"\tICACHE: mask: %h byteoffset: %h perfmonitors: %h",writeenable,val1,perf_monitor); `endif
						rg_perf_monitor<=perf_monitor;
					end
				end
				else begin
					`ifdef prefetch
						if(prefetchmode)begin
							`ifdef verbose $display($time,"\tICACHE: Do not prefetch in IO space"); `endif
							rg_state[0]<=Idle;
							prefetchmode<=False;
						end
						else `endif
					begin

						`ifdef MMU
							ff_request_to_memory.enq(To_Memory {address:truncate(rg_paddress),burst_length:1,ld_st:Load, transfer_size:2});
						`else
							ff_request_to_memory.enq(To_Memory {address:truncate(rg_vaddress),burst_length:1,ld_st:Load, transfer_size:2});
						`endif
						rg_state[0]<=IOReadResp;
						`ifdef verbose $display($time,"\tICACHE: Sending Address for IO ACCESS: %h",rg_paddress); `endif
					end
				end
			end
			else begin
			`ifdef prefetch
				if(prefetchmode)begin
					`ifdef verbose $display($time,"\tICACHE: do not respond if Prefetch generated a exception"); `endif
					prefetchmode<=False;
				end
				else `endif
				begin
					wr_response_to_cpu<=tagged Valid tuple3(0,rg_tlb_exception[0],perf_monitor);
					`ifdef verbose $display($time,"\tICACHE: TLB Exception "); `endif
				end
				rg_state[0]<=Idle;
				`ifdef MMU rg_trnslte_done[0] <= False; `endif
				rg_tlb_exception[0]<=tagged None;
			end
			/*===================================================================*/
			end
			else begin
				`ifdef verbose $display($time,"\tICACHE: Translated Address not Available"); `endif
				rg_state[0] <= KeepPolling;
			end
		endrule
		/*======= filling up the cache from the data recieved from the external memory ======= */
		rule read_IO_response(rg_state[0]==IOReadResp && !memoperation.notEmpty);
			let memresp=ff_response_from_memory.first;
			ff_response_from_memory.deq;
			`ifdef verbose $display($time,"\tICACHE: Got response from IO ADDRESS: %h",memresp.data_line); `endif
			wr_response_to_cpu<=tagged Valid (tuple3(truncate(memresp.data_line),memresp.bus_error==1?tagged Exception Inst_access_fault:tagged None,1));
			rg_state[0]<=Idle;
		endrule
		rule read_from_lbdata_into_hold_reg(line_bytes_written=='1 && memoperation.notEmpty);
			let lb_hold_reg=lbdata.response_portB;
			let {paddress,vaddress,replaceblock,writeenable}=memoperation.first;
			Bit#(`ICACHE_TAG_BITS) cpu_tag=paddress[`PADDR-1:`PADDR-`ICACHE_TAG_BITS];
			Bit#(TLog#(`ICACHE_SETS)) setindex=vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits];
			Bit#(4) lbreplaceblock=0;
			case (replaceblock)
				'd0:lbreplaceblock='b0001;
				'd1:lbreplaceblock='b0010;
				'd2:lbreplaceblock='b0100;
				'd3:lbreplaceblock='b1000;
			endcase
			for(Integer i=0;i<`ICACHE_WAYS;i=i+1)begin	
				tag[i].write_request((unpack(lbreplaceblock[i])&&True),setindex,{2'b1,cpu_tag});
				data[i].write_request(duplicate(lbreplaceblock[i]),setindex,lb_hold_reg);
			end
			line_bytes_written<=0;
			memoperation.deq;
			`ifdef verbose $display($time,"\tICACHE: capturing lbdata cpu_tag: %h setindex: %d linenum: %b data: %h",cpu_tag, setindex,lbreplaceblock,lb_hold_reg); `endif
			if(rg_state[1]==KeepPolling)
				rg_state[1]<=Stall;
		endrule
		
		rule fillcache(memoperation.notEmpty && line_bytes_written!='1);
			let memresp=ff_response_from_memory.first;
			ff_response_from_memory.deq;
			let {paddress,vaddress,replaceblock,writeenable}=memoperation.first;
			let cpu_tag=paddress[`PADDR-1:`PADDR-`ICACHE_TAG_BITS];
			Bit#(TLog#(`ICACHE_SETS)) setindex=vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits];
			`ifdef verbose $display($time,"\tICACHE: Response from Memory: %h writeenable: %h",memresp.data_line, writeenable); `endif 
			let we=writeenable;
			if(|line_bytes_written!=0)begin
				we=rg_we;
			end
			Bit#(TMul#(2,TMul#(`ICACHE_WORD_SIZE,`ICACHE_BLOCK_SIZE))) extended_mask=zeroExtend(we)<<8;
			lbdata.write_portB(we,duplicate(memresp.data_line));
			`ifdef verbose $display($time,"\tICACHE: linebytes: %h currently writing into: %h",line_bytes_written,we); `endif
			if(memresp.last_word)begin // if all the data words have been fetched exit	
				`ifdef verbose $display($time,"\tICACHE: Received Last response from Memory"); `endif
			end
			`ifdef prefetch 
				prefetchmode<=False; 
			`endif
			rg_we<=(extended_mask[2*`ICACHE_BLOCK_SIZE*`ICACHE_WORD_SIZE-1:`ICACHE_BLOCK_SIZE*`ICACHE_WORD_SIZE]|extended_mask[`ICACHE_BLOCK_SIZE*`ICACHE_WORD_SIZE-1:0]);
			line_bytes_written<=line_bytes_written|we;
		endrule
		/*===================================================================================== */
		rule stall_the_next_request_by_one_cycle(rg_state[0]==Stall);
			Bit#(TLog#(`ICACHE_SETS)) setindex=rg_vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits];
			for(Integer i=0;i<`ICACHE_WAYS;i=i+1)begin // send address to the Block_rams
				tag[i].read_request(setindex);
				data[i].read_request(setindex);
			end
			rg_state[0]<=ReadingCache;
		endrule
		/*===================================================================================== */
		rule keep_polling_on_stall(rg_state[1]==KeepPolling);
			Bit#(TLog#(`ICACHE_BLOCK_SIZE)) byteoffset=rg_vaddress[word_bits+byte_bits-1:byte_bits];
			Bit#(TMul#(`ICACHE_WORD_SIZE,`ICACHE_BLOCK_SIZE)) requested_word=('hF<<({2'd0,byteoffset}*4));
			Bit#(`PERFMONITORS) perf_monitor=0;
			if(capture_counters)begin
				perf_monitor[`ICACHE_CACHEABLE]=1;
				perf_monitor[`ICACHE_MISS]=1;
				rg_perf_monitor<=perf_monitor;
			end
			let {lb_paddress,lb_vaddress,replaceblock,writeenable}=memoperation.first;
			Bit#(TLog#(`ICACHE_SETS)) setindex=rg_vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits];
			Bit#(20) cpu_tag=rg_paddress[`PADDR-1:`PADDR-20];
			Bit#(`ICACHE_TAG_BITS) lbtag=lb_paddress[`PADDR-1:`PADDR-`ICACHE_TAG_BITS];
			Bit#(TLog#(`ICACHE_SETS)) lbset=lb_vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits];
			Bool generate_request=True;
			`ifdef verbose $display($time,"\tICACHE: line_bytes_written: %h requested_word: %h memoperation: %b ",line_bytes_written,requested_word,memoperation.notEmpty); `endif
			if(lbset==setindex && lbtag==cpu_tag && memoperation.notEmpty)
				if((line_bytes_written & requested_word) != requested_word)
					generate_request=False;
			if(rg_trnslte_done[1] && generate_request)begin
				if(rg_tlb_exception[1] matches tagged None)begin
					begin
						`ifdef verbose $display($time,"\tICACHE: Accessing LB"); `endif
						rg_state[1]<=ReadingCache; 
						for(Integer i=0;i<`ICACHE_WAYS;i=i+1)begin // send address to the Block_rams
							tag[i].read_request(setindex);
							data[i].read_request(setindex);
						end
					end
				end
				else begin
					rg_state[1]<=ReadingCache; 
				end
			end
		endrule

		/*============= Prediction in burst mode ================================ */
		method Action virtual_address(Bit#(`VADDR) vaddress,Bool fence)if(rg_state[1]==Idle);
			if(fence)begin
				rg_state[1]<=Fence;
			end
			else begin
				Bit#(TLog#(`ICACHE_SETS)) setindex=vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits];
				`ifdef verbose $display($time,"\tICACHE: Request of VAddr: %h set: %d",vaddress, setindex); `endif
				rg_vaddress<=vaddress;
				for(Integer i=0;i<`ICACHE_WAYS;i=i+1)begin // send address to the Block_rams
					tag[i].read_request(truncate(setindex));
					data[i].read_request(vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits]);
				end
				rg_state[1]<=ReadingCache;
			end
		endmethod
		method Maybe#(Tuple3#(Bit#(32), Trap_type, Bit#(`PERFMONITORS))) response_to_core;
			return wr_response_to_cpu;
		endmethod
		`ifdef MMU
			method Action physical_address(Bit#(`PADDR) paddr, Trap_type ex);
				`ifdef verbose $display($time,"\tICACHE: Sending physical address %h to icache ",paddr); `endif
				rg_paddress<=paddr;
				rg_tlb_exception[1]<=ex;
				//rg_state[1]<=ReadingCache;
				rg_trnslte_done[1] <= True;
			endmethod
		`endif
		method ActionValue#(To_Memory#(`PADDR)) request_to_memory;
			ff_request_to_memory.deq;
			return ff_request_to_memory.first;
		endmethod
		method Action response_from_memory(From_Memory#(`DCACHE_WORD_SIZE) resp);
			if(!ignore_memory_response)
				ff_response_from_memory.enq(resp);
			else if(resp.last_word)
				ignore_memory_response<=False;
		endmethod
			method Bit#(`PERFMONITORS) icache_perfmon;
				return rg_perf_monitor;
			endmethod
		method Action stall_fetch(Bool stall);
			rg_stall_fetch <= stall;
		endmethod
		`ifdef prefetch
			method ActionValue#(Bit#(`VADDR)) prefetch() if(rg_state[1]==Idle &&& rg_prefetchpc matches tagged Valid .vaddress &&& !memoperation.notEmpty);
				Bit#(TLog#(`ICACHE_SETS)) setindex=vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits];
				`ifdef verbose $display($time,"\tICACHE: Prefetch Request of VAddr: %h set: %d",vaddress, setindex); `endif
				for(Integer i=0;i<`ICACHE_WAYS;i=i+1)begin // send address to the Block_rams
					tag[i].read_request(truncate(setindex));
					data[i].read_request(vaddress[set_bits+word_bits+byte_bits-1:word_bits+byte_bits]);
				end
				rg_state[1]<=ReadingCache;
				rg_vaddress<=vaddress;
				prefetchmode<=True;
				rg_prefetchpc<=tagged Invalid;
				return vaddress;
			endmethod
		`endif
	endmodule
endpackage
