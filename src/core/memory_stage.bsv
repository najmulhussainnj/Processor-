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
package memory_stage;
	/*===== Package Imports ==== */
	import TxRx::*;
	import FIFOF::*;
	import DReg::*;
	import Vector::*;
	import GetPut::*;
	/*========================== */
	/*===== Project Imports ======*/
	`include "defined_parameters.bsv"
	import defined_types::*;
	//import dcache::*;
	/*============================ */

	interface Ifc_memory_stage;
		/* ====================== pipe connections ========= */
		interface RXe#(IE_IMEM_type) rx_in;
		interface TXe#(IMEM_IWB_type) tx_out;
		/*================================================== */
		method Action update_wEpoch;
		interface Put#(Maybe#(Tuple4#(Bit#(`Reg_width), Trap_type, Bit#(`PERFMONITORS),Bit#(1)))) response_from_dmem;
		method Action loadtrigger_info(TriggerData tdata);
		method Action storetrigger_info(TriggerData tdata);
		method Maybe#(Tuple3#(Bit#(`Reg_width), Bit#(TLog#(`PRFDEPTH)), Bit#(4))) forwarding_data;	
	endinterface

	(*synthesize*)
	module mkmemory_stage(Ifc_memory_stage);
		RX#(IE_IMEM_type) rx <-mkRX;							// receive from the execution stage
		TX#(IMEM_IWB_type) tx <-mkTX;							// send to the writeback stage;
		Wire#(Memout) wr_info_to_dmem <-mkWire;// holds the information to be given to dmem
		Wire#(TriggerData) wr_loadtrigger<-mkDWire(TriggerData{ttype:tagged None,matchscheme:0});
		Wire#(TriggerData) wr_storetrigger<-mkDWire(TriggerData{ttype:tagged None,matchscheme:0});
		Wire#(Maybe#(Tuple4#(Bit#(`Reg_width), Trap_type,Bit#(`PERFMONITORS),Bit#(1)))) wr_response_to_cpu <- mkDWire(tagged Invalid);
		//Wire#(Maybe#(Operand_forwading_type)) wr_forward_from_MEM <-mkDWire(tagged Invalid);// holds the forwarded data from the memory stage
		//Reg#(Maybe#(Tuple3#(Bit#(`Reg_width), Bit#(TLog#(`PRFDEPTH)), Bit#(4)))) wr_forward_from_MEM <-mkDReg(tagged Invalid);// holds the forwarded data from the memory stage
		Wire#(Maybe#(Tuple3#(Bit#(`Reg_width), Bit#(TLog#(`PRFDEPTH)), Bit#(4)))) wr_forward_from_MEM <-mkDWire(tagged Invalid);// holds the forwarded data from the memory stage
		//Ifc_dcache dcache<-mkdcache;
		Reg#(Bit#(1)) wEpoch <-mkReg(0);
		
		function Bool checktrigger(TriggerData tdata, Bit#(`Reg_width) address, Bit#(`Reg_width) lsdata);
			if(tdata.ttype matches tagged Address .addr)
				if(tdata.matchscheme==0 && addr==truncate(address))
					return True;
				else if(tdata.matchscheme==2 && addr>=truncate(address))
					return True;
				else if(tdata.matchscheme==3 && addr<=truncate(address))
					return True;
				else if(tdata.matchscheme==4 && addr[31:0]==(addr[31:0]&address[31:0]))
					return True;
				else if(tdata.matchscheme==5 && addr[31:0]==(addr[`VADDR-1:32]&address[`VADDR-1:32]))
					return True;
				else
					return False;
			else if(tdata.ttype matches tagged Data .data)
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

		//(*conflict_free="receive_info_from_execution_stage,response_to_core"*)
		rule receive_info_from_execution_stage(rx.u.notEmpty && tx.u.notFull);
			Bit#(`PERFMONITORS) pm=rx.u.first.perfmonitors;
			let info=rx.u.first();
			let pc=rx.u.first.program_counter;
			let dest=rx.u.first.destination;
			let rdtype=rx.u.first.rd_type;
			`ifdef simulate let instr=rx.u.first.instruction; `endif
			let sysinstr=False;
			Bit#(`ADDR) baddr = 0;	
			WriteBackType result1=?;
      	`ifdef verbose $display($time,"\t*****************MEMORY STAGE*************************\t PC: %h PRFINDEX: %d PID: %d EPOCHS: %b wEpoch: %b",pc,rx.u.first.index,rx.u.first.pid, rx.u.first.epochs, wEpoch); `endif

			/* If the instruction is a memory operation (Load/Store/Atomic/Fence) then generate
			the request to the cache in this rule and expect the response in the consecutive rule*/
			if(info.execresult matches tagged MEMORY .meminfo)begin
				Bool lstrigger=False;
				`ifdef verbose $display($time,"\tMEMORY: load_trigger: ", fshow(wr_loadtrigger)); `endif
				let exception=rx.u.first.exception;
				if(wr_response_to_cpu matches tagged Valid .d)begin
					let {data,e,perfmonitors,epochs}=d;
					exception=exception matches tagged None?e:exception;
					Bit#(`Reg_width) fwd_data=0;
					if(exception matches tagged Exception .exc)begin
						result1 = tagged RESULT Arithout{aluresult:meminfo.address,fflags:0};
					end
					`ifdef spfpu
					else if(meminfo.transfer_size==2 && rx.u.first.rd_type==FloatingRF)begin
						result1 = tagged RESULT Arithout{aluresult:{'1,data[31:0]},fflags:0};
						fwd_data={'1,data[31:0]};
					end
					`endif
					else begin
						result1 = tagged RESULT Arithout{aluresult:data,fflags:0};
						fwd_data=data;
					end
					pm=pm|perfmonitors;
					rx.u.deq;
					if(epochs==wEpoch)begin
						`ifdef verbose $display($time,"\tMEMORY: Response from DCACHE: Data: %h Address: %h transfersize: %d epochs: %b wEpochs: %b",data,meminfo.address,meminfo.transfer_size,epochs,wEpoch); `endif
						tx.u.enq(IMEM_IWB_type{commit_data:result1, index:rx.u.first.index, pid:rx.u.first.pid, debugcause:rx.u.first.debugcause,
										program_counter:pc,	destination:dest, epochs:{rx.u.first.epochs[1],epochs},
										rd_type:rdtype,		exception:exception, perfmonitors:rx.u.first.perfmonitors
										`ifdef simulate , instruction:instr `endif });
						if(meminfo.mem_type!=Store &&& ((rdtype==IntegerRF && dest!=0) `ifdef spfpu || rdtype==FloatingRF `endif ) &&& exception matches tagged None)
							wr_forward_from_MEM <= tagged Valid tuple3(fwd_data,	rx.u.first.index, rx.u.first.pid);
					end
					else begin
						`ifdef verbose $display($time,"\tMEMORY: Dropping the received response from DCACHE"); `endif
					end
				end
			end
			/* If the instruction is not a memory operation then bypass this stage
			and enque into the write-back stage. If this is a CSR operation changing
			the FCSR register then generate the necessary flag. Also generate the forwarding 
			signal to the decode stage*/
			else begin 
				rx.u.deq;
				if(rx.u.first.epochs[0]!=wEpoch)begin
					`ifdef verbose $display($time,"\tMEMORY: PC: %h Dropping instructions",rx.u.first.program_counter); `endif
				end
				else begin
					`ifdef verbose $display($time,"\tMEMORY: Bypassing Memory Stage"); `endif
					if(info.execresult matches tagged SYSTEM .res1)begin
						result1=tagged SYSTEM res1 ;
					end
					else if(info.execresult matches tagged RESULT .res1)begin
						result1=tagged RESULT res1;
					end
					tx.u.enq(IMEM_IWB_type{commit_data:result1,  index:rx.u.first.index, pid:rx.u.first.pid, debugcause:rx.u.first.debugcause,
										program_counter:pc,	destination:dest, epochs:rx.u.first.epochs,
										rd_type:rdtype,		exception:info.exception, perfmonitors:rx.u.first.perfmonitors
										`ifdef simulate , instruction:instr `endif });
				end
			end
		endrule
		method tx_out=tx.e;
		method rx_in=rx.e;
		interface response_from_dmem = interface Put
			method Action put(Maybe#(Tuple4#(Bit#(`Reg_width), Trap_type, Bit#(`PERFMONITORS),Bit#(1))) resp); 
				wr_response_to_cpu <= resp;
			endmethod
		endinterface;
		method Action loadtrigger_info(TriggerData tdata);
			wr_loadtrigger<=tdata;
		endmethod
		method Action storetrigger_info(TriggerData tdata);
			wr_storetrigger<=tdata;
		endmethod
		method Maybe#(Tuple3#(Bit#(`Reg_width), Bit#(TLog#(`PRFDEPTH)), Bit#(4))) forwarding_data=wr_forward_from_MEM;	
		method Action update_wEpoch;
			wEpoch<=~wEpoch;
		endmethod
	endmodule
endpackage:memory_stage
