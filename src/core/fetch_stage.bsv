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
package fetch_stage;
	/*========= package imports========== */
	import FIFOF::*;
	import SpecialFIFOs::*;
	import Connectable::*;
	import GetPut::*;
	import DReg::*;
	/*==================================== */
	/* ======== project imports ========= */
	import TxRx	::*;
	import defined_types::*;
	`include "defined_parameters.bsv"
	import Stack::*;
	/* ================================== */

	interface Ifc_fetch;
		/*============================ Miscellaneous interface =========================== */
		method Action flush (Bit#(`VADDR) new_pc, Flush_type fl);
		method Action stall_fetch(Bool stall);
		interface Get#(Tuple5#(Bit#(2),Bit#(`VADDR),Bit#(`VADDR),Bool, Bit#(3))) request_to_imem;
		method Action instruction_response_from_imem(Maybe#(Tuple7#(Bit#(`VADDR),Bit#(2),Bit#(`VADDR),Bit#(32), Trap_type, Bit#(`PERFMONITORS),Bit#(3))) x);
		interface Put#(Tuple4#(Bit#(3),Bit#(`VADDR),Bit#(`VADDR),Bit#(2))) prediction_response;
		interface Get#(Tuple2#(Bit#(3),Bit#(`VADDR))) send_prediction_request;
		interface TXe#(IF_ID_type) tx_out; // pipe interface to the external FIFO;
		method Action push_ras(Maybe#(Bit#(`VADDR)) addr);
		method Action update_eEpoch;
		method Action update_wEpoch;
		/*============================================================================== */
	endinterface:Ifc_fetch

	(*synthesize*)
	(*preempts="flush,enque_new_pc"*)
	(*conflict_free="enque_new_pc,prediction_response_put"*)
	module mkfetch#(Bit#(`VADDR) reset_vector)(Ifc_fetch);
		FIFOF#(Tuple3#(Bit#(2),Bit#(`VADDR), Bit#(`VADDR))) generate_pc<-mkLFIFOF();
		Reg#(Bit#(`VADDR)) rg_programcounter[3]<-mkCReg(3,'h1000);
		Reg#(Bit#(1)) eEpoch <-mkReg(0);
		Reg#(Bit#(1)) wEpoch <-mkReg(0);
		Reg#(Bit#(1)) iEpoch[2] <-mkCReg(2,0);
		Wire#(Maybe#(Bit#(`VADDR))) wr_flush_prediction <-mkDWire(tagged Invalid);

		Wire#(Bool) wr_stall_fetch <- mkDWire(False);
		Reg#(Bool) rg_fence[2]<-mkCReg(2,False);
		FIFOF#(Tuple7#(Bit#(`VADDR),Bit#(2),Bit#(`VADDR),Bit#(32),Trap_type, Bit#(`PERFMONITORS),Bit#(3))) ff_response_to_cpu <-mkSizedBypassFIFOF(1);
		TX#(IF_ID_type) tx <-mkTX;
		Ifc_Stack ras <-mkStack;

		rule get_instruction_from_cache;
			let {pc,prediction,npc,instruction,trap,perfmonitors,epoch}=ff_response_to_cpu.first;
			ff_response_to_cpu.deq;
			Bool rs1_link=case (instruction[19:15]) matches 'b00?01:True; default :False; endcase;
			Bool rd_link=case (instruction[11:7]) matches 'b00?01:True; default :False; endcase;
			`ifdef verbose $display($time,"\t************* FETCH STAGE FIRING ************ PC: %h Instr-EPOCHS: %b Current_Epochs: %b",pc, epoch,{iEpoch[0],eEpoch,wEpoch}); `endif
			if(epoch!={iEpoch[0],eEpoch,wEpoch})begin
				`ifdef verbose $display($time,"\tFETCH: Dropping Instruction Since Epochs do not match"); `endif
			end
			else begin
				if(instruction[6:2] matches 'b110?1 &&& !rd_link &&& rs1_link)begin
					if(!ras.empty)begin
						let x<-ras.top;
						if(x!=npc || prediction[1]==0)begin
							npc=x;
							iEpoch[0]<=~iEpoch[0];
							wr_flush_prediction<=tagged Valid npc;
							prediction='b10;
						end
						`ifdef verbose $display($time,"TAKING RAS as the NEXT PC: %h",x); `endif
					end
				end
				else if((prediction[1]==1 && instruction[6:4]!='b110))begin
					iEpoch[0]<=~iEpoch[0];
					wr_flush_prediction<=tagged Valid (pc+4);
					prediction[1]=0;
				end
				tx.u.enq(IF_ID_type{program_counter:pc,
										  instruction:instruction[31:0],
										  nextpc:npc,
										  prediction:prediction,
										  perfmonitors:perfmonitors,
										  exception:trap,
										  epochs:{eEpoch,wEpoch}
										  });
				`ifdef verbose $display($time,"\tInstruction Fetched: %h \t PC: %h PERF: %h Prediction: ",instruction,pc,perfmonitors,fshow(prediction)," next pc: %h",npc); `endif
			end
		endrule

		rule enque_new_pc(wr_flush_prediction matches tagged Valid .newpc);
			`ifdef verbose $display($time,"\tFETCH: Enquiing new PC to ICACHE: %h",newpc); `endif
			generate_pc.enq(tuple3('b00,newpc,newpc+4));			
		endrule

//		/* ================================== Methods and interface definitions =======================*/
		interface request_to_imem = interface Get
			method ActionValue#(Tuple5#(Bit#(2),Bit#(`VADDR),Bit#(`VADDR),Bool, Bit#(3))) get if(tx.u.notFull && !wr_stall_fetch);
				let {prediction,pc,npc}=generate_pc.first;
				`ifdef verbose $display($time,"\tFETCH: Address sent to IMEM: %h epochs: %b",pc,{iEpoch[0],eEpoch,wEpoch}); `endif
					rg_fence[0]<=False;
				if(!rg_fence[0])
					generate_pc.deq;
				return tuple5(prediction,npc,pc,rg_fence[0],{iEpoch[0],eEpoch,wEpoch});
			endmethod
		endinterface;
		method Action instruction_response_from_imem(Maybe#(Tuple7#(Bit#(`VADDR),Bit#(2),Bit#(`VADDR),Bit#(32), Trap_type, Bit#(`PERFMONITORS),Bit#(3))) x);
			if(x matches tagged Valid .instr1)begin
				let {pc,prediction,npc,instruction,trap,perfmonitors,epoch}=instr1;
				`ifdef verbose $display($time,"\tFETCH: GOT Instructions: ",fshow(instr1)); `endif
				ff_response_to_cpu.enq(instr1);
			end
		endmethod
		method Action flush (Bit#(`VADDR) new_pc, Flush_type fl);
			`ifdef verbose $display($time,"\tFETCH: Flushing New PC: %h",new_pc); `endif
			rg_programcounter[1]<=new_pc;
			generate_pc.clear;
			if(fl==Fence)
				rg_fence[1]<=True;
		endmethod
		method Action stall_fetch(Bool stall);
			wr_stall_fetch <= stall;
		endmethod
		interface tx_out = tx.e;
		method Action push_ras(Maybe#(Bit#(`VADDR)) addr);
			if(addr matches tagged Valid .x)begin
				`ifdef verbose $display($time,"RAS: Pushing Addr: %h",x); `endif
				ras.push(x);
			end
		endmethod
		interface prediction_response=interface Put
			method Action put (Tuple4#(Bit#(3),Bit#(`VADDR),Bit#(`VADDR),Bit#(2)) x);
				let {epoch,pc,npc,prediction}=x;
				if(epoch=={iEpoch[1],eEpoch,wEpoch} &&& wr_flush_prediction matches tagged Invalid)begin
					if(prediction[1]==0)
						npc=pc+4;
						rg_programcounter[0]<=npc;
						`ifdef verbose $display($time,"\tFETCH: Got prediction from BPU: %b for PC: %h New PC: %h",prediction,pc,npc); `endif
						generate_pc.enq(tuple3(prediction,pc,npc));		
				end
				`ifdef verbose
				else 
					 $display($time,"\tFETCH: Dropping response from BPU for PC: %h",pc); `endif
			endmethod
		endinterface;
		interface send_prediction_request=interface Get
			method ActionValue#(Tuple2#(Bit#(3),Bit#(`VADDR))) get;
				if(wr_flush_prediction matches tagged Valid .newpc)begin
					`ifdef verbose $display($time,"\tFETCH: Sending Program Counter to BPU: %h",newpc+4); `endif
					return tuple2({iEpoch[1],eEpoch,wEpoch},newpc+4);
				end
				else begin
					`ifdef verbose $display($time,"\tFETCH: Sending Program Counter to BPU: %h",rg_programcounter[2]); `endif
					return tuple2({iEpoch[1],eEpoch,wEpoch},rg_programcounter[2]);
				end
			endmethod
		endinterface;
		method Action update_eEpoch;
			eEpoch<=~eEpoch;
		endmethod
		method Action update_wEpoch;
			wEpoch<=~wEpoch;
		endmethod
		/*================================================================================================= */
	endmodule:mkfetch
endpackage:fetch_stage
