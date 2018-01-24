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
package branchpredictor;
	/*===== Pacakge imports ===== */
	import BRAMCore::*;
	import FIFO::*;
	import FIFOF::*;
	import SpecialFIFOs::*;
	import LFSR::*;
	import ConfigReg::*;
	import DReg::*;
		import Connectable::*;
		import GetPut::*;
	/*===== project imports==== */
	import defined_types::*;
	`include "defined_parameters.bsv"
	/*========================= */

	interface Ifc_branchpredictor;
		interface Put#(Tuple2#(Bit#(3),Bit#(`VADDR))) send_prediction_request;
		interface Get#(Tuple4#(Bit#(3),Bit#(`VADDR),Bit#(`VADDR),Bit#(2))) prediction_response;
		method Action training (Maybe#(Training_data#(`VADDR)) training_data);
	endinterface

	(*synthesize*)
	module mkbranchpredictor(Ifc_branchpredictor);
		let btb_sizebits=valueOf(TLog#(`BTB_DEPTH));
		let tag_sizebits=(`VADDR-(btb_sizebits+2));
		let max_size=tag_sizebits+3;
		BRAM_DUAL_PORT#(Bit#(TLog#(`BTB_DEPTH)),Bit#(`VADDR)) rg_target_addr <- mkBRAMCore2(valueOf(`BTB_DEPTH),False);
		BRAM_DUAL_PORT#(Bit#(TLog#(`BTB_DEPTH)),Bit#(TAdd#(3,TSub#(TSub#(`VADDR, TLog#(`BTB_DEPTH)),2)))) rg_tag <- mkBRAMCore2(valueOf(`BTB_DEPTH),False);
		Reg#(Bit#(TSub#(TSub#(`VADDR,TLog#(`BTB_DEPTH)),2))) training_tag <-mkReg(0);
		Reg#(Bit#(TLog#(`BTB_DEPTH))) training_index <-mkReg(0);
		Reg#(Bool) rg_initialize <-mkReg(True);
		Reg#(Bit#(TAdd#(1,TLog#(`BTB_DEPTH)))) rg_index<-mkReg(0);
		FIFOF#(Tuple2#(Bit#(3),Bit#(`VADDR))) capture_prediction_request <-mkLFIFOF();
		rule initialize_brams(rg_initialize);
			rg_tag.b.put(True,truncate(rg_index),{3'b001,'d0});	
			if(rg_index==(`BTB_DEPTH-1))begin
				rg_initialize<=False;
				rg_index<=0;
			end
			else
				rg_index<=rg_index+1;
		endrule
		interface send_prediction_request = interface Put 
			method Action put(Tuple2#(Bit#(3),Bit#(`VADDR)) req)if(!rg_initialize);
				let {epoch,vaddress} = req;
				`ifdef verbose $display($time,"\tBPU: Prediction Request for Address: %h",vaddress); `endif
				rg_target_addr.a.put(False,vaddress[btb_sizebits+1:2],?);
				rg_tag.a.put(False,vaddress[btb_sizebits+1:2],?);
				capture_prediction_request.enq(req);
			endmethod
		endinterface;
		interface prediction_response = interface Get
			method ActionValue#(Tuple4#(Bit#(3),Bit#(`VADDR),Bit#(`VADDR),Bit#(2))) get if(!rg_initialize);
				let {epoch,vaddress} = capture_prediction_request.first;
				Bit#(`VADDR) target_address=rg_target_addr.a.read;
				let info=rg_tag.a.read;
				Bit#(TSub#(TSub#(`VADDR,btb_sizebits),2)) tag=info[tag_sizebits-1:0];
				Bit#(TSub#(TSub#(`VADDR, TLog#(`BTB_DEPTH)),2)) cpu_tag=vaddress[`VADDR-1:btb_sizebits+2];
				Bit#(1) valid=info[tag_sizebits+2];
				Bit#(1) tag_match=pack(tag==cpu_tag)&valid;
				Bit#(2) state=(tag_match==1)?info[tag_sizebits+1:tag_sizebits]:'b01;
				let x= tuple4(epoch,vaddress,target_address,state);
				capture_prediction_request.deq;
				return x;
			endmethod
		endinterface;
		method Action training (Maybe#(Training_data#(`VADDR)) training_data)if(!rg_initialize); //to train the bpu;
			if(training_data matches tagged Valid .td)begin
				let addr=td.branch_address;
				Bit#(TLog#(`BTB_DEPTH)) index=td.pc[btb_sizebits+1:2];
				Bit#(TSub#(TSub#(`VADDR, TLog#(`BTB_DEPTH)),2)) tag=td.pc[`VADDR-1:btb_sizebits+2];
				`ifdef verbose $display($time,"\tBPU: training for PC: %h JumpAddr: %h index: %d State:",td.pc,addr,index,fshow(td.state)); `endif
				rg_target_addr.b.put(True,td.pc[btb_sizebits+1:2],addr);
				rg_tag.b.put(True,td.pc[btb_sizebits+1:2],{1,td.state,tag});
			end
		endmethod
	endmodule
endpackage
