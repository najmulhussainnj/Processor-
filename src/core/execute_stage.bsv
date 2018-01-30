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
package execute_stage;
	/*===== Package Imports ==== */
	import TxRx::*;
	import FIFOF::*;
	import DReg::*;
	import Clocks::*;
	import GetPut::*;
	import SpecialFIFOs::*;
	/*========================== */
	/*===== Project Imports ======*/
	import alu::*;
	`include "defined_parameters.bsv"
	import defined_types::*;
	`ifdef muldiv
	`ifdef RV64
		import muldiv::*;
	`endif
	`endif
	`ifdef spfpu
		import fpu::*;
	`endif
	import prf::*;
	/*===============================*/

	(*noinline*)
	function Bit#(`Reg_width) multiplication (Bit#(`Reg_width) in1,Bit#(`Reg_width) in2,Bit#(2) funct3 `ifdef RV64 ,Bool word_double `endif );
		Bit#(TMul#(2,`Reg_width)) op1= ((funct3[0]^funct3[1])==1 && in1[`Reg_width-1]==1)?zeroExtend((~in1)+1):zeroExtend(in1);
		Bit#(TMul#(2,`Reg_width)) op2= (funct3[1:0]==1 && in2[`Reg_width-1]==1)?zeroExtend((~in2)+1):zeroExtend(in2);
		Bit#(1) lv_take_complement = 0;
		if(funct3[1:0]==1)
			lv_take_complement=((in1[`Reg_width-1]^in2[`Reg_width-1])==1)?1:0;
		else if(funct3[1:0]==2)
			lv_take_complement=in1[`Reg_width-1];
			
		let product=op1*op2;
		product=(lv_take_complement==1)?(~product+1): product;
		`ifdef RV64
			if(!word_double)// 32-bit
				product=signExtend(product[31:0]);
		`endif
		if(funct3==0)
				return product[`Reg_width-1:0];
		else
				return product[2*`Reg_width-1:`Reg_width];
	endfunction

	interface Ifc_execute_stage;
		method Action flush_prf;
		/* ====================== pipe connections ========= */
		interface RXe#(ID_IE_type) rx_in;
		interface TXe#(IE_IMEM_type) tx_out;
		/*================================================== */
		method Action roundingmode(Bit#(3) rm);
		method Tuple2#(Flush_type,Bit#(`VADDR)) generate_flush;
		method Maybe#(Training_data#(`VADDR)) training_data;
		method Bit#(`PERFMONITORS) execute_perfmon; // icache performance counters
		method Maybe#(Bit#(`VADDR)) ras_push;
		interface Get#(Tuple2#(Memout,Bit#(1))) to_dmem;
		method Action _forwarding_from_memory (Maybe#(Tuple3#(Bit#(`Reg_width), Bit#(TLog#(`PRFDEPTH)), Bit#(4))) fwd_data);
		method Action update_wEpoch;
	endinterface:Ifc_execute_stage

	`ifdef muldiv
		`ifdef spfpu
			`ifdef sequential
				(*mutually_exclusive="read_output_from_fpu, read_outputs_from_muldiv"*)
			`endif
		`endif
	`endif
	(*synthesize*)
	(*conflict_free="rl_receive_info_from_decode_stage, to_dmem_get"*)
	module mkexecute_stage(Ifc_execute_stage);
		Wire#(Maybe#(Bit#(`VADDR))) wr_ras_push<-mkDWire(tagged Invalid);
		Ifc_prf_new prf <-mkprf_new();
		RX#(ID_IE_type) rx <-mkRX;								// receive ffrom the decode stage
		TX#(IE_IMEM_type) tx <-mkTX;							// send to the memory stage;
	   `ifdef muldiv 
			`ifdef sequential
				Ifc_muldiv muldiv <-mkmuldiv;
			`endif
			`ifdef parallel
				Reg#(Maybe#(Bit#(`Reg_width))) rg_mul_output<-mkReg(tagged Invalid);
			`endif
		`endif // instantiating
      `ifdef spfpu Ifc_fpu fpu <- mkfpu();	`endif // instantiating the Floating point units. 

		//Wire#(Memout) wr_info_to_dmem <-mkWire;// holds the information to be given to dmem
		FIFOF#(Tuple2#(Memout,Bit#(1))) wr_info_to_dmem <-mkBypassFIFOF;// holds the information to be given to dmem
		Wire#(Maybe#(Tuple3#(Bit#(`Reg_width), Bit#(TLog#(`PRFDEPTH)), Bit#(4)))) wr_forward_from_EXE <-mkDWire(tagged Invalid);// holds the forwarded data from the memory stage
		Wire#(Bit#(3)) wr_rounding_mode<-mkDWire(0);
		Reg#(Bool) multicylce_output[2] <-mkCReg(2,False);		// is true if the operation is multicycle.
		Wire#(Flush_type) rg_flush_execute <-mkDWire(None);
		Wire#(Bit#(`VADDR)) rg_effective_address<-mkDWire(0);
		Reg#(Maybe#(Training_data#(`VADDR))) wr_training_data <-mkDReg(tagged Invalid);
		`ifdef perf
			Reg#(Bit#(`PERFMONITORS)) rg_execute_perfmon<-mkDReg(0);
		`endif
		Reg#(Bit#(1)) eEpoch <-mkReg(0);
		Reg#(Bit#(1)) wEpoch <-mkReg(0);
		PulseWire wb_flush <-mkPulseWire();

		Reg#(Bit#(4)) rg_pid <-mkReg(0);
		Reg#(Bit#(TLog#(`PRFDEPTH))) rg_prf_index<-mkReg(0);
		

		rule update_eEpoch_reg(rg_flush_execute!=None && !wb_flush);
			`ifdef verbose $display($time,"\tEXECUTION: Inverting eEPOCH"); `endif
			eEpoch<=~eEpoch;
		endrule

		rule forward_data_from_exe(wr_forward_from_EXE matches tagged Valid .fwdata) ;
			let {data,index,pid}=fwdata;
			prf.fwd_from_execution(data,index,pid);
		endrule
	
		rule rl_receive_info_from_decode_stage(rx.u.notEmpty && tx.u.notFull && !multicylce_output[1] );
			Bit#(`PERFMONITORS) perfmonitors=0;
			let data=rx.u.first;
			let pc=data.program_counter;
			let dest=data.destination;
			let rdtype=data.rdtype;
			let exception=data.exception;
			let inst_type=data.inst_type;
			Bit#(`VADDR) nextpc=rx.u.first.nextpc;
			`ifdef simulate let instruction=data.instruction; `endif
			Execution_output result1=tagged Busy;
			`ifdef verbose $display($time,"\t********** EXECUTION STAGE FIRING ************ PC: :%h EPOCHS: %b Instr-EPOCHS: %b",pc,{eEpoch,wEpoch}, data.epochs)  ; `endif
			if({eEpoch,wEpoch}!=data.epochs)begin
				rx.u.deq;
				`ifdef verbose $display($time,"\tEXECUTION: PC: %h Dropping Instruction since Epochs do not match", rx.u.first.program_counter); `endif
			end
			else if(exception matches tagged None)begin
				Bool start_execution=False;
				`ifdef verbose $display($time,"\tEXECUTION: rs1type: ",fshow(data.rs1_type)," rs2_type ",fshow(data.rs2_type)); `endif
				RFType#(`Reg_width) op1<-prf.read_rs1(data.rs1addr,data.rs1_type,data.rs1);
				RFType#(`Reg_width) op2<-prf.read_rs2(data.rs2addr,data.rs2_type,data.rs2);
				RFType#(`Reg_width) op3<-prf.read_rs3(data.rs3addr,data.rs3_type,data.rs3_imm);
				if(op1 matches tagged Present .rs1 &&& op2 matches tagged Present .rs2 &&& op3 matches tagged Present .rs3)begin
					`ifdef muldiv
					let {prf_index,pid}<-prf.get_index_pid(data.destination,data.rdtype);
					`ifdef verbose $display($time,"\tEXECUTION: PRFINDEX: %d PID: %d",prf_index,pid); `endif
					rg_prf_index<=prf_index;
					rg_pid<=pid;
					/*========== Multiplication =============== */
					if(inst_type==MUL || inst_type == DIV)begin
						Bit#(1) is_mul=0;
						if(inst_type==MUL) begin
							is_mul=1;
							`ifdef verbose $display($time,"\tEXECUTION: Multiplication Operation Op1: %h Op2: %h ",rs1,rs2); `endif
						end
						else begin
							`ifdef verbose $display($time,"\tEXECUTION: Division Operation Op1: %h Op2: %h ",rs1,rs2); `endif
						end
						`ifdef sequential
							muldiv.input_operands(rs1,rs2,data.funct3[1:0],pack(data.word32),is_mul);
							prf.update_rd(prf_index,pid);
							multicylce_output[1]<=True;
						`endif
						`ifdef parallel
							let x= multiplication(rs1,rs2,data.funct3[1:0] `ifdef RV64 ,data.word32 `endif );
							rg_mul_output<=tagged Valid x;
							multicylce_output[1]<=True;
						`endif
					end
					`endif
					/*========================================== */
					/*============== FLOATING POINT ============================ */
					`ifdef spfpu
					else if(inst_type == FLOATING || inst_type==DFLOATING)begin
						`ifdef verbose $display($time,"\tEXECUTION: Floating Point Operation "); `endif
						fpu._start(truncate(rs1),truncate(rs2),truncate(rs3),data.fn,rs3[11:5],data.funct3,rs3[1:0],wr_rounding_mode, data.word32);
						multicylce_output[1]<=True;
						prf.update_rd(prf_index,pid);
					end
					`endif
					/*========================================== */
					/*================ SINGLE CYCLE ALU ========================== */
					else begin
						`ifdef verbose $display($time,"\tEXECUTION: Single Cycle Operation "); `endif
		   	      let {x,ea,flush,td,raspush,ex,pm} = fn_alu(data.fn,rs1,rs2,rs3,pc,data.inst_type,nextpc,data.funct3,data.mem_access,dest,data.prediction,rx.u.first.perfmonitors,data.word32);
						result1=x;
						`ifdef verbose $display($time,"\tEXE: rs1: %h rs2: %h rs3_imm: %h",rs1,rs2,rs3); `endif
						`ifdef verbose $display($time,"\tEXECUTION: Result: ",fshow(result1)); `endif
						`ifdef verbose $display($time,"\tEXECUTION: PC: %h",pc," Flush: ",fshow(flush)," EA: %h",ea," Instruction: ",fshow(data.inst_type)); `endif
						if(result1 matches tagged RESULT .res)begin
							if(dest!=0)
								wr_forward_from_EXE <= tagged Valid tuple3(res.aluresult,prf_index,pid);
						end
						else begin
							prf.update_rd(prf_index,pid);
						end
						if(result1 matches tagged MEMORY .meminfo)begin
							rx.u.deq;
							tx.u.enq(IE_IMEM_type{execresult:result1, 
								program_counter:pc,	exception:exception,	debugcause:rx.u.first.debugcause,
								destination:dest,		rd_type:rdtype ,  pid:pid, index:prf_index, perfmonitors:perfmonitors ,epochs:rx.u.first.epochs
								`ifdef simulate , instruction:instruction `endif  });
							wr_info_to_dmem.enq(tuple2(meminfo,rx.u.first.epochs[0]));
							rg_flush_execute<=flush;
							rg_effective_address<=ea;
						end
						else begin
							rx.u.deq;
							exception=ex;
							wr_ras_push<=raspush;
							rg_flush_execute<=flush;
							rg_effective_address<=ea;
							wr_training_data<=td;
							perfmonitors=pm;
							tx.u.enq(IE_IMEM_type{execresult:result1, 
								program_counter:pc,	exception:exception,	debugcause:rx.u.first.debugcause,
								destination:dest,		rd_type:rdtype ,  pid:pid, index:prf_index, perfmonitors:perfmonitors ,epochs:rx.u.first.epochs
								`ifdef simulate , instruction:instruction `endif  });
						end
					end
					/*========================================== */
				end
				else begin
					`ifdef verbose $display($time,"\tEXECUTION: Waiting for operands.\nRS1: ",fshow(op1),"\nRS2: ",fshow(op2),"\nRS3: ",fshow(op3)); `endif
				end
			end
			else begin
				rx.u.deq;
				`ifdef verbose $display($time,"\tEXECUTE: EXCEPTION"); `endif
				tx.u.enq(IE_IMEM_type{execresult:tagged RESULT Arithout{aluresult:0,fflags:0}, 
						program_counter:pc,	exception:exception,	debugcause:rx.u.first.debugcause,
						destination:dest,		rd_type:rdtype , index:rg_prf_index,  pid:rg_pid, perfmonitors:perfmonitors,epochs:rx.u.first.epochs
						`ifdef simulate , instruction:instruction `endif  });
			end
		endrule

		`ifdef muldiv
			`ifdef sequential
				rule read_outputs_from_muldiv(rx.u.notEmpty && tx.u.notFull && multicylce_output[1] );
					`ifdef verbose $display($time,"\tEXECUTION: Multiplier sending output to Memory stage"); `endif
					let res<-muldiv.muldiv_result;
					rx.u.deq;
					let decodedata=rx.u.first;
					let pc=decodedata.program_counter;
					let dest=decodedata.destination;
					let rdtype=decodedata.rdtype;
					let exception=decodedata.exception;
					`ifdef simulate let instr=decodedata.instruction; `endif
      		   Execution_output result1= tagged RESULT(Arithout{aluresult:res,fflags:0});
					if({eEpoch,wEpoch}!=rx.u.first.epochs)begin
						`ifdef verbose $display($time,"Epochs do not match"); `endif
					end
					else begin
						tx.u.enq(IE_IMEM_type{execresult:result1,debugcause:rx.u.first.debugcause, 
								program_counter:pc,	exception:exception,	
								destination:dest,		rd_type:rdtype , index:rg_prf_index,pid:rg_pid, perfmonitors:rx.u.first.perfmonitors,epochs:rx.u.first.epochs
								`ifdef simulate , instruction:instr `endif  });
						if(dest!=0)
							wr_forward_from_EXE <= tagged Valid tuple3(res,rg_prf_index,rg_pid);
					end
					multicylce_output[1]<=False;
				endrule
			`endif
			`ifdef parallel
				rule read_outputs_from_muldiv(rx.u.notEmpty &&& tx.u.notFull &&& multicylce_output[1] &&& rg_mul_output matches tagged Valid .x);
					rg_mul_output<=tagged Invalid;
					rx.u.deq;
					let decodedata=rx.u.first;
					let pc=decodedata.program_counter;
					let dest=decodedata.destination;
					let rdtype=decodedata.rdtype;
					let exception=decodedata.exception;
					`ifdef simulate let instr=decodedata.instruction; `endif
      	   	Execution_output result1= tagged RESULT(Arithout{aluresult:x,fflags:0});
					if({eEpoch,wEpoch}!=rx.u.first.epochs)begin
						`ifdef verbose $display($time,"Epochs do not match"); `endif
					end
					else begin
						tx.u.enq(IE_IMEM_type{execresult:result1,debugcause:rx.u.first.debugcause, 
								program_counter:pc,	exception:exception,	
								destination:dest,		rd_type:rdtype , index:rg_prf_index,pid:rg_pid, perfmonitors:rx.u.first.perfmonitors,epochs:rx.u.first.epochs
								`ifdef simulate , instruction:instr `endif  });
						if(dest!=0)
							wr_forward_from_EXE <= tagged Valid tuple3(x,rg_prf_index,rg_pid);
					end
					multicylce_output[1]<=False;
				endrule
			`endif
		`endif

	`ifdef spfpu
		rule read_output_from_fpu(rx.u.notEmpty && tx.u.notFull && multicylce_output[1] );
			let res<-fpu.get_result;
			rx.u.deq;
			let decodedata=rx.u.first;
			let pc=decodedata.program_counter;
			let dest=decodedata.destination;
			let rdtype=decodedata.rdtype;
			let exception=decodedata.exception;
			`ifdef simulate let instr=decodedata.instruction; `endif
         Execution_output result1= tagged RESULT(Arithout{aluresult:res.final_result, fflags:res.fflags});
			if({eEpoch,wEpoch}!=rx.u.first.epochs)begin
				`ifdef verbose $display($time,"Epochs do not match"); `endif
			end
			else begin
				tx.u.enq(IE_IMEM_type{execresult:result1, debugcause:rx.u.first.debugcause,
						program_counter:pc,	exception:exception,	
						destination:dest,		rd_type:rdtype, index:rg_prf_index,pid:rg_pid , perfmonitors:rx.u.first.perfmonitors,epochs:rx.u.first.epochs
						`ifdef simulate , instruction:instr `endif  });
				if((dest!=0 && rdtype==IntegerRF) || rdtype==FloatingRF)
					wr_forward_from_EXE <= tagged Valid tuple3(res.final_result,rg_prf_index,rg_pid);
			end
			multicylce_output[1]<=False;
		endrule
		`endif
		interface to_dmem = interface Get 
			method ActionValue#(Tuple2#(Memout,Bit#(1))) get ;
				`ifdef verbose $display($time,"\tEXECUTION: DEQUEING MEM REQUEST",fshow(wr_info_to_dmem.first)); `endif
				wr_info_to_dmem.deq;
				return wr_info_to_dmem.first;
			endmethod
		endinterface;
		method tx_out=tx.e;
		method rx_in=rx.e;
		method Action roundingmode(Bit#(3) rm);
			wr_rounding_mode<=rm;
		endmethod
		method  generate_flush=tuple2(rg_flush_execute,rg_effective_address);
		method Maybe#(Training_data#(`VADDR)) training_data=wr_training_data;
		method Maybe#(Bit#(`VADDR)) ras_push = wr_ras_push;
		method Action _forwarding_from_memory (Maybe#(Tuple3#(Bit#(`Reg_width), Bit#(TLog#(`PRFDEPTH)), Bit#(4))) fwd_data);
			if(fwd_data matches tagged Valid .fwdata)begin
				let {data,index,pid}=fwdata;
				prf.fwd_from_memory(data,index,pid);
			end
		endmethod
		method Action update_wEpoch;
			`ifdef verbose $display($time,"\tEXECUTION: Updating wEPOCH"); `endif
			wEpoch<=~wEpoch;
			wb_flush.send;
		endmethod
		method Action flush_prf;
			prf.flush_all;
		endmethod
	endmodule
endpackage:execute_stage
