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
package prf;
	import defined_types::*;
	`include "defined_parameters.bsv"
	import Vector::*;
	interface Ifc_prf_new;
		method ActionValue#(RFType#(`Reg_width)) read_rs1 (Bit#(5) addr, Operand_type rs1type, Bit#(`Reg_width) data);
		method ActionValue#(RFType#(`Reg_width)) read_rs2 (Bit#(5) addr, Operand_type rs2type, Bit#(`Reg_width) data);
		method ActionValue#(RFType#(`Reg_width)) read_rs3 (Bit#(5) addr, Operand_type rs3type, Bit#(`Reg_width) data);
		method ActionValue#(Tuple2#(Bit#(TLog#(`PRFDEPTH)),Bit#(4))) get_index_pid(Bit#(5) addr, Operand_type rdtype);
		method Action update_rd (Bit#(TLog#(`PRFDEPTH)) index, Bit#(4) pid);
		method Action fwd_from_execution (Bit#(`Reg_width) data, Bit#(TLog#(`PRFDEPTH)) index, Bit#(4) pid);
		method Action fwd_from_memory (Bit#(`Reg_width) data, Bit#(TLog#(`PRFDEPTH)) index, Bit#(4) pid);
		method Action flush_all;
	endinterface
	(*conflict_free="fwd_from_execution,fwd_from_memory"*)
	(*conflict_free="fwd_from_execution,update_rd"*)
	(*conflict_free="update_rd,fwd_from_memory"*)
	(*synthesize*)
	module mkprf_new(Ifc_prf_new);
		Reg#(RFType#(`Reg_width)) physical_rf [`PRFDEPTH];
		Reg#(Bit#(4)) pid_rf [`PRFDEPTH];
		Vector#(`PRFDEPTH,Reg#(Tuple2#(Bit#(5),Operand_type))) rd_rf<-replicateM(mkReg(tuple2(0,IntegerRF)));
		Reg#(Bit#(4)) rg_pid_counter<-mkReg(0);
		Reg#(Bit#(TLog#(`PRFDEPTH))) rg_prf_index<-mkReg(0);
		Wire#(Bool) wr_flush<-mkDWire(False);
		for(Integer i=0;i<`PRFDEPTH;i=i+1)begin
			physical_rf[i]<-mkReg(tagged Absent 0);
			pid_rf[i]<-mkReg(0);
		end
		rule flush_all_mapping(wr_flush);
			for(Integer i=0;i<`PRFDEPTH;i=i+1)begin
				physical_rf[i]<=tagged Absent 0;
				pid_rf[i]<=0;
				rd_rf[i]<=tuple2(0,IntegerRF);
			end
		endrule
		method ActionValue#(RFType#(`Reg_width)) read_rs1 (Bit#(5) addr, Operand_type rs1type, Bit#(`Reg_width) data);
			if(rs1type==IntegerRF && addr==0)
				return tagged Present 0;
			else if(rs1type==IntegerRF `ifdef spfpu || rs1type==FloatingRF `endif ) begin
				let array_rd=readVReg(rd_rf); // convert the reg-vector to bit#(5)-vector
				let index=findElem(tuple2(addr,rs1type),array_rd); // find the index of match
				if(index matches tagged Valid .idx)begin // if match exists
					`ifdef verbose $display($time,"\tPRF: READ_RS1: valid index: %d",idx); `endif
					return physical_rf[idx];
				end
				else // there is no instruction in pipe updating this reg.
					return tagged Present data;	
			end
				else // there is no instruction in pipe updating this reg.
					return tagged Present data;	
		endmethod
		method ActionValue#(RFType#(`Reg_width)) read_rs2 (Bit#(5) addr, Operand_type rs2type, Bit#(`Reg_width) data);
			if(rs2type==IntegerRF && addr==0)
				return tagged Present 0;
			else if(rs2type==IntegerRF `ifdef spfpu || rs2type==FloatingRF `endif ) begin
				let array_rd=readVReg(rd_rf); // convert the reg-vector to bit#(5)-vector
				let index=findElem(tuple2(addr,rs2type),array_rd); // find the index of match
				if(index matches tagged Valid .idx)begin // if match exists
					`ifdef verbose $display($time,"\tPRF: READ_RS2: valid index: %d",idx); `endif
					return physical_rf[idx];
				end
				else // there is no instruction in pipe updating this reg.
					return tagged Present data;	
			end
				else // there is no instruction in pipe updating this reg.
					return tagged Present data;	
		endmethod
		method ActionValue#(RFType#(`Reg_width)) read_rs3 (Bit#(5) addr, Operand_type rs3type, Bit#(`Reg_width) data);
			if(rs3type==IntegerRF && addr==0)
				return tagged Present 0;
			else if(rs3type==IntegerRF `ifdef spfpu || rs3type==FloatingRF `endif ) begin
				let array_rd=readVReg(rd_rf); // convert the reg-vector to bit#(5)-vector
				let index=findElem(tuple2(addr,rs3type),array_rd); // find the index of match
				if(index matches tagged Valid .idx)begin // if match exists
					return physical_rf[idx];
				end
				else // there is no instruction in pipe updating this reg.
					return tagged Present data;	
			end
				else // there is no instruction in pipe updating this reg.
					return tagged Present data;	
		endmethod
		method ActionValue#(Tuple2#(Bit#(TLog#(`PRFDEPTH)),Bit#(4))) get_index_pid(Bit#(5) addr, Operand_type rdtype);
			let array_rd=readVReg(rd_rf);
			let index=findElem(tuple2(addr,rdtype),array_rd); // find the index of match
			rg_pid_counter<=rg_pid_counter+1;
			if(rg_prf_index==`PRFDEPTH-1)
				rg_prf_index<=0;
			else
				rg_prf_index<=rg_prf_index+1;
			rd_rf[rg_prf_index]<=tuple2(addr,rdtype);
			if(index matches tagged Valid .idx)begin
				if(pack(idx)!=rg_prf_index)
					rd_rf[idx]<=tuple2(0,IntegerRF);
				`ifdef verbose $display($time,"\tPRF: Removing previously alotted index: %d",idx); `endif
			end
				`ifdef verbose $display($time,"\tPRF: Giving Index: %d PID: %d",rg_prf_index, rg_pid_counter); `endif
			return tuple2(rg_prf_index,rg_pid_counter);
		endmethod
		method Action update_rd (Bit#(TLog#(`PRFDEPTH)) index, Bit#(4) pid);
			physical_rf[index]<=tagged Absent pid;
			pid_rf[index]<=pid;
		endmethod
		method Action fwd_from_execution (Bit#(`Reg_width) data, Bit#(TLog#(`PRFDEPTH)) index, Bit#(4) pid);
			`ifdef verbose $display($time,"\tPRF: FWD from EXE Data: %h index: %d pid: %d",data,index,pid); `endif
			physical_rf[index]<=tagged Present data;	
		endmethod
		method Action fwd_from_memory (Bit#(`Reg_width) data, Bit#(TLog#(`PRFDEPTH)) index, Bit#(4) pid);
			`ifdef verbose $display($time,"\tPRF: FWD from MEM Data: %h index: %d pid: %d",data,index,pid); `endif
			physical_rf[index]<=tagged Present data;	
		endmethod
		method Action flush_all;
			wr_flush<=True;
		endmethod

	endmodule
endpackage
