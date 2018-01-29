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
package sample_axiexpslave_small;
	/*=== Package imports === */
	import FIFO::*;
	import FIFOF::*;
	import SpecialFIFOs::*;
	import BUtils::*;
	import Connectable::*;
	import GetPut::*;
  	import BRAMCore :: *;
	/*==== Project imports === */
	import defined_types::*;
	`include "defined_parameters.bsv"
	/*======================== */
	`define BRAM_DEPTH 20

	interface Ifc_sample_axiexpslave;
		interface Get#(Bit#(35)) to_slave;
		interface Put#(Bit#(35)) from_slave;
	endinterface

	typedef enum {WriteResponse1,WriteResponse2,WriteResponse3,ReadResponse1,ReadResponse2,ReadResponse3,Idle} State deriving(Eq,Bits,FShow);
    (*synthesize*)
	module mksample_axiexpslave(Ifc_sample_axiexpslave);
		FIFO#(Bit#(35)) get_slave_req <-mkLFIFO();
		FIFO#(Bit#(35)) send_slave_resp <-mkLFIFO();
		Reg#(State) rg_state<-mkReg(Idle);
		BRAM_DUAL_PORT_BE#(Bit#(TSub#(`BRAM_DEPTH,2)),Bit#(32),4) dmemMSB <- mkBRAMCore2BELoad(valueOf(TExp#(TSub#(`BRAM_DEPTH,2))),False,"axi.mem.MSB",False);
		BRAM_DUAL_PORT_BE#(Bit#(TSub#(`BRAM_DEPTH,2)),Bit#(32),4) dmemLSB <- mkBRAMCore2BELoad(valueOf(TExp#(TSub#(`BRAM_DEPTH,2))),False,"axi.mem.LSB",False);
		Reg#(Bit#(`PADDR)) rg_address<-mkReg(0);
		Reg#(Bit#(22)) rg_metadata <-mkReg(0);
		Reg#(Bit#(3)) rg_transfer_size<-mkReg(0);
		Reg#(Bit#(8)) rg_readburst_value<-mkReg(0);
		Reg#(Bit#(32)) rg_data <-mkReg(0);
		Reg#(Bit#(8)) rg_wr_strb <-mkReg(0);

		rule show_stage;
			$display($time,"\tAXI_EXP_SLAVE: STATE: ",fshow(rg_state));
		endrule

		rule read_from_bram(get_slave_req.first[33:32]==2 && rg_state==Idle); // read operation 
			let address=get_slave_req.first[31:0];	
			rg_state<=ReadResponse1;
			rg_address<=address;
			get_slave_req.deq;
		endrule
		rule read_from_bram2(get_slave_req.first[33:32]==2 && rg_state==ReadResponse1);
			Bit#(TSub#(`BRAM_DEPTH,2)) index_address=(rg_address-fromInteger(valueOf(`AxiExp1Base)))[valueOf(`BRAM_DEPTH)-1:3];
			rg_transfer_size<=get_slave_req.first[19:17];
			rg_readburst_value<=get_slave_req.first[16:9];
			rg_state<=ReadResponse2;
			get_slave_req.deq;
			dmemLSB.a.put(0,index_address,?);
			`ifdef RV64 dmemMSB.a.put(0,index_address,?); `endif
		endrule
		rule send_response(rg_state==ReadResponse2);
	    	Bit#(`Reg_width) data0 = {dmemMSB.a.read(),dmemLSB.a.read()};
			Bit#(64) response=zeroExtend(data0);
			if(rg_transfer_size==2)begin // 32 bit
				if(rg_address[`byte_offset:0]==0)
					response[63:0]=duplicate(data0[31:0]);
				else
					response[63:0]=duplicate(data0[63:32]);
			end
      	else if (rg_transfer_size=='d1)begin // half_word
				if(rg_address[`byte_offset:0] ==0)
					response[63:0] = duplicate(data0[15:0]);
				else if(rg_address[`byte_offset:0] ==2)
					response[63:0] = duplicate(data0[31:16]);
				`ifdef RV64
					else if(rg_address[`byte_offset:0] ==4)
						response[63:0] = duplicate(data0[47:32]);
					else if(rg_address[`byte_offset:0] ==6)
						response[63:0] = duplicate(data0[63:48]);
				`endif
      	end
      	else if (rg_transfer_size=='d0) begin// one byte
				if(rg_address[`byte_offset:0] ==0)
      	  	  response[63:0] = duplicate(data0[7:0]);
      	  	else if(rg_address[`byte_offset:0] ==1)
      	  	  response[63:0] = duplicate(data0[15:8]);
      	  	else if(rg_address[`byte_offset:0] ==2)
      	  	  response[63:0] = duplicate(data0[23:16]);
      	  	else if(rg_address[`byte_offset:0] ==3)
      	  	  response[63:0] = duplicate(data0[31:24]);
			  	`ifdef RV64
      	  		else if(rg_address[`byte_offset:0] ==4)
						response[63:0] = duplicate(data0[39:32]);
      	  		else if(rg_address[`byte_offset:0] ==5)
						response[63:0] = duplicate(data0[47:40]);
      	  		else if(rg_address[`byte_offset:0] ==6)
						response[63:0] = duplicate(data0[55:48]);
      	  		else if(rg_address[`byte_offset:0] ==7)
						response[63:0] = duplicate(data0[63:56]);
				`endif
      	end
			rg_data<=response[63:32];
	      send_slave_resp.enq(zeroExtend(response[31:0]));
			rg_state<=ReadResponse3;
		endrule
		rule send_msb_word(rg_state==ReadResponse3);
			send_slave_resp.enq(zeroExtend(rg_data));
			rg_state<=Idle;
		endrule

		rule write_addr_to_bram(get_slave_req.first[33:32]==0 && rg_state==Idle); // write address fetch 
			rg_address <=get_slave_req.first[31:0];	
			get_slave_req.deq;
		   rg_state<=WriteResponse1;
		endrule
		
		rule write_addr_to_bram2(get_slave_req.first[33:32]==0 && rg_state==WriteResponse1); // write address fetch 
         rg_wr_strb <= get_slave_req.first[8:1];
			get_slave_req.deq;
		   rg_state<=WriteResponse2;
		endrule
		
		rule write_addr_to_bram3(get_slave_req.first[33:32]==1 && rg_state==WriteResponse2); // write address fetch 
         rg_data <= get_slave_req.first[31:0];
			get_slave_req.deq;
		   rg_state<=WriteResponse3;
		endrule

		rule write_data_to_bram(get_slave_req.first[33:32]==1 && rg_state==WriteResponse3); // write data operation 
			Bit#(64) data1 ={get_slave_req.first[31:0],rg_data};	
			Bit#(TSub#(`BRAM_DEPTH,2)) index_address1 =(rg_address-fromInteger(valueOf(`AxiExp1Base)))[valueOf(`BRAM_DEPTH)-1:3];
			dmemLSB.a.put(rg_wr_strb[3:0],index_address1,data1[31:0]);
			`ifdef RV64 dmemMSB.a.put(rg_wr_strb[7:4],index_address1,data1[63:32]); `endif
         send_slave_resp.enq(0);
			get_slave_req.deq;
			rg_state<=Idle;
		endrule

		interface from_slave=interface Put
			method Action put (Bit#(35) request);
				get_slave_req.enq(request);
			endmethod
		endinterface;
		interface to_slave=interface Get
			method ActionValue#(Bit#(35)) get;
				send_slave_resp.deq;
				return send_slave_resp.first;
			endmethod
		endinterface;
	endmodule
endpackage
