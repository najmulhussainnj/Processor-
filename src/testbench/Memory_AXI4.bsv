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

package Memory_AXI4;
	import defined_types::*;
	`include "defined_parameters.bsv"
  	import BRAMCore :: *;
	import DReg::*;
	import Semi_FIFOF        :: *;
	import AXI4_Types   :: *;
	import AXI4_Fabric  :: *;
	import BUtils::*;

interface Memory_IFC#(numeric type base_address, numeric type mem_size);
	interface AXI4_Slave_IFC#(`PADDR,`Reg_width,`USERSPACE) axi_slave;
endinterface
typedef enum{Send_req,Get_resp} Mem_state deriving(Bits,Eq);
module mkMemory #(parameter String mem_init_file1 `ifdef RV64 , parameter String mem_init_file2 `endif  ,parameter String module_name) (Memory_IFC#(base_address,mem_size));
	
	`ifndef RV64
		BRAM_DUAL_PORT_BE#(Bit#(TSub#(mem_size,2)),Bit#(32),4) dmemLSB <- mkBRAMCore2BELoad(valueOf(TExp#(TSub#(mem_size,2))),False,mem_init_file1,False);
	`else
		BRAM_DUAL_PORT_BE#(Bit#(TSub#(mem_size,2)),Bit#(32),4) dmemMSB <- mkBRAMCore2BELoad(valueOf(TExp#(TSub#(mem_size,2))),False,mem_init_file1,False);
		BRAM_DUAL_PORT_BE#(Bit#(TSub#(mem_size,2)),Bit#(32),4) dmemLSB <- mkBRAMCore2BELoad(valueOf(TExp#(TSub#(mem_size,2))),False,mem_init_file2,False);
	`endif

	AXI4_Slave_Xactor_IFC #(`PADDR, `Reg_width, `USERSPACE)  s_xactor <- mkAXI4_Slave_Xactor;
	Reg#(Mem_state) rg_state[2] <-mkCReg(2,Send_req);
	 Reg#(Bit#(8)) rg_readburst_counter<-mkReg(0);
	 Reg#(Bit#(8)) rg_readburst_value<-mkReg(0);
	 Reg#(Bit#(8)) rg_writeburst_counter<-mkReg(0);
	 Reg#(Bit#(4)) rg_id<-mkReg(0);
	 Reg#(Bit#(`PADDR)) rg_address<-mkReg(0);
	 Reg#(Bit#(3)) rg_transfer_size<-mkReg(0);

	rule rl_wr_respond;
		// Get the wr request
      let aw <- pop_o (s_xactor.o_wr_addr);
      let w  <- pop_o (s_xactor.o_wr_data);
		Bit#(TSub#(mem_size,2)) index_address=(aw.awaddr-fromInteger(valueOf(base_address)))[valueOf(mem_size)-1:`byte_offset+1];
		dmemLSB.b.put(w.wstrb[3:0],index_address,truncate(w.wdata));
		`ifdef RV64 dmemMSB.b.put(w.wstrb[7:4],index_address,truncateLSB(w.wdata)); `endif
	   let b = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: aw.awuser, bid:aw.awid};
		if(rg_writeburst_counter==aw.awlen)begin
			rg_writeburst_counter<=0;
      	s_xactor.i_wr_resp.enq (b);
		end
		else
			rg_writeburst_counter<=rg_writeburst_counter+1;
			`ifdef verbose $display($time,"\t",module_name,":\t Recieved Write Request for Address: %h data: %h strb: %b awlen: %d rg_writeburst_counter: %d",aw.awaddr,w.wdata,w.wstrb,aw.awlen,rg_writeburst_counter);  `endif
	endrule

	rule rl_rd_request(rg_state[1]==Send_req);
		  let ar<- pop_o(s_xactor.o_rd_addr);
			Bit#(TSub#(mem_size,2)) index_address=(ar.araddr-fromInteger(valueOf(base_address)))[valueOf(mem_size)-1:`byte_offset+1];
			rg_address<=ar.araddr;
			rg_transfer_size<=ar.arsize;
			rg_readburst_value<=ar.arlen;
			rg_id<=ar.arid;
			dmemLSB.a.put(0,index_address,?);
			`ifdef RV64 dmemMSB.a.put(0,index_address,?); `endif
			rg_state[1]<=Get_resp;
			`ifdef verbose $display($time,"\t",module_name,":\t Recieved Read Request for Address: %h Index Address: %h",ar.araddr,index_address);  `endif
	 endrule

	rule rl_rd_response(rg_state[0]==Get_resp);
		`ifdef RV64
	    	Bit#(`Reg_width) data0 = {dmemMSB.a.read(),dmemLSB.a.read()};
		`else 
	    	Bit#(`Reg_width) data0 = dmemLSB.a.read();
		`endif
		if(data0=='haaaaaaaaaaaaaaaa)
			data0=0;
      AXI4_Rd_Data#(`Reg_width,`USERSPACE) r = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: data0 ,rlast:rg_readburst_counter==rg_readburst_value, ruser: 0, rid:rg_id};
		if(rg_transfer_size==2)begin // 32 bit
			if(rg_address[`byte_offset:0]==0)
				r.rdata=duplicate(data0[31:0]);
			else
				r.rdata=duplicate(data0[63:32]);
		end
      else if (rg_transfer_size=='d1)begin // half_word
			if(rg_address[`byte_offset:0] ==0)
				r.rdata = duplicate(data0[15:0]);
			else if(rg_address[`byte_offset:0] ==2)
				r.rdata = duplicate(data0[31:16]);
			`ifdef RV64
				else if(rg_address[`byte_offset:0] ==4)
					r.rdata = duplicate(data0[47:32]);
				else if(rg_address[`byte_offset:0] ==6)
					r.rdata = duplicate(data0[63:48]);
			`endif
      end
      else if (rg_transfer_size=='d0) begin// one byte
			if(rg_address[`byte_offset:0] ==0)
        	  r.rdata = duplicate(data0[7:0]);
        	else if(rg_address[`byte_offset:0] ==1)
        	  r.rdata = duplicate(data0[15:8]);
        	else if(rg_address[`byte_offset:0] ==2)
        	  r.rdata = duplicate(data0[23:16]);
        	else if(rg_address[`byte_offset:0] ==3)
        	  r.rdata = duplicate(data0[31:24]);
		  	`ifdef RV64
        		else if(rg_address[`byte_offset:0] ==4)
					r.rdata = duplicate(data0[39:32]);
        		else if(rg_address[`byte_offset:0] ==5)
					r.rdata = duplicate(data0[47:40]);
        		else if(rg_address[`byte_offset:0] ==6)
					r.rdata = duplicate(data0[55:48]);
        		else if(rg_address[`byte_offset:0] ==7)
					r.rdata = duplicate(data0[63:56]);
			`endif
      end
      s_xactor.i_rd_data.enq(r);
		rg_state[0]<=Send_req;
		if(rg_readburst_counter==rg_readburst_value)
			rg_readburst_counter<=0;
		else
			rg_readburst_counter<=rg_readburst_counter+1;
		Bit#(64) new_data=r.rdata;
		`ifdef verbose $display($time,"\t",module_name,":\t Responding Read Request with Data: %8h BurstCounter: %d BurstValue: %d",new_data,rg_readburst_counter,rg_readburst_value);  `endif
   endrule

   interface axi_slave= s_xactor.axi_side;
endmodule
endpackage
