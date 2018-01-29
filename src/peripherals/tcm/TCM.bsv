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
package TCM;
	import defined_types::*;
	`include "defined_parameters.bsv"
  	import BRAMCore :: *;
	import Semi_FIFOF        :: *;
	import AXI4_Types   :: *;
	import AXI4_Fabric  :: *;
	import BUtils  :: *;
    import FIFOF::*;
    import DReg::*;
    `define verbose
interface Ifc_TCM;
	interface AXI4_Slave_IFC#(`PADDR,`Reg_width,`USERSPACE) axi_slave;
endinterface
typedef enum{Send_rd_req,Get_rd_resp,Send_wr_req,Get_wr_resp,Idle} Mem_state deriving(Bits,Eq,FShow);
(*synthesize*)
module mkTCM (Ifc_TCM);
	
//	BRAM_DUAL_PORT#(Bit#(8),Bit#(64)) data [64];
//	for(Integer i=0;i<64;i=i+1)
//		data[i] <- mkBRAMCore2(256,False);
	BRAM_DUAL_PORT#(Bit#(14), Bit#(32)) dataLSB <-mkBRAMCore2(16384,False);
	BRAM_DUAL_PORT#(Bit#(14), Bit#(32)) dataMSB <-mkBRAMCore2(16384,False);

	AXI4_Slave_Xactor_IFC #(`PADDR, `Reg_width, `USERSPACE)  s_xactor <- mkAXI4_Slave_Xactor;
	Reg#(Mem_state) rg_state[2] <-mkCReg(2,Idle);
	Reg#(Bit#(8)) rg_readburst_counter<-mkReg(0);
	Reg#(Bit#(8)) rg_readburst_value<-mkReg(0);
	Reg#(Bit#(8)) rg_writeburst_counter<-mkReg(0);
	Reg#(Bit#(4)) rg_id<-mkReg(0);
	Reg#(Bit#(`PADDR)) rg_address<-mkReg(0);
	Reg#(Bit#(3)) rg_transfer_size<-mkReg(0);
	Reg#(Bit#(`USERSPACE)) rg_buser <- mkReg(0);
	Reg#(Bit#(TDiv#(`Reg_width, 8))) rg_wrstrb <- mkReg(0);
	Reg#(Bit#(`Reg_width)) rg_data<-mkReg(0);
	Reg#(Bool) rg_wlast<-mkReg(False);
    Reg#(Bool) read_last_address <- mkDReg(False);
    Reg#(Bit#(`Reg_width)) rg_last_read <- mkReg(0);
    FIFOF#(Bit#(64)) ff_input_fifo <- mkSizedFIFOF(3); 

	(*preempts="rl_wr_request, rl_rd_request"*)
	rule rl_wr_request(rg_state[1]==Send_wr_req || rg_state[1]==Idle);
		// Get the wr request
      let aw <- pop_o (s_xactor.o_wr_addr);
      let w  <- pop_o (s_xactor.o_wr_data);
	   let b = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: aw.awuser, bid:aw.awid};
		Bit#(14) index_address= aw.awaddr[16:3];
        Bit#(TSub#(`PADDR,3)) add_index = rg_address[`PADDR-1:3];
		rg_address <= aw.awaddr;
		rg_buser <= aw.awuser;
		rg_id <= aw.awid;
		rg_transfer_size<=aw.awsize;
		rg_wrstrb <= w.wstrb;
		rg_data <= w.wdata;
		rg_readburst_value <= aw.awlen;
        rg_wlast <= w.wlast;
		dataMSB.a.put(False,index_address,?);
		dataLSB.a.put(False,index_address,?);
		rg_state[1] <= Send_wr_req;
        ff_input_fifo.enq(w.wdata);
        if(add_index==aw.awaddr[`PADDR-1:3]) begin
            read_last_address <= True;
            //$display($time,"\t rg_last_address True\n");
        end
		`ifdef verbose $display($time,"\tTCM:\t Recieved Write Request for Address: %h data: %h strb: %b awlen: %d ",aw.awaddr,w.wdata,w.wstrb,aw.awlen);  `endif
	endrule

	rule rl_wr_response(rg_state[0]==Send_wr_req);
	  Bit#(`Reg_width) word = {dataMSB.a.read,dataLSB.a.read};
        let rg_data = ff_input_fifo.first;
        ff_input_fifo.deq; 
        if(read_last_address)
            word = rg_last_read;
		
        if(rg_wrstrb[0]==1)
			word[7:0] = rg_data[7:0];
		if(rg_wrstrb[1]==1)
			word[15:7] = rg_data[15:7];
		if(rg_wrstrb[2]==1)
			word[23:16] = rg_data[23:16];
		if(rg_wrstrb[3]==1)
			word[31:24] = rg_data[31:24];
		if(rg_wrstrb[4]==1)
			word[39:32] = rg_data[39:32];
		if(rg_wrstrb[5]==1)
			word[47:40] = rg_data[47:40];
		if(rg_wrstrb[6]==1)
			word[55:48] = rg_data[55:48];
		if(rg_wrstrb[7]==1)
			word[63:56] = rg_data[63:56];

		
		dataMSB.b.put(True,rg_address[16:3],word[63:32]);
		dataLSB.b.put(True,rg_address[16:3],word[31:0]);
        rg_last_read <= word;
		if(rg_wlast)begin
			rg_writeburst_counter<=0;
            s_xactor.i_wr_resp.enq (AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: rg_buser, bid:rg_id});
			rg_state[0] <= Idle;
		end
		else begin
			rg_writeburst_counter<=rg_writeburst_counter+1;
			//rg_state[1] <= Send_wr_req;
		end
		`ifdef verbose $display($time,"\tTCM:\t Writing into Address: %h data: %h strb: %b if wlast %d rg_writeburst_counter: %d",rg_address,word,rg_wrstrb,rg_wlast,rg_writeburst_counter);  `endif

	endrule

	rule rl_rd_request(rg_state[1]==Send_rd_req || rg_state[1]==Idle);
		let ar<- pop_o(s_xactor.o_rd_addr);
		Bit#(14) index_address=ar.araddr[16:3];
		rg_address<=ar.araddr;
		rg_transfer_size<=ar.arsize;
		rg_readburst_value<=ar.arlen;
		rg_id<=ar.arid;
		dataMSB.a.put(False,index_address,?);
		dataLSB.a.put(False,index_address,?);
		rg_state[1]<=Get_rd_resp;
        `ifdef verbose $display($time,"\tTCM received read request for address %h, transfer size %d burst length %d", ar.araddr, ar.arsize, ar.arlen); `endif
	 endrule

	rule rl_rd_response(rg_state[0]==Get_rd_resp);
	   Bit#(`Reg_width) word = {dataMSB.a.read,dataLSB.a.read};
		if(rg_transfer_size==3) begin
			word = word;
		end
		else if(rg_transfer_size==2) begin 
			if(rg_address[2]==0)
				word = duplicate(word[31:0]);
			else
				word = duplicate(word[63:32]);
		end
		else if(rg_transfer_size==1) begin 
			if(rg_address[2:1]=='b0)
				word = duplicate(word[15:0]);
			else if(rg_address[2:1]=='b01)
				word = duplicate(word[31:16]);
			else if(rg_address[2:1]=='b10)
				word = duplicate(word[47:32]);
			else 
				word = duplicate(word[63:48]);
		end
		else begin
			if(rg_address[2:0]=='b0)
				word = duplicate(word[7:0]);
			else if(rg_address[2:0]=='b001)
				word = duplicate(word[15:8]);
			else if(rg_address[2:0]=='b010)
				word = duplicate(word[23:16]);
			else if(rg_address[2:0]=='b011)
				word = duplicate(word[31:24]);
			else if(rg_address[2:0]=='b100)
				word = duplicate(word[39:32]);
			else if(rg_address[2:0]=='b101)
				word = duplicate(word[47:40]);
			else if(rg_address[2:0]=='b110)
				word = duplicate(word[55:48]);
			else 
				word = duplicate(word[63:56]);
		end
    AXI4_Rd_Data#(`Reg_width,`USERSPACE) r = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: word ,rlast:rg_readburst_counter==rg_readburst_value, ruser: 0, rid:rg_id};
    s_xactor.i_rd_data.enq(r);
       if(rg_readburst_counter==rg_readburst_value) begin
			rg_readburst_counter<=0;
            rg_state[0]<=Idle;
        end
        else begin
			rg_readburst_counter<=rg_readburst_counter+1;
            rg_state[0]<=Send_rd_req;
        end
        `ifdef verbose $display($time,"\tTCM responding data %h, transfer size %d, burst number %d", word, rg_transfer_size, rg_readburst_counter); `endif
   endrule

   interface axi_slave= s_xactor.axi_side;
endmodule
endpackage
