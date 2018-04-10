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
package BootRom;
	import defined_types::*;
	`include "defined_parameters.bsv"
  	import BRAMCore :: *;
	import DReg::*;
	import GetPut ::*;
	import BUtils::*;
	`ifdef TILELINK
		import Tilelink_Types ::*;
		import Tilelink ::*;
		import TLMemoryMap ::*;
		import tilelink_addr_generator ::*;
	`else
		import Semi_FIFOF        :: *;
		import AXI4_Types   :: *;
		import AXI4_Fabric  :: *;
		import axi_addr_generator::*;
	`endif

interface BootRom_IFC;
	`ifdef TILELINK
		interface Ifc_fabric_side_slave_link#(`PADDR,`Reg_width, 4) slave_ifc;
	`else
		interface AXI4_Slave_IFC#(`paddr,`reg_width,`userspace) slave_ifc;
	`endif
endinterface
typedef enum{Idle,HandleBurst} Mem_state deriving(Bits,Eq);
(*synthesize*)
module mkBootRom (BootRom_IFC);

	// we create 2 32-bit BRAMs since the xilinx tool is easily able to map them to BRAM32BE cells
	// which makes it easy to use data2mem for updating the bit file.
	BRAM_PORT#(Bit#(13),Bit#(32)) dmemMSB <- mkBRAMCore1Load(valueOf(TExp#(13)),False,"boot.MSB",False);
	BRAM_PORT#(Bit#(13),Bit#(32)) dmemLSB <- mkBRAMCore1Load(valueOf(TExp#(13)),False,"boot.LSB",False);

	`ifdef TILELINK
		Ifc_Slave_link#(`PADDR,`Reg_width, 4) s_xactor <- mkSlaveXactor(True, True);
	`else
		AXI4_Slave_Xactor_IFC #(`PADDR, `Reg_width, `USERSPACE)  s_xactor <- mkAXI4_Slave_Xactor;
	`endif

	Reg#(Mem_state) rd_state <-mkReg(Idle);
	Reg#(Mem_state) wr_state <-mkReg(Idle);


	`ifdef TILELINK

		Reg#(A_channel#(`PADDR,`Reg_width, 4)) rg_read_packet <-mkReg(?);
		Reg#(Bit#(12)) rg_readburst_counter<-mkReg(0);
	
	`else
		Reg#(Bit#(8)) rg_readburst_counter<-mkReg(0);
	Reg#(AXI4_Rd_Addr	#(`PADDR,`USERSPACE)) rg_read_packet <-mkReg(?);
	Reg#(AXI4_Wr_Resp	#(`USERSPACE)) rg_write_response <-mkReg(?);

		rule rl_wr_respond(wr_state==Idle);
    	  let aw <- pop_o (s_xactor.o_wr_addr);
    	  let w  <- pop_o (s_xactor.o_wr_data);
		   let b = AXI4_Wr_Resp {bresp: AXI4_SLVERR, buser: aw.awuser, bid:aw.awid};
			rg_write_response<=b;
			$display($time,"\tBootROM: Illegal Write operation on BootROM");
			if(aw.awburst!=0)
				wr_state<=HandleBurst;
			else
		     	s_xactor.i_wr_resp.enq (b);
		endrule

		rule rl_wr_burst_response(wr_state==HandleBurst);
    	  let w  <- pop_o (s_xactor.o_wr_data);
			if(w.wlast) begin
				wr_state<=Idle;
		    	s_xactor.i_wr_resp.enq (rg_write_response);
			end
		endrule
	`endif

	rule rl_rd_request(rd_state==Idle);
		`ifdef TILELINK
			let ar<- s_xactor.core_side.xactor_request.get;
			let address = ar.a_address;
           	Data_size beat_blocks = ar.a_size - 3;
            Bit#(12) burst_counter = 1;
            burst_counter = burst_counter << beat_blocks;
            rg_readburst_counter <= burst_counter-1;
		`else
			let ar<- pop_o(s_xactor.o_rd_addr);
			let address = ar.araddr;
		`endif
		rg_read_packet<=ar;
	  	Bit#(13) index_address=(address-`BootRomBase)[15:3];
		dmemLSB.put(False,index_address,?);
		dmemMSB.put(False,index_address,?);
		rd_state<=HandleBurst;
		`ifdef verbose $display($time,"\tBootROM: Recieved Read Request for Address: %h Index Address: %h",address, index_address);  `endif
	endrule

	rule rl_rd_response(rd_state==HandleBurst);
	   Bit#(`Reg_width) data0 = {dmemMSB.read(),dmemLSB.read()};
		let ar = rg_read_packet;
		`ifdef TILELINK 
			let address = rg_read_packet.a_address;
			let mask = rg_read_packet.a_mask;
			let {transfer_size, addr} = burst_address_generator(ar.a_opcode, mask, address, ar.a_size);	
			let r = D_channel {d_opcode : AccessAckData, d_param : 0, d_size: transfer_size, d_source : ar.a_source,
				                                                                         d_data : data0,  d_error: False}; 
        	Bit#(TMul#(`LANE_WIDTH,2)) mask_double = zeroExtend(mask);
        	mask_double = mask_double << transfer_size;
        	mask = mask_double[2*v_lane_width-1: v_lane_width] | mask_double[v_lane_width-1:0]; // this is for the wrap
        	ar.a_mask = mask;
        	ar.a_address = addr;
		`else
      		AXI4_Rd_Data#(`Reg_width,`USERSPACE) r = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: data0 ,rlast:rg_readburst_counter==rg_read_packet.arlen, ruser: 0, rid:rg_read_packet.arid};
			let transfer_size=rg_read_packet.arsize;
			let address=rg_read_packet.araddr;
		`endif
		if(transfer_size==2)begin // 32 bit
			if(address[2:0]==0)
				data0=duplicate(data0[31:0]);
			else
				data0=duplicate(data0[63:32]);
		end
      else if (transfer_size=='d1)begin // half_word
			if(address[2:0] ==0)
				data0 = duplicate(data0[15:0]);
			else if(address[2:0] ==2)
				data0 = duplicate(data0[31:16]);
			else if(address[2:0] ==4)
				data0 = duplicate(data0[47:32]);
			else if(address[2:0] ==6)
				data0 = duplicate(data0[63:48]);
      end
      else if (transfer_size=='d0) begin// one byte
			if(address[2:0] ==0)
        		data0 = duplicate(data0[7:0]);
        	else if(address[2:0] ==1)
        	  	data0 = duplicate(data0[15:8]);
        	else if(address[2:0] ==2)
        	 	data0 = duplicate(data0[23:16]);
        	else if(address[2:0] ==3)
        		data0 = duplicate(data0[31:24]);
        	else if(address[2:0] ==4)
				data0 = duplicate(data0[39:32]);
        	else if(address[2:0] ==5)
				data0 = duplicate(data0[47:40]);
        	else if(address[2:0] ==6)
				data0 = duplicate(data0[55:48]);
        	else if(address[2:0] ==7)
				data0 = duplicate(data0[63:56]);
      end
		`ifdef TILELINK
			 r.d_data = data0;
			 s_xactor.core_side.xactor_response.put(r);
	  		 Bit#(13) index_address=(addr-`BootRomBase)[15:3];
             if(rg_readburst_counter==0)begin
                 rd_state<=Idle;
             end
             else begin
                 dmemLSB.put(False,index_address,?);
                 dmemMSB.put(False,index_address,?);
                 rg_readburst_counter<=rg_readburst_counter-1;
             end
             rg_read_packet <= ar;
             Bit#(64) new_data=r.d_data;
             `ifdef verbose $display($time,"\t BootRom : Responding Read Request with CurrAddr: %h Data: %8h BurstCounter: %d NextAddress: %h",addr,new_data,rg_readburst_counter,address);  `endif
		`else
			r.rdata = data0;
      		s_xactor.i_rd_data.enq(r);
			address=burst_address_generator(rg_read_packet.arlen, rg_read_packet.arsize, rg_read_packet.arburst,rg_read_packet.araddr);
	  		Bit#(13) index_address=(address-`BootRomBase)[15:3];
			if(rg_readburst_counter==rg_read_packet.arlen)begin
				rg_readburst_counter<=0;
				rd_state<=Idle;
			end
			else begin
				dmemLSB.put(False,index_address,?);
				dmemMSB.put(False,index_address,?);
				rg_readburst_counter<=rg_readburst_counter+1;
			end
			rg_read_packet.araddr<=address;
			Bit#(64) new_data=r.rdata;
			`ifdef verbose $display($time,"\tBootROM : Responding Read Request with CurrAddr: %h Data: %8h BurstCounter: %d BurstValue: %d NextAddress: %h",rg_read_packet.araddr,new_data,rg_readburst_counter,rg_read_packet.arlen,address);  `endif
		`endif
   endrule

	`ifdef TILELINK
		interface slave_ifc = s_xactor.fabric_side;
	`else
   		interface slave_ifc= s_xactor.axi_side;
	`endif
endmodule
endpackage
