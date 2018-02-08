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
package AXI4Lite_AXI4_Bridge;
	import AXI4_Lite_Fabric::*;
	import AXI4_Lite_Types::*;
	import AXI4_Fabric::*;
	import AXI4_Types ::*;
	import Semi_FIFOF	::*;
	import defined_types::*;
	`include "defined_parameters.bsv"
	`include "AXI4Lite.defines"

	interface Ifc_AXI4Lite_AXI4_Bridge;
		interface AXI4_Slave_IFC#(`PADDR,`Reg_width,`USERSPACE) axi_slave;
		interface AXI4_Master_IFC#(`PADDR,64,0) axi4_lite_master;
	endinterface

	typedef enum {RegularReq,BurstReq} BridgeState deriving (Bits,Eq,FShow);

	(*synthesize*)
	module mkAXI4Lite_AXI4_Bridge(Ifc_AXI4Lite_AXI4_Bridge);
		AXI4_Slave_Xactor_IFC #(`PADDR, `Reg_width, 0)  s_xactor <- mkAXI4_Slave_Xactor;
		AXI4_Lite_Master_Xactor_IFC #(`PADDR,`Reg_width,0) m_xactor <- mkAXI4_Lite_Master_Xactor;
		Reg#(BridgeState) rd_state <-mkReg(RegularReq);
		Reg#(BridgeState) wr_state <-mkReg(RegularReq);
		Reg#(Bit#(4)) rd_id<-mkReg(0);
		Reg#(Bit#(4)) wr_id<-mkReg(0);
		Reg#(Bit#(8)) rg_readburst_counter<-mkReg(0);
		Reg#(Bit#(8)) rg_readburst_value<-mkReg(0);
		// This rule will receive the read request from the AXI4 fabric and pass it on to the AXI4Lite fabric.
		// If the request is a burst then they are broken down to individual axi4lite read requests. These
		// are carried out in the next rule. TODO
		rule capture_read_requests_from_Axi4(rd_state==RegularReq);
			let request<-pop_o(s_xactor.o_rd_addr);
		 	let lite_request = AXI4_Lite_Rd_Addr {araddr: request.araddr, arprot: 0, aruser: 0}; // arburst: 00-FIXED 01-INCR 10-WRAP
   	   m_xactor.i_rd_addr.enq(lite_request);	
			rd_id<=request.arid;
			rg_readburst_value<=request.arlen;
			// TODO: add logic to handle bursts.
		endrule
		// This rule will capture the write request from the AXI4 fabric and pass it on to the AXI4Lite fabric.
		// In case of burst requests, they are broken down to individual requests of axi4lite writes. Care
		// needs to be taken when writes are of different sizes in settin the write-strobe correctly.
		rule capture_write_requests_from_Axi4(wr_state==RegularReq);
			let wr_addr_req  <- pop_o (s_xactor.o_wr_addr);
	      let wr_data_req  <- pop_o (s_xactor.o_wr_data);
			let aw = AXI4_Lite_Wr_Addr {awaddr: wr_addr_req.awaddr, awprot:0, awuser:0}; // arburst: 00-FIXED 01-INCR 10-WRAP
			let w  = AXI4_Lite_Wr_Data {wdata:  wr_data_req.wdata, wstrb: wr_data_req.wstrb};
			m_xactor.i_wr_addr.enq(aw);
			m_xactor.i_wr_data.enq(w);
			wr_id<=wr_addr_req.awid;
			// TODO add logic to handle bursts.
		endrule

		// This rule forwards the read response from the AXI4Lite to the AXI4 fabric.
		rule capture_read_responses;
			let response <- pop_o (m_xactor.o_rd_data);
			AXI4_Resp rresp= case(response.rresp)
				AXI4_LITE_OKAY  : AXI4_OKAY;
				AXI4_LITE_EXOKAY: AXI4_EXOKAY;
				AXI4_LITE_SLVERR: AXI4_SLVERR;
				AXI4_LITE_DECERR: AXI4_DECERR;
				default: AXI4_SLVERR; endcase;
			AXI4_Rd_Data#(`Reg_width,0) r = AXI4_Rd_Data {rresp: rresp, rdata: response.rdata ,rlast:rg_readburst_counter==rg_readburst_value, ruser: 0, rid:rd_id};
			if(rg_readburst_counter==rg_readburst_value)
				rg_readburst_counter<=0;
			else
				rg_readburst_counter<=rg_readburst_counter+1;
			s_xactor.i_rd_data.enq(r);
		endrule
		rule capture_write_responses;
			let response<-pop_o(m_xactor.o_wr_resp);
			AXI4_Resp bresp= case(response.bresp)
				AXI4_LITE_OKAY  : AXI4_OKAY;
				AXI4_LITE_EXOKAY: AXI4_EXOKAY;
				AXI4_LITE_SLVERR: AXI4_SLVERR;
				AXI4_LITE_DECERR: AXI4_DECERR;
				default: AXI4_SLVERR; endcase;
			let b = AXI4_Wr_Resp {bresp: bresp, buser:0, bid:wr_id};
			s_xactor.i_wr_resp.enq(b);
		endrule

	endmodule
endpackage
