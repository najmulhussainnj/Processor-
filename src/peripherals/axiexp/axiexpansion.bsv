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
package axiexpansion;
	/*=== Package imports === */
	import FIFO::*;
	import FIFOF::*;
	import SpecialFIFOs::*;
	import BUtils::*;
	import Connectable::*;
	import GetPut::*;
	/*==== Project imports === */
	import defined_types::*;
	`include "defined_parameters.bsv"
	import Semi_FIFOF        :: *;
	import AXI4_Types   :: *;
	import AXI4_Fabric  :: *;
	/*======================== */

	/*=== Type of info to be capture by slave=== */
	typedef struct{
		Bit#(`Reg_width) data;
		Bit#(`PADDR) address;
		Bit#(2) burstmode; // 52:53
		Bit#(3) size;		// 49:51
		Bit#(8) burstlen; // 41:48
		Bit#(8) wrstrb; //33:40
		Bit#(1) wlast; //32
	} SlaveReq deriving (Bits,Eq,FShow);
	/*=============================================== */
	typedef enum {SendReadAddress, SendWriteAddress, SendWriteData, SendWriteResponse, ReceiveReadData, Idle} State deriving (Bits,Eq,FShow);

	interface Ifc_AxiExpansion;
		interface AXI4_Slave_IFC#(`PADDR,`Reg_width,`USERSPACE) axi_slave;
		// 1-bit indicating slave response or master request , 2 bits for slverror, 64-info signals, 
	//	method ActionValue#(Bit#(67)) slave_out;
		interface Get#(Bit#(67)) slave_out;
		// 1-bit indicating slave response or master request , 2 bits for slverror, 64-info signals, 
		interface Put#(Bit#(67)) slave_in;
		//method ActionValue slave_in(Bit#(67) datain);
	endinterface

	(*synthesize*)
	(*preempts="receive_from_AXIWrite_request,receive_from_AXIRead_request"*)
	module mkAxiExpansion(Ifc_AxiExpansion);
		AXI4_Slave_Xactor_IFC #(`PADDR, `Reg_width, `USERSPACE)  slave_xactor <- mkAXI4_Slave_Xactor;
		FIFO#(SlaveReq) capture_slave_req <-mkLFIFO();
		FIFO#(Bit#(67)) send_slave_req <-mkLFIFO();
		Wire#(Maybe#(Bit#(67))) wr_receive_slavedata<-mkDWire(tagged Invalid);
		FIFO#(Bit#(67)) ff_receive_slavedata<-mkLFIFO;

		Reg#(Bit#(8)) rg_readburst_counter<-mkReg(0);
		Reg#(Bit#(8)) rg_writeburst_counter<-mkReg(0);
		Reg#(Bit#(8)) rg_readburst_value<-mkReg(0);
		Reg#(Bit#(4)) rg_id<-mkReg(0);

		Reg#(Bit#(32)) rg_prescale <-mkReg(10);
		Reg#(Bit#(32)) rg_count <-mkReg(0);

		Reg#(State) rg_state<-mkReg(Idle);

		rule counter;
			if(rg_count==rg_prescale)
				rg_count<=0;
			else
				rg_count<=rg_count+1;
		endrule
		/*============================= Write channel logic =========== */	
		rule receive_from_AXIWrite_request(rg_state==Idle);
			$display($time,"\tAXIEXP: Got Write Request");
			let aw <- pop_o (slave_xactor.o_wr_addr);
			let w  <- pop_o (slave_xactor.o_wr_data);
			let b = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser:?, bid:aw.awid};
			if(aw.awaddr=='hFFFFFFFF)begin
				rg_prescale<=truncate(w.wdata);
				slave_xactor.i_wr_resp.enq (b);
				rg_state<=Idle;
			end
			else begin
				capture_slave_req.enq(SlaveReq{
					data:w.wdata,
					address:aw.awaddr,
					burstmode:aw.awburst,
					size:aw.awsize,
					burstlen:aw.awlen,
					wrstrb:w.wstrb,
					wlast:pack(w.wlast)});	
				rg_writeburst_counter<=aw.awlen;
				rg_state<=SendWriteAddress;
				rg_id<=aw.awid;
			end
		endrule
		rule send_slave_writerequest(rg_state==SendWriteAddress && rg_count==0);
			let x=capture_slave_req.first;
			send_slave_req.enq({1'b1,2'd0,10'd0,x.burstmode,x.size,x.burstlen,x.wrstrb,x.wlast,x.address[31:0]});
			rg_state<=SendWriteData;
		endrule
		rule pop_burst_write_request_from_AXI(rg_state==SendWriteData && rg_count==0);
			let aw <- pop_o (slave_xactor.o_wr_addr);
			let w  <- pop_o (slave_xactor.o_wr_data);
			capture_slave_req.enq(SlaveReq{
				data:w.wdata,
				address:aw.awaddr,
				burstmode:aw.awburst,
				size:aw.awsize,
				burstlen:aw.awlen,
				wrstrb:w.wstrb,
				wlast:pack(w.wlast)});	
		endrule
		rule send_slave_writedata(rg_state==SendWriteData && rg_count==0);
			let x=capture_slave_req.first;
			send_slave_req.enq({1'b1,2'd1,x.data});
			if(x.wlast==1)begin
				rg_state<=SendWriteResponse;
			end
			capture_slave_req.deq;
		endrule
		rule send_slave_writeresponse(rg_state==SendWriteResponse && ff_receive_slavedata.first[66]==0);
			let b = AXI4_Wr_Resp {bresp: unpack(ff_receive_slavedata.first[65:64]), buser:?, bid:rg_id};
			ff_receive_slavedata.deq;
			slave_xactor.i_wr_resp.enq (b);
			rg_state<=Idle;
		endrule
		/*============================================================ */

		/*============================= Read channel logic =========== */	
		rule receive_from_AXIRead_request(rg_state==Idle);
			$display($time,"\tAXIEXP: Got Read Request");
			let ar<- pop_o(slave_xactor.o_rd_addr);
			AXI4_Rd_Data#(`Reg_width,`USERSPACE) r = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: duplicate(rg_prescale) ,rlast:True, ruser: 0, rid:ar.arid};
			if(ar.araddr=='hFFFFFFFF)begin
				slave_xactor.i_rd_data.enq(r);
				rg_state<=Idle;
			end
			else begin
				capture_slave_req.enq(SlaveReq{
					data:0,
					address:ar.araddr,
					burstmode:ar.arburst,
					size:ar.arsize,
					burstlen:ar.arlen,
					wrstrb:0,
					wlast:0});	
				rg_readburst_value<=ar.arlen;
				rg_state<=SendReadAddress;
				rg_id<=ar.arid;
			end
		endrule
		rule send_slave_readrequest(rg_state==SendReadAddress && rg_count==0);
			let x=capture_slave_req.first;
			send_slave_req.enq({1'b1,2'd2,10'd0,x.burstmode,x.size,x.burstlen,x.wrstrb,x.wlast,x.address[31:0]});
			capture_slave_req.deq;
			rg_state<=ReceiveReadData;
		endrule
		rule send_axiread_slave_response(rg_state==ReceiveReadData && ff_receive_slavedata.first[66]==0);
			AXI4_Rd_Data#(`Reg_width,`USERSPACE) r = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: ff_receive_slavedata.first[63:0] ,rlast:rg_readburst_counter==rg_readburst_value, ruser: 0, rid:rg_id};
			ff_receive_slavedata.deq;
			if(ff_receive_slavedata.first[65:64]!=0)
				r.rresp=unpack(ff_receive_slavedata.first[65:64]);
			slave_xactor.i_rd_data.enq(r);
			if(rg_readburst_counter==rg_readburst_value)begin
				rg_readburst_counter<=0;
			end
			else begin
				rg_readburst_counter<=rg_readburst_counter+1;
			end
			rg_state<=Idle;
		endrule
		/*============================================================ */

		interface axi_slave= slave_xactor.axi_side;
		interface slave_out= interface Get
			method ActionValue#(Bit#(67)) get;
				send_slave_req.deq;
				return send_slave_req.first;
			endmethod
		endinterface;
		interface slave_in=interface Put
			method Action put (Bit#(67) datain) if(rg_count==0); 
				ff_receive_slavedata.enq(datain);
			endmethod
		endinterface;
	endmodule
endpackage
