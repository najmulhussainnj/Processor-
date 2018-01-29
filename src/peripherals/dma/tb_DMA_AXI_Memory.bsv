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
//TODO
//1. Remove all userid and corresponding registers i.e. rg_memory_user and rg_peripheral_user.
package tb_DMA_AXI_Memory;
	import DMA::*;
	import Semi_FIFOF :: *;
	import AXI4_Types :: *;
	import AXI4_Fabric :: *;
	import Connectable :: *;
	import ConfigReg :: *;
	import RegFile::*;
	import Clocks :: * ;
	import DReg ::*;
	`include "defined_parameters.bsv"
	typedef 2 Num_Masters;
	typedef 3 Num_Slaves;

	`define Max_index 14
	typedef enum{Send_req,Get_resp} Mem_state deriving(Bits,Eq);
	//check with table in pdf
	/*
	function Bit#(8) write_strobe_generation(Bit#(3) transfer_size);
		if(transfer_size==0)
			return 'b1;
		else if(transfer_size==1)
			return 'b11;
		else if(transfer_size==2)
			return 'b1111;
		else
			return '1;
	endfunction
	*/

	function Tuple2 #(Bool, Bit#(TLog#(Num_Slaves))) fn_addr_to_slave_num (Bit #(`PADDR) addr);
		if(addr<='h00000100)
			return tuple2(True,0);	//DMA config registers
		else if(addr<='hFFFF)
			return tuple2(True,2);	//Peripherals
		else 
			return tuple2(True,1);	//Memory
	endfunction

	(*synthesize*)
	//(*mutually_exclusive="get_read_response,get_write_response"*)
	module mkTb_DMA_AXI_Memory(Empty);
		/*
		
		Clock curr_clk<-exposeCurrentClock;
		Reset curr_reset<-exposeCurrentReset;
		MakeResetIfc myrst1 <-mkReset(0,False,curr_clk);
		Reset final_reset<-mkResetEither(myrst1.new_rst,curr_reset);

		*/
		DmaC#(7,12) dma <-mkDMA();//check parameter as DmaC-data type of mkDMA input takes two inputs
		AXI4_Master_Xactor_IFC #(`PADDR,`Reg_width,`USERSPACE) proc_xactor <- mkAXI4_Master_Xactor();
		AXI4_Slave_Xactor_IFC #(`PADDR,`Reg_width,`USERSPACE) slave_memory_xactor <- mkAXI4_Slave_Xactor();
		AXI4_Slave_Xactor_IFC #(`PADDR,`Reg_width,`USERSPACE) slave_peripheral_xactor <- mkAXI4_Slave_Xactor();
		
		//master testbench to slave dma connection
		//mkConnection (m_xactor.axi_side, dma.cfg);
		
		//slave testbench to master dma connection
		//mkConnection (dma.mmu,s_xactor.axi_side);
		


		// Fabric
		AXI4_Fabric_IFC #(Num_Masters, Num_Slaves, `PADDR, `Reg_width,`USERSPACE)
		fabric <- mkAXI4_Fabric(fn_addr_to_slave_num);

		// Connect traffic generators to fabric
		mkConnection (proc_xactor.axi_side, fabric.v_from_masters [0]);
		mkConnection (dma.mmu, fabric.v_from_masters [1]);

		// Connect fabric to memory slaves
		mkConnection(fabric.v_to_slaves[0],dma.cfg);
		mkConnection(fabric.v_to_slaves[1],slave_memory_xactor.axi_side);
		mkConnection(fabric.v_to_slaves[2],slave_peripheral_xactor.axi_side);

		Reg#(Bit#(6)) index<-mkConfigReg(0); // states 0..7
		Reg#(Bool) rg_read_flag<-mkConfigReg(False);
		Reg#(Bool) rg_write_flag<-mkConfigReg(False);
		RegFile#(Bit#(6),Bit#(136)) input_instructions <-mkRegFileLoad("trial.hex",0,`Max_index);
		Reg#(Bit#(32)) rg_count <-mkReg(0);
		//Reg#(Maybe#(AXI4_Rd_Data#(`Reg_width, `USERSPACE))) rg_slave_read_response <- mkReg(tagged Invalid);
		
		Reg#(Mem_state) 		rg_memory_state <-mkDReg(Send_req);
		Reg#(Bit#(8)) 			rg_memory_readburst_counter<-mkReg(0);
		Reg#(Bit#(8)) 			rg_memory_readburst_value<-mkReg(0);
		Reg#(Bit#(8)) 			rg_memory_writeburst_counter<-mkReg(0);
		Reg#(Bit#(`USERSPACE)) 	rg_memory_user<-mkReg(0);
		Reg#(Bit#(4)) 			rg_memory_id<-mkReg(0);
		Reg#(Bit#(64)) 			rg_memory_address<-mkReg(0);
		Reg#(Bit#(3)) 			rg_memory_transfer_size<-mkReg(0);
		Reg#(Bit#(`Reg_width)) 	rg_memory_data <-mkReg(0);

		Reg#(Mem_state) 		rg_peripheral_state <-mkDReg(Send_req);
		Reg#(Bit#(8)) 			rg_peripheral_readburst_counter<-mkReg(0);
		Reg#(Bit#(8)) 			rg_peripheral_readburst_value<-mkReg(0);
		Reg#(Bit#(8)) 			rg_peripheral_writeburst_counter<-mkReg(0);
		Reg#(Bit#(`USERSPACE)) 	rg_peripheral_user<-mkReg(0);
		Reg#(Bit#(4)) 			rg_peripheral_id<-mkReg(0);
		Reg#(Bit#(64)) 			rg_peripheral_address<-mkReg(0);
		Reg#(Bit#(3)) 			rg_peripheral_transfer_size<-mkReg(0);
		Reg#(Bit#(`Reg_width)) 	rg_peripheral_data <-mkReg(0);

		rule rl_inc_count;
			rg_count<= rg_count + 1;
			$display("\n");
			$display($time,"\tCount: %d",rg_count);
			if(rg_count==400)
				$finish(0);
		endrule

		rule proc_start_read(!rg_read_flag && index<=`Max_index && input_instructions.sub(truncate(index))[64]==0); //read operation
			let x = input_instructions.sub(truncate(index));
			let addr= x[31:0];
			let wdata= x[63:32];
			let stop= x[65];
			let size= 2'd2;
			$display($time,"\tTBMaster_Processor: Sending Read Request to addr %h",addr);
			let read_request = AXI4_Rd_Addr {araddr: zeroExtend(addr), arprot: 0, aruser: 0, arsize: zeroExtend(size), arlen: 0 , arburst: 1, // arburst: 00-FIXED 01-INCR 10-WRAP
											 arlock: 0, arcache: 0, arqos: 0,
											 arregion: 0, arid: 0 };
			proc_xactor.i_rd_addr.enq(read_request);
			//x[15:0]=fn_decr_cndtr(x[15:0],2,x[50:48]);//CHECK correct this code USE FUNCTION
			rg_read_flag<= True;
		endrule

		rule proc_start_write(!rg_write_flag && index<=`Max_index && input_instructions.sub(truncate(index))[64]==1); //write operation
			let x = input_instructions.sub(truncate(index));
			let addr= x[31:0];
			let wdata= x[63:32];
			let stop= x[65];
			let size= 2'd2;
			$display($time,"\tTBMaster_Processor: Sending Write Request");
			let aw = AXI4_Wr_Addr {	awaddr: zeroExtend(addr), awprot:0, awuser:0, awlen: 0, awsize: zeroExtend(size), awburst: 1, 
									awlock: 0, awcache: 0, awqos: 0,
									awregion: 0, awid: 0 };
			let w  = AXI4_Wr_Data {wdata: zeroExtend(wdata), wstrb: 0, wlast:True, wid: 0};
			proc_xactor.i_wr_addr.enq(aw);
			proc_xactor.i_wr_data.enq(w);
			rg_write_flag<= True;
		endrule

		rule get_read_response1(rg_read_flag);
			let response <- pop_o (proc_xactor.o_rd_data);
			$display($time,"\tTBMaster_Processor: Received read response with data %h",response.rdata);
			index<= index+1;
			rg_read_flag<= False;
		endrule

		rule get_write_response1(rg_write_flag);
			let response <- pop_o (proc_xactor.o_wr_resp);
			$display($time,"\tTBMaster_Processor: Received write response");
			index<= index+1;
			rg_write_flag<= False;
		endrule
	
		///////////////// ******************   SLAVE MEMORY **************//////////////////////
		///////      SLAVE MEMORY READ       ////////
		rule rl_dummy_slave_memory_handle_read_request;
			let ar<- pop_o(slave_memory_xactor.o_rd_addr);
			Bit#(TSub#(mem_size,2)) index_address=(ar.araddr-fromInteger(valueOf(`Base_addr)))[valueOf(`Addr_space)-1:`byte_offset+1];
			rg_memory_address<=ar.araddr;
			rg_memory_transfer_size<=ar.arsize;
			rg_memory_readburst_value<=ar.arlen;
			rg_memory_user<=ar.aruser;
			rg_memory_id<= ar.arid;
			rg_memory_data<=zeroExtend({16'hbabe,rg_count[15:0]}); 
			rg_memory_state<=Get_resp;
			$display($time,"\tTbSlave_Memory: Recieved Read Request for Address: %h for id: %d",ar.araddr, ar.aruser);
		endrule

		rule rl_dummy_slave_memory_handle_read_response(rg_memory_state==Get_resp);
			/*`ifdef RV64
				Bit#(`Reg_width) data0 = {dmemMSB.a.read(),dmemLSB.a.read()};
			`else 
				Bit#(`Reg_width) data0 = dmemLSB.a.read();
			`endif*/
			Bit#(`Reg_width) data0= rg_memory_data;
			AXI4_Rd_Data#(`Reg_width,`USERSPACE) r = AXI4_Rd_Data {	rresp: AXI4_OKAY, rdata: data0 ,
																	rlast: rg_memory_readburst_counter==rg_memory_readburst_value,
																	ruser: rg_memory_user, rid: rg_memory_id};
			if(rg_memory_transfer_size==2)begin // 32 bit
				if(rg_memory_address[`byte_offset:0]==0)
					r.rdata={data0[31:0],data0[31:0]};
				else
					r.rdata={data0[63:32],data0[63:32]};
			end
			else if (rg_memory_transfer_size=='d1)begin // half_word
				if(rg_memory_address[`byte_offset:0] ==0)
					r.rdata = {data0[15:0],data0[15:0],data0[15:0],data0[15:0]};
				else if(rg_memory_address[`byte_offset:0] ==2)
					r.rdata = {data0[31:16],data0[31:16],data0[31:16],data0[31:16]};
				`ifdef RV64
					else if(rg_memory_address[`byte_offset:0] ==4)
						r.rdata = {data0[47:32],data0[47:32],data0[47:32],data0[47:32]};
					else if(rg_memory_address[`byte_offset:0] ==6)
						r.rdata = {data0[63:48],data0[63:48],data0[63:48],data0[63:48]};
				`endif
			end
			else if (rg_memory_transfer_size=='d0) begin// one byte
				if(rg_memory_address[`byte_offset:0] ==0)
					r.rdata = {data0[7:0],data0[7:0],data0[7:0],data0[7:0],data0[7:0],data0[7:0],data0[7:0],data0[7:0]};
				else if(rg_memory_address[`byte_offset:0] ==1)
					r.rdata = {data0[15:8],data0[15:8],data0[15:8],data0[15:8],data0[15:8],data0[15:8],data0[15:8],data0[15:8]};
				else if(rg_memory_address[`byte_offset:0] ==2)
					r.rdata = {data0[23:16],data0[23:16],data0[23:16],data0[23:16],data0[23:16],data0[23:16],data0[23:16],data0[23:16]};
				else if(rg_memory_address[`byte_offset:0] ==3)
					r.rdata = {data0[31:24],data0[31:24],data0[31:24],data0[31:24],data0[31:24],data0[31:24],data0[31:24],data0[31:24]};
				`ifdef RV64
				else if(rg_memory_address[`byte_offset:0] ==4)
					r.rdata = {data0[39:32],data0[39:32],data0[39:32],data0[39:32],data0[39:32],data0[39:32],data0[39:32],data0[39:32]};
				else if(rg_memory_address[`byte_offset:0] ==5)
				r.rdata = {data0[47:40],data0[47:40],data0[47:40],data0[47:40],data0[47:40],data0[47:40],data0[47:40],data0[47:40]};
				else if(rg_memory_address[`byte_offset:0] ==6)
				r.rdata = {data0[55:48],data0[55:48],data0[55:48],data0[55:48],data0[55:48],data0[55:48],data0[55:48],data0[55:48]};
				else if(rg_memory_address[`byte_offset:0] ==7)
					r.rdata = {data0[63:56],data0[63:56],data0[63:56],data0[63:56],data0[63:56],data0[63:56],data0[63:56],data0[63:56]};
				`endif
			end
			slave_memory_xactor.i_rd_data.enq(r);
			if(rg_memory_readburst_counter==rg_memory_readburst_value)
				rg_memory_readburst_counter<=0;
			else
				rg_memory_readburst_counter<=rg_memory_readburst_counter+1;
			$display($time,"\tTBSlave: Responding Read Request with Data: %8h BurstCounter: %d BurstValue: %d for id: %d",r.rdata,rg_memory_readburst_counter,rg_memory_readburst_value,rg_memory_user);
		endrule

		///////      SLAVE MEMORY WRITE       ////////
		rule rl_wr_respond_memory;
			// Get the wr request
			let aw <- pop_o (slave_memory_xactor.o_wr_addr);
			let w  <- pop_o (slave_memory_xactor.o_wr_data);
			//Bit#(TSub#(mem_size,2)) index_address=(aw.awaddr-fromInteger(valueOf(base_address)))[valueOf(mem_size)-1:`byte_offset+1];
			//dmemLSB.b.put(w.wstrb[3:0],index_address,truncate(w.wdata));
			//`ifdef RV64 dmmMSB.b.put(w.wstrb[7:4],index_address,truncateLSB(w.wdata)); `endif
			let b = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: aw.awuser, bid:aw.awid};
			if(rg_memory_writeburst_counter==aw.awlen)begin
				rg_memory_writeburst_counter<=0;
			slave_memory_xactor.i_wr_resp.enq (b);
			end
			else
				rg_memory_writeburst_counter<=rg_memory_writeburst_counter+1;
			$display($time,"\tTBSlave_Memory: Recieved Write Request for Address: %h data: %h strb: %b awlen: %d rg_memory_writeburst_counter: %d",aw.awaddr,w.wdata,w.wstrb,aw.awlen,rg_memory_writeburst_counter);
		endrule




		///////////////// ******************   SLAVE PERIPHERAL **************//////////////////////
		///////      SLAVE PERIPHERAL READ       ////////
		rule rl_dummy_slave_peripheral_handle_read_request;
			//let ar<- pop_o(slave_peripheral_xactor.o_rd_addr);
			let ar<- pop_o(slave_peripheral_xactor.o_rd_addr);
			Bit#(TSub#(mem_size,2)) index_address=(ar.araddr-fromInteger(valueOf(`Base_addr)))[valueOf(`Addr_space)-1:`byte_offset+1];
			rg_peripheral_address<=ar.araddr;
			rg_peripheral_transfer_size<=ar.arsize;
			rg_peripheral_readburst_value<=ar.arlen;
			rg_peripheral_user<=ar.aruser;
			rg_peripheral_id<= ar.arid;
			rg_peripheral_data<=zeroExtend({16'hbabe,rg_count[15:0]}); 
			rg_peripheral_state<=Get_resp;
			$display($time,"\tTbSlave_peripheral: Recieved Read Request for Address: %h for id: %d",ar.araddr, ar.aruser);
		endrule

		rule rl_dummy_slave_peripheral_handle_read_response(rg_peripheral_state==Get_resp);
			/*`ifdef RV64
				Bit#(`Reg_width) data0 = {dmemMSB.a.read(),dmemLSB.a.read()};
			`else 
				Bit#(`Reg_width) data0 = dmemLSB.a.read();
			`endif*/
			Bit#(`Reg_width) data0= rg_peripheral_data;
			AXI4_Rd_Data#(`Reg_width,`USERSPACE) r = AXI4_Rd_Data {	rresp: AXI4_OKAY, rdata: data0 ,
																	rlast: rg_peripheral_readburst_counter==rg_peripheral_readburst_value,
																	ruser: rg_peripheral_user, rid: rg_peripheral_id};
			if(rg_peripheral_transfer_size==2)begin // 32 bit
				if(rg_peripheral_address[`byte_offset:0]==0)
					r.rdata={data0[31:0],data0[31:0]};
				else
					r.rdata={data0[63:32],data0[63:32]};
			end
			else if (rg_peripheral_transfer_size=='d1)begin // half_word
				if(rg_peripheral_address[`byte_offset:0] ==0)
					r.rdata = {data0[15:0],data0[15:0],data0[15:0],data0[15:0]};
				else if(rg_peripheral_address[`byte_offset:0] ==2)
					r.rdata = {data0[31:16],data0[31:16],data0[31:16],data0[31:16]};
				`ifdef RV64
					else if(rg_peripheral_address[`byte_offset:0] ==4)
						r.rdata = {data0[47:32],data0[47:32],data0[47:32],data0[47:32]};
					else if(rg_peripheral_address[`byte_offset:0] ==6)
						r.rdata = {data0[63:48],data0[63:48],data0[63:48],data0[63:48]};
				`endif
			end
			else if (rg_peripheral_transfer_size=='d0) begin// one byte
				if(rg_peripheral_address[`byte_offset:0] ==0)
					r.rdata = {data0[7:0],data0[7:0],data0[7:0],data0[7:0],data0[7:0],data0[7:0],data0[7:0],data0[7:0]};
				else if(rg_peripheral_address[`byte_offset:0] ==1)
					r.rdata = {data0[15:8],data0[15:8],data0[15:8],data0[15:8],data0[15:8],data0[15:8],data0[15:8],data0[15:8]};
				else if(rg_peripheral_address[`byte_offset:0] ==2)
					r.rdata = {data0[23:16],data0[23:16],data0[23:16],data0[23:16],data0[23:16],data0[23:16],data0[23:16],data0[23:16]};
				else if(rg_peripheral_address[`byte_offset:0] ==3)
					r.rdata = {data0[31:24],data0[31:24],data0[31:24],data0[31:24],data0[31:24],data0[31:24],data0[31:24],data0[31:24]};
				`ifdef RV64
				else if(rg_peripheral_address[`byte_offset:0] ==4)
					r.rdata = {data0[39:32],data0[39:32],data0[39:32],data0[39:32],data0[39:32],data0[39:32],data0[39:32],data0[39:32]};
				else if(rg_peripheral_address[`byte_offset:0] ==5)
				r.rdata = {data0[47:40],data0[47:40],data0[47:40],data0[47:40],data0[47:40],data0[47:40],data0[47:40],data0[47:40]};
				else if(rg_peripheral_address[`byte_offset:0] ==6)
				r.rdata = {data0[55:48],data0[55:48],data0[55:48],data0[55:48],data0[55:48],data0[55:48],data0[55:48],data0[55:48]};
				else if(rg_peripheral_address[`byte_offset:0] ==7)
					r.rdata = {data0[63:56],data0[63:56],data0[63:56],data0[63:56],data0[63:56],data0[63:56],data0[63:56],data0[63:56]};
				`endif
			end
			slave_peripheral_xactor.i_rd_data.enq(r);
			if(rg_peripheral_readburst_counter==rg_peripheral_readburst_value)
				rg_peripheral_readburst_counter<=0;
			else
				rg_peripheral_readburst_counter<=rg_peripheral_readburst_counter+1;
			$display($time,"\tTBSlave_Peripheral: Responding Read Request with Data: %8h BurstCounter: %d BurstValue: %d for id: %d",r.rdata,rg_peripheral_readburst_counter,rg_peripheral_readburst_value,rg_peripheral_user);
		endrule


		///////      SLAVE PERIPHERAL WRITE       ////////
		rule rl_wr_respond_peripheral;
			// Get the wr request
			let aw <- pop_o (slave_peripheral_xactor.o_wr_addr);
			let w  <- pop_o (slave_peripheral_xactor.o_wr_data);
			//Bit#(TSub#(mem_size,2)) index_address=(aw.awaddr-fromInteger(valueOf(base_address)))[valueOf(mem_size)-1:`byte_offset+1];
			//dmemLSB.b.put(w.wstrb[3:0],index_address,truncate(w.wdata));
			//`ifdef RV64 dmmMSB.b.put(w.wstrb[7:4],index_address,truncateLSB(w.wdata)); `endif
			let b = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: aw.awuser, bid:aw.awid};
			if(rg_peripheral_writeburst_counter==aw.awlen)begin
				rg_peripheral_writeburst_counter<=0;
			slave_peripheral_xactor.i_wr_resp.enq (b);
			end
			else
				rg_peripheral_writeburst_counter<=rg_peripheral_writeburst_counter+1;
			$display($time,"\tTBSlave_Peripheral: Recieved Write Request for Address: %h data: %h strb: %b awlen: %d rg_peripheral_writeburst_counter: %d",aw.awaddr,w.wdata,w.wstrb,aw.awlen,rg_peripheral_writeburst_counter);
		endrule

		/////////////////////////        SEND INTERRUPT FROM PERIPHERAL        //////////////////////////////
		rule rl_send_interrupt;
			dma.interrupt_from_peripherals('hA01);
		endrule
		
	endmodule
endpackage
