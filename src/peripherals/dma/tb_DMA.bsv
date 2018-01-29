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
package tb_DMA;
	import DMA::*;
	import Memory_AXI4	::*;
	import Semi_FIFOF :: *;
	import AXI4_Types :: *;
	import AXI4_Fabric :: *;
	import Connectable :: *;
	import ConfigReg :: *;
	import RegFile::*;
	import Clocks :: * ;
	import DReg ::*;
	import defined_types::*;
	`include "defined_parameters.bsv"


	`define Num_DMA_Channels 7
	`define Num_Peripherals 12
	`define Num_Masters 2
	`define Num_Slaves TAdd#(`Num_Peripherals,2)

	`define DMA_cfg_addr_start	 	64'h00000000
	`define DMA_cfg_addr_end 		64'h000000FF
	`define Peripheral1_addr_start 	64'h00001000
	`define Peripheral1_addr_end 	64'h00001FFF
	`define Peripheral2_addr_start 	64'h00002000
	`define Peripheral2_addr_end 	64'h00002FFF
	`define Peripheral3_addr_start 	64'h00003000
	`define Peripheral3_addr_end 	64'h00003FFF
	`define MainMemory_addr_start 	64'h80000000
	`define MainMemory_addr_end 	64'hFFFFFFFF

	typedef enum{Send_req,Get_resp} Mem_state deriving(Bits,Eq);

	/*function Tuple2#(t , Bit#(`Addr_space)) fn_slave_addr_range(Integer i) provisos(Literal#(t));
		case (i) matches
			0: return tuple2(`DMA_cfg_addr_start, `DMA_cfg_addr_end);
			1: return tuple2(`MainMemory_addr_start, `MainMemory_addr_end);
			2: return tuple2(`Peripheral1_addr_start, `Peripheral1_addr_end);
			default: return tuple2(0,0);
		endcase
	endfunction*/

	function Tuple2 #(Bool, Bit#(TLog#(`Num_Slaves))) fn_addr_to_slave_num (Bit #(`PADDR) addr);
		if(addr>=`MainMemory_addr_start && addr<=`MainMemory_addr_end)
			return tuple2(True,1);	//Memory
		else if(addr>=`Peripheral1_addr_start && addr<=`Peripheral1_addr_end)
			return tuple2(True,2);	//Peripherals
		else if(addr>=`Peripheral2_addr_start && addr<=`Peripheral2_addr_end)
			return tuple2(True,3);	//Peripherals
		else if(addr>=`Peripheral3_addr_start && addr<=`Peripheral3_addr_end)
			return tuple2(True,4);	//Peripherals
		else if(addr>=`DMA_cfg_addr_start && addr<= `DMA_cfg_addr_end)
			return tuple2(True,0);	//DMA config registers
		else
			return tuple2(False,0);
	endfunction

	(*synthesize*)
	//(*mutually_exclusive= "get_read_response,get_write_response"*)
	module mkTb_DMA(Empty);
		DmaC#(`Num_DMA_Channels, `Num_Peripherals) dma <-mkDMA();//check parameter as DmaC-data type of mkDMA input takes two inputs
		AXI4_Master_Xactor_IFC #(`PADDR,`Reg_width,`USERSPACE) processor <- mkAXI4_Master_Xactor();
		Memory_IFC#(`MainMemory_addr_start,`Addr_space) main_memory <- mkMemory("MainMemory_MSB","MainMemory_LSB","MainMemory");

		Memory_IFC#(`Peripheral1_addr_start,`Addr_space) peripheral1 <- mkMemory("Peripheral1_MSB", "Peripheral1_LSB", "Peripheral[1]");
		Memory_IFC#(`Peripheral2_addr_start,`Addr_space) peripheral2 <- mkMemory("Peripheral2_MSB", "Peripheral2_LSB", "Peripheral[2]");
		Memory_IFC#(`Peripheral3_addr_start,`Addr_space) peripheral3 <- mkMemory("Peripheral3_MSB", "Peripheral3_LSB", "Peripheral[2]");
		/*for(Integer i=0; i<`Num_Peripherals; i=i+1) begin
			let lv_base_addr= fn_slave_addr_range(i).tpl_1;
			peripheral[i] <- mkMemory("Peripheral" +integerToString(i)+ "_MSB", "Peripheral" +integerToString(i)+ "_LSB", "Peripheral[" +integerToString(i)+ "]");
		end*/

		// Fabric
		AXI4_Fabric_IFC #(`Num_Masters, `Num_Slaves, `PADDR, `Reg_width,`USERSPACE)
		fabric <- mkAXI4_Fabric(fn_addr_to_slave_num());

		//Connect the masters to fabric
		mkConnection (processor.axi_side, fabric.v_from_masters [0]);	//MASTER0: Processor
		mkConnection (dma.mmu, fabric.v_from_masters [1]);				//MASTER1: DMA's MMU

		mkConnection(fabric.v_to_slaves[0],dma.cfg);					//SLAVE1: Main Memory
		mkConnection(fabric.v_to_slaves[1],main_memory.axi_slave);		//SLAVE1: Main Memory

		mkConnection(fabric.v_to_slaves[2],peripheral1.axi_slave);		//SLAVE2: Peripheral1
		mkConnection(fabric.v_to_slaves[3],peripheral2.axi_slave);		//SLAVE2: Peripheral1
		mkConnection(fabric.v_to_slaves[4],peripheral3.axi_slave);		//SLAVE2: Peripheral1
		/*for(Integer i=0; i<`Num_Peripherals; i=i+1)
			mkConnection(fabric.v_to_slaves[i+2],peripheral[i].axi_slave);		//SLAVE2 onwards: Various Peripherals*/

		//Registers for reading instructions from Dummy processor
		Reg#(Bit#(6)) index<-mkConfigReg(0); // states 0..7
		Reg#(Bool) rg_read_flag<-mkConfigReg(False);
		Reg#(Bool) rg_write_flag<-mkConfigReg(False);
		RegFile#(Bit#(6),Bit#(136)) input_instructions <-mkRegFileFullLoad("trial.hex");
		Reg#(Bit#(32)) rg_count <-mkReg(0);
		Reg#(Bool) rg_done <-mkReg(False);

		rule rl_inc_count;
			rg_count<= rg_count + 1;
			$display("\n");
			$display($time,"\tCount: %d",rg_count);
			if(rg_count==500)
				$finish(0);
		endrule

		rule proc_start_read(!rg_read_flag && !rg_done && input_instructions.sub(truncate(index))[65:64]==2'b00); //read operation
			let x = input_instructions.sub(truncate(index));
			let addr= x[31:0];
			let wdata= x[63:32];
			let stop= x[65];
			let size= 2'd2;
			$display($time,"\tTBMaster Processor: Sending Read Request to addr %h",addr);
			let read_request = AXI4_Rd_Addr {araddr: zeroExtend(addr), arprot: 0, aruser: 0, arsize: zeroExtend(size), arlen: 0 , arburst: 1, // arburst: 00-FIXED 01-INCR 10-WRAP
											 arlock: 0, arcache: 0, arqos: 0,
											 arregion: 0, arid: 0 };
			processor.i_rd_addr.enq(read_request);
			//x[15:0]=fn_decr_cndtr(x[15:0],2,x[50:48]);//CHECK correct this code USE FUNCTION
			rg_read_flag<= True;
			rg_done<= unpack(stop);
		endrule

		rule proc_start_write(!rg_write_flag && !rg_done && input_instructions.sub(truncate(index))[65:64]==2'b01); //write operation
			let x = input_instructions.sub(truncate(index));
			let addr= x[31:0];
			let wdata= x[63:32];
			let stop= x[65];
			let size= 2'd2;
			$display($time,"\tTBMaster Processor: Sending Write Request");
			let aw = AXI4_Wr_Addr {	awaddr: zeroExtend(addr), awprot:0, awuser:0, awlen: 0, awsize: zeroExtend(size), awburst: 1, 
									awlock: 0, awcache: 0, awqos: 0,
									awregion: 0, awid: 0 };
			Bit#(8) lv_strb=0;
			Bit#(64) lv_wdata=0;
			if(addr[2:0]=='b000) begin
				lv_strb= 'h0F;
				lv_wdata= zeroExtend(wdata);
			end
			else if(addr[2:0]=='b100) begin
				lv_strb= 'hF0;
				lv_wdata= {wdata,32'd0};
			end
			else begin
				$display($time,"ERROR: Write to a wrong address");
			end
			let w  = AXI4_Wr_Data {wdata: lv_wdata, wstrb: lv_strb, wlast:True, wid: 0};	//TODO generate wstrb to be 2'b10 for all regs except cmar and cpar
			processor.i_wr_addr.enq(aw);
			processor.i_wr_data.enq(w);
			rg_write_flag<= True;
			rg_done<= unpack(stop);
			index<= index+1;
		endrule

		rule get_read_response1(rg_read_flag);
			let response <- pop_o (processor.o_rd_data);
			$display($time,"\tTBMaster Processor: Received read response with data %h",response.rdata);
			rg_read_flag<= False;
		endrule

		rule get_write_response1(rg_write_flag);
			let response <- pop_o (processor.o_wr_resp);
			$display($time,"\tTBMaster Processor: Received write response");
			rg_write_flag<= False;
		endrule
		
		/////////////////////////        SEND INTERRUPT FROM PERIPHERAL        //////////////////////////////
		rule rl_send_interrupt;
			dma.interrupt_from_peripherals('d-1);
		endrule


		rule rl_read_interrupt_status(rg_count>20 && rg_count<60);
			let size= 2'd2;
			$display($time,"\tTBMaster Processor: Sending Read Request to addr 0 to read interrupt status...");
			let read_request = AXI4_Rd_Addr {araddr: 0, arprot: 0, aruser: 0, arsize: zeroExtend(size), arlen: 0 , arburst: 1, // arburst: 00-FIXED 01-INCR 10-WRAP
											 arlock: 0, arcache: 0, arqos: 0,
											 arregion: 0, arid: 0 };
			processor.i_rd_addr.enq(read_request);
			//x[15:0]=fn_decr_cndtr(x[15:0],2,x[50:48]);//CHECK correct this code USE FUNCTION
			rg_read_flag<= True;

		endrule
	endmodule
endpackage
