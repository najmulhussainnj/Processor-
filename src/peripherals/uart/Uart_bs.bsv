
/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Description: Bluespec UART with an AXI interface.
*/
package Uart_bs;

	`define Depth 16

	import defined_types::*;
	import AXI4_Types::*;
	import AXI4_Fabric::*;
	import Semi_FIFOF::*;
	`include "defined_parameters.bsv"
	import RS232_modified::*;
	import GetPut::*;

	interface Ifc_Uart_bs;
  		interface AXI4_Slave_IFC#(`PADDR,`Reg_width,`USERSPACE) slave_axi_uart;
		interface RS232 coe_rs232;
	endinterface

	(*synthesize*)
	(*preempts="(rl_handle_axi4_uart_read, rl_handle_axi4_uart_status), rl_handle_rest_axi4_req "*)
	module mkUart_bs(Ifc_Uart_bs);

		Reg#(Bit#(16)) baud_value <-mkReg(`BAUD_RATE);
		UART#(`Depth) uart <-mkUART(8,NONE,STOP_1,baud_value); // charasize,Parity,Stop Bits,BaudDIV
		AXI4_Slave_Xactor_IFC #(`PADDR,`Reg_width,`USERSPACE)  s_xactor <- mkAXI4_Slave_Xactor;
		Reg#(Bit#(4)) rg_status <-mkReg(0);		//This register keeps track of whether some data
												//is pending to be sent out through the UART Tx

		//Address 'h11304 is uart read data
		rule rl_handle_axi4_uart_read(s_xactor.o_rd_addr.notEmpty && s_xactor.o_rd_addr.first.araddr[3:2]=='d1);
			let req <- pop_o (s_xactor.o_rd_addr);
			`ifdef verbose $display($time,"\tReq: RD_ADDR %h", req.araddr); `endif
			Bit#(8) data<-uart.tx.get;
			let lv_resp= AXI4_Rd_Data {rresp:AXI4_OKAY, rdata: zeroExtend(data), rlast: True, ruser: ?, rid: req.arid};

			`ifdef verbose $display($time,"\tResp: RD_RESP %h", req.araddr); `endif
			s_xactor.i_rd_data.enq(lv_resp);
		endrule

		//Address 'b11308 is uart read status
		rule rl_handle_axi4_uart_status(s_xactor.o_rd_addr.notEmpty && s_xactor.o_rd_addr.first.araddr[3:2]=='d2);
			let req <- pop_o (s_xactor.o_rd_addr);
			`ifdef verbose $display($time,"\tReq: RD_ADDR %h", req.araddr); `endif
			let lv_resp= AXI4_Rd_Data {rresp:AXI4_OKAY, rdata: zeroExtend(rg_status), rlast: True, ruser: ?, rid: req.arid};

			`ifdef verbose $display($time,"\tResp: RD_RESP %h Status: %b", req.araddr, rg_status); `endif
			s_xactor.i_rd_data.enq(lv_resp);
		endrule
		
		//Address 'h1130c is uart read status
		rule rl_handle_axi4_uart_baud(s_xactor.o_rd_addr.notEmpty && s_xactor.o_rd_addr.first.araddr[3:2]=='d3);
			let req <- pop_o (s_xactor.o_rd_addr);
			`ifdef verbose $display($time,"\tReq: RD_ADDR %h", req.araddr); `endif
			let lv_resp= AXI4_Rd_Data {rresp:AXI4_OKAY, rdata: zeroExtend(baud_value), rlast: True, ruser: ?, rid: req.arid};

			`ifdef verbose $display($time,"\tResp: RD_RESP %h Status: %b", req.araddr, rg_status); `endif
			s_xactor.i_rd_data.enq(lv_resp);
		endrule

		rule rl_handle_rest_axi4_req(s_xactor.o_rd_addr.notEmpty);

			let req <- pop_o (s_xactor.o_rd_addr);
			let lv_resp= AXI4_Rd_Data {rresp:AXI4_SLVERR, rdata: ?, rlast: True, ruser: ?, rid: req.arid};
			s_xactor.i_rd_data.enq(lv_resp);
		endrule

		//Address 'b0000 is uart write data
		rule rl_handle_axi4_write(s_xactor.o_wr_addr.notEmpty && s_xactor.o_wr_data.notEmpty);
			let wr_addr <- pop_o(s_xactor.o_wr_addr);
			`ifdef verbose $display($time,"\tReq: WR_ADDR %h", wr_addr.awaddr); `endif
			let wr_data <- pop_o(s_xactor.o_wr_data);
			`ifdef verbose $display($time,"\tReq: WR_DATA %h", wr_data.wdata); `endif

			if(wr_addr.awaddr[3:2]==2'd0) begin	//The address is valid for a write request
				uart.rx.put(truncate(wr_data.wdata));
        		let lv_resp = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: ?, bid: wr_addr.awid};
        		s_xactor.i_wr_resp.enq(lv_resp);
			end
			else if(wr_addr.awaddr[3:2]=='d3) begin // change the baud value
				baud_value<=truncate(wr_data.wdata);
        		let lv_resp = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: ?, bid: wr_addr.awid};
        		s_xactor.i_wr_resp.enq(lv_resp);
			end
			else begin
        		let lv_resp = AXI4_Wr_Resp {bresp: AXI4_SLVERR, buser: ?, bid: wr_addr.awid};
        		s_xactor.i_wr_resp.enq(lv_resp);
			end
		endrule

		//The status register is 1 if the transmission FIFO is empty
		(*no_implicit_conditions, fire_when_enabled*)
		rule rl_update_status_reg;
			let lv_status= {pack(uart.receiver_not_empty), pack(uart.receiver_not_full), pack(uart.transmittor_not_empty), pack(uart.transmission_done)};
			rg_status<= lv_status;
			`ifdef verbose
			if(lv_status==0)
				$display($time,"-------UART1 TX Fifo not empty"); 
			`endif
		endrule

		interface slave_axi_uart = s_xactor.axi_side;
		interface coe_rs232= uart.rs232;
	endmodule
endpackage
		
