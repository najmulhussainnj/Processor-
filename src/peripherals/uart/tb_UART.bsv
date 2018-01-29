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
/*-
 * Copyright (c) 2013 Simon W. Moore
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory under DARPA/AFRL contract FA8750-10-C-0237
 * ("CTSRD"), as part of the DARPA CRASH research programme.
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */

/******************************************************************************
 * Unit Test UART16550
 * ===================
 * Simon Moore, July 2013
 ******************************************************************************/

package tb_UART;

import FIFO::*;
import FIFOF::*;
import GetPut::*;
import Semi_FIFOF        :: *;
import AXI4_Types   :: *;
import AXI4_Fabric  :: *;
import Connectable :: *;
import RegFile::*;
import Clocks :: * ;
import StmtFSM::*;
import Uart16550::*;

/*
typedef enum { MemRead, MemWrite} MemAccess deriving(Bits,Eq);
 typedef Maybe#(AXI4_Rd_Data#(32, 0)) ReturnedData;
 typedef Maybe#(AXI4_Wr_Resp#(0)) ReturnedData1;
*/

(* mutually_exclusive = "get_read_response, get_write_response" *)
module mktb_UART(Empty);

Uart16550_AXI4_Ifc dut <- mkUart16550;
AXI4_Master_Xactor_IFC #(32,32,0) m_xactor <- mkAXI4_Master_Xactor();

mkConnection (m_xactor.axi_side, dut.slave_axi_uart);


Reg#(Bool) state <- mkReg(False);
RegFile#(Bit#(10),Bit#(136)) input_instructions <-mkRegFileFullLoad("/home/gopi/UART/beri-master/cherilibs/trunk/peripherals/uart16550/uart16550_axi_1/testcases/trial.hex");
Reg#(Bit#(10)) index <- mkReg(0);
Reg#(bit)  last_stx <- mkReg(0);		


  
rule configure_control_register(!state);
let x = input_instructions.sub(index);

if(x[135:132]==2)
$finish(0);
	else if(x[135:132]!=1)	
	begin
		if(x[131]==0)
		begin// read		
		   $display($time,"\tTB: Sending Read Request Address: %h",x[68:64]);
		   let read_request = AXI4_Rd_Addr {araddr: x[95:64], arprot: 0, aruser: 0, arsize: 0, arlen:0 , arburst: 'b01, arlock: ?,arcache: ?, arqos: ?, arregion: ?, arid: ?}; // arburst: 00-FIXED 01-INCR 10-WRAP
  		   m_xactor.i_rd_addr.enq(read_request);	
		   state<=True;
		   $display ("INDEX INDEX INDEX %d",index );
		   index<=index+1;
		end
       		else 
		begin // write
		   $display($time,"\tTB: Sending WRITE Request");
	    	   let aw = AXI4_Wr_Addr {awaddr: x[95:64], awprot:0, awuser:0, awlen: 0, awsize: 0, awburst: 'b01, awlock: ?,awcache: ?, awqos: ?, awregion: ?, awid: ?}; 
  	  	   let w  = AXI4_Wr_Data {wdata: x[31:0], wstrb: 0,wid: ?, wlast:True};
      		   m_xactor.i_wr_addr.enq(aw);
   	    	   m_xactor.i_wr_data.enq(w);
		   state<=True;
		   $display ("INDEX INDEX INDEX %d",index );
		   index<=index+1;
		end
	end
/*		else
		begin
		        if(dut.irq==1)
			begin
			  $display($time,"\tReseting the UART");
			  //myrst1.assertReset();
			   index<=index+1;
			end
		end */
endrule

rule get_read_response(state);
let response <- pop_o (m_xactor.o_rd_data);
$write(fshow(response));	
$display($time,"\tTB: Received read response: %h",response.rdata);
state<=False;
endrule

rule get_write_response(state);
let response <- pop_o (m_xactor.o_wr_resp);
$write(fshow(response));	
$display($time,"\tTB: Received write response");
state<=False;
endrule


rule loop_back_tx_rx;
    bit stx = dut.coe_rs232.modem_output_stx;
    bit srx = stx;
    bit cts = 0;
    bit dsr = 0;
    bit ri  = 0;
    bit dcd = 0;
    dut.coe_rs232.modem_input(srx, cts, dsr, ri, dcd);
    last_stx <= stx;
    if(stx!=last_stx)
    $display("%05t: UnitTest: stx changed to %1d", $time, stx);
endrule	
endmodule	
endpackage

