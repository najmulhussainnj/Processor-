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

package clint;
	/*=== library imports === */ 
	import ConfigReg::*;
	import Semi_FIFOF::*;
	import AXI4_Lite_Types::*;
	import BUtils ::*;
	/*======================== */
	/*==== Project imports ====*/
	import defined_types::*;
	`include "defined_parameters.bsv"	
	/*=========================*/

	interface Ifc_clint;
		method Bit#(1) msip_int;
		method Bit#(1) mtip_int;
		method Bit#(`Reg_width) mtime;
		interface AXI4_Lite_Slave_IFC#(`PADDR, `Reg_width,`USERSPACE) axi4_slave;
	endinterface
	
	function Reg#(t) writeSideEffect(Reg#(t) r, Action a);
		return (interface Reg;
         method t _read = r._read;
         method Action _write(t x);
				r._write(x);
            a;
         endmethod
      endinterface);
	endfunction

	(*synthesize*)
	module mkclint(Ifc_clint);

		AXI4_Lite_Slave_Xactor_IFC #(`PADDR, `Reg_width, `USERSPACE)  s_xactor <- mkAXI4_Lite_Slave_Xactor;
		Wire#(Bool) wr_mtimecmp_written<-mkDWire(False);
		Reg#(Bit#(1)) msip <-mkReg(0);
		Reg#(Bit#(1)) mtip <-mkReg(0);
		Reg#(Bit#(64)) rgmtime<-mkReg(0);
		Reg#(Bit#(64)) rgmtimecmp<-mkReg(0);
		Reg#(Bit#(64)) csr_mtimecmp=writeSideEffect(rgmtimecmp,wr_mtimecmp_written._write(True));
		Reg#(Bit#(2)) rg_tick <-mkReg(0);

		rule generate_time_interrupt(!wr_mtimecmp_written);
			mtip<=pack(rgmtime>=rgmtimecmp);
		endrule
		rule clear_interrupt(wr_mtimecmp_written);
			mtip<=0;
		endrule
		rule increment_timer;
			if(rg_tick==0)begin
				rgmtime<=rgmtime+1;
			end
			rg_tick<=rg_tick+1;
		endrule


		rule axi_read_transaction;
			let ar <- pop_o(s_xactor.o_rd_addr);
			let r = AXI4_Lite_Rd_Data {rresp: AXI4_LITE_OKAY, rdata: ?, ruser: 0};
			case (ar.araddr[15:0]) matches
				'h0000:		r.rdata=zeroExtend(msip); // MSIP interrupt bit
				'h4000:		r.rdata=csr_mtimecmp;
				'hbff8:		r.rdata=rgmtime;
				default:	begin	r.rdata=0; r.rresp=AXI4_LITE_SLVERR; end
			endcase
			s_xactor.i_rd_data.enq(r);
		endrule
		
		rule axi_write_transaction;
			let aw <- pop_o(s_xactor.o_wr_addr);
			let w <- pop_o(s_xactor.o_wr_data);
			let r = AXI4_Lite_Wr_Resp {bresp: AXI4_LITE_OKAY, buser: 0 };

			case (aw.awaddr[15:0]) matches
				'h0000:		msip<=w.wdata[0]; // MSIP interrupt bit
				'h4000:		csr_mtimecmp<=w.wdata;
				default:		r.bresp=AXI4_LITE_SLVERR;
			endcase
			s_xactor.i_wr_resp.enq (r);
		endrule

		interface axi4_slave = s_xactor.axi_side;
		method Bit#(1) msip_int=msip;
		method Bit#(1) mtip_int=mtip;
		method Bit#(`Reg_width) mtime = rgmtime;

	endmodule
endpackage
