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
package tb_bsv_wrapper;

import Semi_FIFOF        :: *;
import AXI4_Types   :: *;
import AXI4_Fabric  :: *;
import bsvmksdram_model_wrapper :: *;
import Connectable       :: *;
`include "defined_parameters.bsv"

`define DELAY 10200

   interface Ifc_tb_sdram_in;
      
        method Action iAddr(Bit#(11) addr);
        method Action iBa(Bit#(2) ba);
        method Action iCke(bit cke);
        method Action iClk(bit clk);
        method Action iCs_n(bit cs_n);
        method Action iRas_n(bit ras_n);
        method Action iCas_n(bit cas_n);
        method Action iWe_n(bit we_n);
        method Action iDqm(Bit#(8) dqm);

    endinterface

interface Ifc_tb_bsv_wrapper;
    interface AXI4_Master_IFC#(`PADDR, `Reg_width, `USERSPACE) axi4_sdram;
    interface AXI4_Master_IFC#(`PADDR, `Reg_width, `USERSPACE) axi4_cntrl_reg;
    interface Ifc_tb_sdram_in ifc_tb_sdram_in;
    interface Inout#(Bit#(32)) dq_0 ;
    interface Inout#(Bit#(32)) dq_1;    
endinterface


module mktb_bsv_wrapper(Ifc_tb_bsv_wrapper);

   Reg#(Bit#(9)) rg_burst_count <- mkReg(0);
   Reg#(bit) rg_tb_app_req <- mkReg(0);       
   Reg#(bit) rg_tb_app_wr_n <- mkRegU();
   Reg#(Bit#(64))  rg_tb_app_req_addr <- mkReg(0);
   Reg#(Bit#(8))   rg_tb_app_req_len  <- mkReg(0);
   Reg#(Bit#(64))  rg_tb_wr_data <- mkReg(0);
   Reg#(Bit#(8))   rg_tb_wr_en_n <- mkReg(0);
   Reg#(Bit#(64))  rg_tb_rd_data <- mkReg(0);
   Reg#(Bit#(5))   rg_state_cnt  <- mkReg(0);
   Reg#(Bit#(64))  rg_tb_app_rd_addr <- mkReg(0);
   Reg#(Bit#(8))   rg_tb_app_rd_len  <- mkReg(0);
   Reg#(Bit#(16))  rg_delay_count <- mkReg(0);
   Reg#(bit)   rg_ff <- mkReg(0);
   Reg#(Bit#(2)) rg_bmode <- mkReg(0);

   Reg#(Bit#(64)) rg_tb_cntrl_addr <- mkReg(0);
   Reg#(Bit#(64)) rg_tb_cntrl_data <- mkReg(4);   

   AXI4_Master_Xactor_IFC #(`PADDR,`Reg_width,`USERSPACE) m_xactor_sdram <- mkAXI4_Master_Xactor;
   AXI4_Master_Xactor_IFC #(`PADDR,`Reg_width,`USERSPACE) m_xactor_cntrl_reg <- mkAXI4_Master_Xactor;

   Ifc_sdram_model sdram_model_0 <- mksdram_model_wrapper;
   Ifc_sdram_model sdram_model_1 <- mksdram_model_wrapper;

   function Action burst_write(Bit#(64) address, Bit#(8) bl);
     return
     action  
       rg_tb_app_req        <= 1;       
       rg_tb_app_wr_n       <= 0;
       rg_tb_app_req_addr   <= address;
       rg_tb_app_req_len    <= bl;

//       for(Integer i = 0;i <= bl; i++) begin
       rg_tb_wr_en_n <= 8'hFF;
  //     end
      endaction;
  endfunction

  function Action data_change(bit ff);
      return
      action
       if(ff == 1)
           rg_tb_wr_data <= 64'hDEADBEEFBABECAFE;         
       else
           rg_tb_wr_data <= 64'hBABECAFEDEADBEEF;

           rg_ff <= ~rg_ff;

      endaction;
  endfunction


   function Action burst_read(Bit#(64) address, Bit#(8) bl);
       return
       action 
       rg_tb_app_req        <= 1;
       rg_tb_app_wr_n       <= 1;
       rg_tb_app_rd_addr   <= address;
       rg_tb_app_rd_len    <= bl;
       endaction;

    endfunction

    function Action fn_send_cntrl_reg(Bit#(9) count);
        action
        case(count)
            0: begin
                rg_tb_cntrl_addr <= 64'h00;
                rg_tb_cntrl_data <= 64'h4;
            end
            1: begin
                rg_tb_cntrl_addr <= 64'h08;
                rg_tb_cntrl_data <= 64'h4;
            end
            2: begin
                rg_tb_cntrl_addr <= 64'h10;
                rg_tb_cntrl_data <= 64'h2;
            end
            3: begin
                rg_tb_cntrl_addr <= 64'h18;
                rg_tb_cntrl_data <= 64'h1;
            end
            4: begin
                rg_tb_cntrl_addr <= 64'h20;
                rg_tb_cntrl_data <= 64'h3;
            end
            5: begin
                rg_tb_cntrl_addr <= 64'h28;
                rg_tb_cntrl_data <= 64'h32;
            end
            6: begin
                rg_tb_cntrl_addr <= 64'h30;
                rg_tb_cntrl_data <= 64'h3;
            end
            7: begin
                rg_tb_cntrl_addr <= 64'h38;
                rg_tb_cntrl_data <= 64'h7;
            end
            8: begin
                rg_tb_cntrl_addr <= 64'h40;
                rg_tb_cntrl_data <= 64'h1;
            end
            9: begin
                rg_tb_cntrl_addr <= 64'h48;
                rg_tb_cntrl_data <= 64'h100;
            end
            10: begin
                rg_tb_cntrl_addr <= 64'h50;
                rg_tb_cntrl_data <= 64'h6;
            end
        endcase
    endaction
endfunction

   
    rule rl_delay(rg_ff == 0 && rg_state_cnt == 0);
        if(rg_burst_count == 300)
            rg_ff <= 1;            
        else
            rg_burst_count <= rg_burst_count + 1;

        if(rg_burst_count <= 10) begin 
            if(rg_burst_count == 0) begin
                let aw = AXI4_Wr_Addr {awaddr: rg_tb_cntrl_addr, awprot:0, awuser:0, awlen: 10, awsize: 3, awburst: 'b01}; 
                m_xactor_cntrl_reg.i_wr_addr.enq(aw);
            end

            let w = AXI4_Wr_Data {wdata:  rg_tb_cntrl_data, wstrb: 8'h11, wlast: (rg_burst_count == 10)};
            m_xactor_cntrl_reg.i_wr_data.enq(w);
            fn_send_cntrl_reg(rg_burst_count + 1);
            $display("%d: Sending Control register info",$stime());
        end
    endrule
    
    rule rl_response_from_cntrl_reg;
       let resp <- pop_o(m_xactor_cntrl_reg.o_wr_resp);
       $display("%d: Response from control reg bus %d",$stime(),(resp.bresp == AXI4_OKAY)? 1:0);
    endrule



    rule rl_write_request(rg_state_cnt == 0 && rg_ff == 1);
        burst_write({32'b0,{8'd0,11'h0,2'b00,8'd0,3'b0}}, 8'd255);
        rg_state_cnt <= rg_state_cnt + 1;
        rg_burst_count <= 0;
    endrule

    rule rl_read_request_gen(rg_state_cnt == 15);
        burst_read({32'b0,{8'd0,11'd0,2'b00,8'hA,3'b000}}, 8'd7);
        rg_state_cnt <= rg_state_cnt + 1;
        rg_bmode <= 'b01;
        rg_burst_count <= 0;
    endrule

   rule rl_iapp_req_ack_connection(rg_state_cnt == 2 || rg_state_cnt == 5 || rg_state_cnt == 8 || rg_state_cnt == 11 || rg_state_cnt == 14 || rg_state_cnt == 27);
       let resp <- pop_o(m_xactor_sdram.o_wr_resp);
       $display("%d: Response from write bus %d",$stime(),(resp.bresp == AXI4_OKAY)? 1:0);
       rg_state_cnt <= rg_state_cnt + 1;
//       $finish();
//       tb_core.iapp_req_ack((resp.bresp == AXI4_OKAY)? 1:0 );
   endrule

    rule rl_oapp_wr_data(rg_state_cnt == 1 || rg_state_cnt == 4 || rg_state_cnt == 7 || rg_state_cnt == 10 || rg_state_cnt == 13 || rg_state_cnt == 26);
        if(rg_tb_app_req_len == 0) begin
           let aw = AXI4_Wr_Addr {awaddr: rg_tb_app_req_addr, awprot:0, awuser:0, awlen: extend(rg_tb_app_req_len), awsize: 2, awburst: 'b01}; 
           let w = AXI4_Wr_Data {wdata:  64'hDEADBEEFBABECAFE, wstrb: rg_tb_wr_en_n, wlast: True};
        m_xactor_sdram.i_wr_addr.enq(aw);
        m_xactor_sdram.i_wr_data.enq(w);
        $display("%d: Sending write request",$stime());
        end
        else begin
            if(rg_burst_count == 0) begin
                let aw = AXI4_Wr_Addr {awaddr: rg_tb_app_req_addr, awprot:0, awuser: 0, awlen: extend(rg_tb_app_req_len), awsize: 2, awburst: 'b01}; 
                let w = AXI4_Wr_Data {wdata:  64'hDEADBEEFBABECAFE, wstrb: rg_tb_wr_en_n, wlast: False};                
                data_change(rg_ff);
                rg_burst_count <= rg_burst_count + 1;
                m_xactor_sdram.i_wr_addr.enq(aw);
                m_xactor_sdram.i_wr_data.enq(w);

            end
            else if(rg_burst_count <= extend(rg_tb_app_req_len) && rg_burst_count != 0) begin
                let w = AXI4_Wr_Data {wdata:  rg_tb_wr_data, wstrb: rg_tb_wr_en_n, wlast: (rg_burst_count == extend(rg_tb_app_req_len))};
                rg_burst_count <= rg_burst_count + 1;
                $display("%d: Sending write data %x rg_ff %b",$stime(),rg_tb_wr_data, rg_ff);
                data_change(rg_ff);
                m_xactor_sdram.i_wr_data.enq(w);
                //tb_core.iapp_last_wr(pack(rg_burst_count == tb_core.oapp_req_len));
            end 
            else begin
                rg_burst_count <= 0;
                rg_state_cnt <= rg_state_cnt + 1;
            end
        end
   endrule

   rule rl_next_write(rg_state_cnt == 3);
        burst_write({32'b0,{8'd0,11'd1500,2'b01,8'd56,3'b0}}, 8'd246);
        rg_state_cnt <= rg_state_cnt + 1;
        rg_burst_count <= 0;
    endrule

   rule rl_next_write1(rg_state_cnt == 6);
        burst_write({32'b0,{8'd0,11'd2010,2'b01,8'd25,3'b0}}, 8'd7);
        rg_state_cnt <= rg_state_cnt + 1;
        rg_burst_count <= 0;
    endrule

   rule rl_next_write2(rg_state_cnt == 9);
        burst_write({32'b0,{8'd0,11'd1600,2'b10,8'd55,3'b0}}, 8'd4);
        rg_state_cnt <= rg_state_cnt + 1;
        rg_burst_count <= 0;
    endrule

   rule rl_next_write3(rg_state_cnt == 12);
        burst_write({32'b0,{8'd0,11'h8,2'b0,8'd5,3'b0}}, 8'd3);
        rg_state_cnt <= rg_state_cnt + 1;
        rg_burst_count <= 0;
    endrule

   rule rl_next_write4(rg_state_cnt == 20);
        burst_write({32'b0,{8'd0,11'h8,2'b01,8'd255,3'b0}}, 8'd2);
        rg_state_cnt <= rg_state_cnt + 1;
        rg_burst_count <= 0;
    endrule

   rule rl_read_request(rg_state_cnt == 16 || rg_state_cnt == 19);
       let read_request = AXI4_Rd_Addr {araddr: rg_tb_app_rd_addr, arprot: 0, aruser: 0, arlen: extend(rg_tb_app_rd_len), arsize: 3, arburst: rg_bmode}; // arburst: 00-FIXED 01-INCR 10-WRAP
       m_xactor_sdram.i_rd_addr.enq(read_request);
       $display("%d: Sending read request",$stime());
       rg_state_cnt <= rg_state_cnt + 1;
   endrule

   rule rl_read_response(rg_state_cnt == 17 || rg_state_cnt == 20);
       if(rg_burst_count == 300) begin
           let response <- pop_o(m_xactor_sdram.o_rd_data);
           rg_tb_rd_data <= response.rdata;
           $display("%d: Read data %x",$stime(),response.rdata);
           rg_tb_app_rd_len <= rg_tb_app_rd_len - 1;
           if(rg_tb_app_rd_len == 0)
               rg_state_cnt <= rg_state_cnt + 1;
       end 
       else rg_burst_count <= rg_burst_count + 1;
   endrule

    rule rl_read_request_gen1(rg_state_cnt == 18);
        burst_read({32'b0,{8'd0,11'd0,2'b00,8'hA,3'b000}}, 8'd3);
        rg_bmode <= 'b10;
        rg_state_cnt <= rg_state_cnt + 1;
        rg_burst_count <= 0;
    endrule
   
   

   interface dq_0 = sdram_model_0.dq;
   interface dq_1 = sdram_model_1.dq;

   interface Ifc_tb_sdram_in ifc_tb_sdram_in;
        
//       interface Inout#(Bit#(32)) dq = tb_core.sdr_dq;

        method Action iAddr(Bit#(11) addr);
            sdram_model_0.iAddr(addr);
            sdram_model_1.iAddr(addr);
        endmethod

        method Action iBa(Bit#(2) ba);
            sdram_model_0.iBa(ba);
            sdram_model_1.iBa(ba);
        endmethod

        method Action iCke(bit cke);
            sdram_model_0.iCke(cke);
            sdram_model_1.iCke(cke);
        endmethod

        method Action iClk(bit clk);
            sdram_model_0.iClk(clk);
            sdram_model_1.iClk(clk);
        endmethod

        method Action iCs_n(bit cs_n);
            sdram_model_0.iCs_n(cs_n);
            sdram_model_1.iCs_n(cs_n);
        endmethod

        method Action iRas_n(bit ras_n);
            sdram_model_0.iRas_n(ras_n);
            sdram_model_1.iRas_n(ras_n);
        endmethod

        method Action iCas_n(bit cas_n);
            sdram_model_0.iCas_n(cas_n);
            sdram_model_1.iCas_n(cas_n);
        endmethod

        method Action iWe_n(bit we_n);
            sdram_model_0.iWe_n(we_n);
            sdram_model_1.iWe_n(we_n);
        endmethod

        method Action iDqm(Bit#(8) dqm);
            sdram_model_0.iDqm(dqm[3:0]);
            sdram_model_1.iDqm(dqm[7:4]);
        endmethod

    endinterface

   interface axi4_sdram = m_xactor_sdram.axi_side;
   interface axi4_cntrl_reg = m_xactor_cntrl_reg.axi_side;

endmodule
endpackage




