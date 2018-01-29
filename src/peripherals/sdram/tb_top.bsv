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
package tb_top;

import Semi_FIFOF        :: *;
import AXI4_Types   :: *;
import AXI4_Fabric  :: *;
import sdr_top           :: *;
import tb_bsv_wrapper    :: *;
import Connectable       :: *;
`include "defined_parameters.bsv"


typedef 1 Num_Masters;
typedef 1 Num_Slaves;

function Tuple2 #(Bool, Bit#(TLog#(Num_Slaves))) fn_addr_to_slave_num (Bit #(`PADDR) addr);
		return tuple2(True, 0);
endfunction

(*synthesize*)
module mkTb_top(Empty);


   AXI4_Fabric_IFC #(Num_Masters, Num_Slaves, `PADDR, `Reg_width,`USERSPACE)
               fabric_m <- mkAXI4_Fabric(fn_addr_to_slave_num);
   AXI4_Fabric_IFC #(Num_Masters, Num_Slaves, `PADDR, `Reg_width,`USERSPACE)
               fabric_c <- mkAXI4_Fabric(fn_addr_to_slave_num);

   Ifc_sdr_slave sdr_cntrl <- mksdr_axi4_slave;
               
   Ifc_tb_bsv_wrapper tb_wrapper <- mktb_bsv_wrapper;

   mkConnection (tb_wrapper.axi4_sdram, fabric_m.v_from_masters [0]);
   mkConnection(fabric_m.v_to_slaves[0], sdr_cntrl.axi4_slave_sdram);  
   mkConnection (tb_wrapper.axi4_cntrl_reg, fabric_c.v_from_masters [0]);
   mkConnection(fabric_c.v_to_slaves[0], sdr_cntrl.axi4_slave_cntrl_reg);  
   mkConnection(tb_wrapper.dq_0, sdr_cntrl.sdr_dq0);  
   mkConnection(tb_wrapper.dq_1, sdr_cntrl.sdr_dq1);  


  rule rl_iAddr_connection;
      let in = sdr_cntrl.ifc_sdram_out.osdr_addr();
     tb_wrapper.ifc_tb_sdram_in.iAddr(truncate(in));
  endrule

  rule rl_iBa_connection;
      let in = sdr_cntrl.ifc_sdram_out.osdr_ba();
      tb_wrapper.ifc_tb_sdram_in.iBa(in);
  endrule

  rule rl_iCke_connection;
      let in = sdr_cntrl.ifc_sdram_out.osdr_cke();
      tb_wrapper.ifc_tb_sdram_in.iCke(pack(in));
  endrule

  rule rl_iClk_connection;
      let in = sdr_cntrl.ifc_sdram_out.osdr_clk();
      tb_wrapper.ifc_tb_sdram_in.iClk(in);
  endrule

  rule rl_iCs_n_connection;
      let in = sdr_cntrl.ifc_sdram_out.osdr_cs_n();
      tb_wrapper.ifc_tb_sdram_in.iCs_n(pack(in));
  endrule

  rule rl_iRas_n_connection;
      let in = sdr_cntrl.ifc_sdram_out.osdr_ras_n();
      tb_wrapper.ifc_tb_sdram_in.iRas_n(pack(in));
  endrule

  rule rl_iCas_n_connection;
      let in = sdr_cntrl.ifc_sdram_out.osdr_cas_n();
      tb_wrapper.ifc_tb_sdram_in.iCas_n(pack(in));
  endrule

  rule rl_iWe_n_connection;
      let in = sdr_cntrl.ifc_sdram_out.osdr_we_n();
      tb_wrapper.ifc_tb_sdram_in.iWe_n(pack(in));
  endrule

  rule rl_iDqm_connection;
      let in = sdr_cntrl.ifc_sdram_out.osdr_dqm();
      tb_wrapper.ifc_tb_sdram_in.iDqm(extend(in));
  endrule

 endmodule
 endpackage
