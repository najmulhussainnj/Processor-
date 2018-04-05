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

package Tilelink_lite;

import Tilelink_lite_Types ::*;
`include "defined_parameters.bsv"
import defined_types ::*;
import Vector ::*;
import GetPut ::*;



interface Ifc_Master_tile;
   interface Put#(A_channel_lite) xactor_request_d_master;
   interface Get#(D_channel_lite) xactor_response_master;
endinterface

interface Ifc_Slave_tile_lite;
   interface Get#(A_channel_lite) xactor_request_to_slave;
   interface Put#(D_channel_lite) xactor_response_to_slave;
endinterface

// ================================================================
// The interface for the fabric module

interface Tilelink_Fabric_IFC_lite #(numeric type num_masters,
				 numeric type num_slaves, numeric type route);
				 
   method Action reset;
   method Action set_verbosity (Bit #(4) verbosity);

   // From masters
   interface Vector #(num_masters, Ifc_master_tilelink_core_side_lite)  v_from_masters;

   // To slaves
   interface Vector #(num_slaves,  Ifc_slave_tilelink_core_side_lite) v_to_slaves;
endinterface

module mkTilelinkLite#(function Tuple2 #(Bool, Bit#(TLog#(Num_Slow_Slaves))) 
				fn_address_mapping(Opcode_lite command, Bit#(`PADDR) addr))(Tilelink_Fabric_IFC_lite#(Num_Slow_Masters, Num_Slow_Slaves, route));

Vector#(Num_Slaves, Bit#(Num_Slow_Masters)) master_route; 
//The "master_route" variable indicates a matrix where columns are the masters and rows are slaves. 
													// encoding -----> DMA[4], Debug[3], IMEM[2], DMEM write[1], DMEM read[0]  
if(valueOf(route)==0) begin
	master_route[valueOf(Sdram_slave_num)]        				 = truncate(5'b11101);
	master_route[valueOf(Sdram_slave_num_wr)]           		 = truncate(5'b11010);
	`ifdef SDRAM master_route[valueOf(Sdram_cfg_slave_num)]      = truncate(5'b11011); `endif
	`ifdef TCM master_route[valueOf(TCM_slave_num)]              = truncate(5'b11111); `endif
	`ifdef BOOTROM master_route[valueOf(BootRom_slave_num)]      = truncate(5'b11101); `endif
	`ifdef DEBUG master_route[valueOf(Debug_slave_num)]          = truncate(5'b11111); `endif
	`ifdef DMA master_route[valueOf(Dma_slave_num)]              = truncate(5'b11011); `endif
	master_route[valueOf(SlowPeripheral_slave_num_rd)]  		 = truncate(5'b11101);
	master_route[valueOf(SlowPeripheral_slave_num_wr)]  		 = truncate(5'b11010);
end
else begin
	master_route[valueOf(Uart1_slave_num)]              		 = truncate(5'b11111);
	`ifdef UART0 master_route[valueOf(Uart0_slave_num)] 	     = truncate(5'b11011); 	`endif 
	`ifdef CLINT master_route[valueOf(CLINT_slave_num)] 	     = truncate(5'b01011);  `endif 
	`ifdef PLIC master_route[valueOf(Plic_slave_num)]   	     = truncate(5'b01011);  `endif 
	`ifdef I2C0 master_route[valueOf(I2c0_slave_num)]   	     = truncate(5'b11011);  `endif 
	`ifdef I2C1 master_route[valueOf(I2c1_slave_num)]   	     = truncate(5'b11011);  `endif 
	`ifdef QSPI0  master_route[valueOf(Qspi0_slave_num)]	     = truncate(5'b11011);  `endif 
	`ifdef QSPI1  master_route[valueOf(Qspi1_slave_num)]	     = truncate(5'b11011);  `endif 
	`ifdef AXIEXP master_route[valueOf(AxiExp1_slave_num)]       = truncate(5'b11011);  `endif 
end

	// Transactors facing masters
	Vector #(Num_Masters,  Ifc_Master_tilelink_lite)
	   xactors_masters <- replicateM (mkMasterFabricLite);

	// Transactors facing slaves
	Vector #(Num_Slaves,   Ifc_Slave_tilelink_lite)
	    xactors_slaves    <- replicateM (mkSlaveFabricLite);

	function Bool fn_route_to_slave(Integer mj, Integer sj);
		Bool route_legal = False;
		let {legal, slave_num} = fn_address_mapping(xactors_masters[mj].fabric_side_request.fabric_a_channel.a_opcode,
											xactors_masters[mj].fabric_side_request.fabric_a_channel.a_address);
		if(legal && slave_num == fromInteger(sj))
			route_legal = True;
		return route_legal;
	endfunction

	//These rules connect the masters and the slaves. If the sender is valid and the receiver is ready the 
	//the packet is exchanged. In addition the route must valid. 

	//The slave destination is determined by address map function
	for(Integer s = 0; s < valueOf(Num_Slow_Slaves); s = s+1) begin
		for(Integer m =0; m <valueOf(Num_Slow_Masters); m = m+1) begin
		if(master_route[s][m]==1) begin
			rule rl_fabric_requests(fn_route_to_slave(m, s) && xactors_masters[m].fabric_side_request.fabric_a_channel_valid
															&& xactors_slaves[s].fabric_side_request.fabric_a_channel_ready);
				let req = xactors_masters[m].fabric_side_request.fabric_a_channel; 
					xactors_masters[m].fabric_side_request.fabric_a_channel_ready(True);
					xactors_slaves[s].fabric_side_request.fabric_a_channel(req);
					`ifdef verbose $display($time, "\tTILELINK LITE : Beat exchanged from master %d to slave %d", m, s); `endif
				//else if() //TODO send the slave error
			endrule
		end
		end
	end

	//The master destination is determined by the signal in the D channel - d_source
	for(Integer m = 0; m < valueOf(Num_Masters); m = m+1) begin
		Rules rl_to_master = emptyRules();
		for(Integer s = 0; s < valueOf(Num_Slaves); s = s+1) begin
			Rules rs_to_master = (rules
			rule rl_fabric_responses(xactors_slaves[s].fabric_side_response.fabric_d_channel.d_source==fromInteger(m)
									 	&&	xactors_slaves[s].fabric_side_response.fabric_d_channel_valid);
				let resp = xactors_slaves[s].fabric_side_response.fabric_d_channel; 
				if(	xactors_masters[m].fabric_side_response.fabric_d_channel_ready) begin
					xactors_slaves[s].fabric_side_response.fabric_d_channel_ready(True);
					xactors_masters[m].fabric_side_response.fabric_d_channel(resp);
					`ifdef verbose $display($time, "\tTILELINK LITE : Beat exchanged from slave %d to master %d", s, m); `endif
				end
				//else if() //TODO send the slave error
			endrule
			endrules);
			rl_to_master = rJoinPreempts(rs_to_master, rl_to_master);
		end
		addRules(rl_to_master);
	end

   Vector #(num_masters, Ifc_master_tilelink_core_side_lite)  temp_v_from_masters;

   Vector #(num_slaves,  Ifc_slave_tilelink_core_side_lite) temp_v_to_slaves;

	for(Integer m=0; m < valueOf(Num_Slow_Masters); m=m+1) begin 

		temp_v_from_masters[m] = xactors_masters[m].v_from_masters;

	end

	for(Integer s=0; s < valueOf(Num_Slow_Slaves); s=s+1) begin

		temp_v_to_slaves[s] = xactors_slaves[s].v_to_slaves;

	end
	//interface to connect to trasactor masters
	interface v_from_masters = temp_v_from_masters;
	//interface to connect to trasactor slaves
	interface v_to_slaves = temp_v_to_slaves;

endmodule

endpackage
