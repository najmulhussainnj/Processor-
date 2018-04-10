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
package Tilelink;

import Tilelink_Types ::*;
//import defined_types ::*;
import Vector ::*;
import GetPut ::*;

`include "defined_parameters.bsv"



interface Ifc_Master_tile#(numeric type a, numeric type w, numeric type z);
   interface Put#(A_channel_control#(a,z)) xactor_request_c_master;
   interface Put#(A_channel_data#(w)) xactor_request_d_master;
   interface Get#(D_channel#(w,z)) xactor_response_master;
endinterface

interface Ifc_Slave_tile#(numeric type a, numeric type w, numeric type z);
   interface Get#(A_channel#(a,w,z)) xactor_request_to_slave;
   interface Put#(D_channel#(w,z)) xactor_response_to_slave;
endinterface

// ================================================================
// The interface for the fabric module

interface Tilelink_Fabric_IFC #(numeric type num_masters,
				 				numeric type num_slaves, 
								numeric type a,
								numeric type w,
								numeric type z);
				 
   method Action reset;
   method Action set_verbosity (Bit #(4) verbosity);

   // From masters
   interface Vector #(num_masters, Ifc_master_tilelink_core_side#(a,w,z))  v_from_masters;

   // To slaves
   interface Vector #(num_slaves,  Ifc_slave_tilelink_core_side#(a,w,z)) v_to_slaves;
endinterface

module mkTilelink#(function Tuple2 #(Bool, Bit#(TLog#(num_slaves))) 
				fn_addr_to_slave_num(Opcode command, Bit#(a) addr), 
				   Vector#(num_slaves, Bit#(num_masters)) master_route)
				(Tilelink_Fabric_IFC#(num_masters, num_slaves, a, w, z));


	// Transactors facing masters
	Vector #(num_masters,  Ifc_Master_tilelink#(a,w,z))
	   xactors_masters <- replicateM (mkMasterFabric);

	// Transactors facing slaves
	Vector #(num_slaves,   Ifc_Slave_tilelink#(a,w,z))
	    xactors_slaves    <- replicateM (mkSlaveFabric);

	function Bool fn_route_to_slave(Integer mj, Integer sj);
		Bool route_legal = False;
		let {legal, slave_num} = fn_addr_to_slave_num(xactors_masters[mj].fabric_side_request.fabric_a_channel.a_opcode,
											xactors_masters[mj].fabric_side_request.fabric_a_channel.a_address);
		if(legal && slave_num == fromInteger(sj))
			route_legal = True;
		return route_legal;
	endfunction

	//These rules connect the masters and the slaves. If the sender is valid and the receiver is ready the 
	//the packet is exchanged. In addition the route must valid. 

	//The slave destination is determined by address map function
	for(Integer s = 0; s < valueOf(num_slaves); s = s+1) begin
		for(Integer m =0; m <valueOf(num_masters); m = m+1) begin
		if(unpack(master_route[s][m])) begin
			rule rl_fabric_requests(fn_route_to_slave(m, s) && xactors_masters[m].fabric_side_request.fabric_a_channel_valid
															&& xactors_slaves[s].fabric_side_request.fabric_a_channel_ready);
				let req = xactors_masters[m].fabric_side_request.fabric_a_channel; 
					xactors_masters[m].fabric_side_request.fabric_a_channel_ready(True);
					xactors_slaves[s].fabric_side_request.fabric_a_channel(req);
					`ifdef verbose $display($time, "\tTILELINK : Beat exchanged from master %d to slave %d", m, s); `endif
				//else if() //TODO send the slave error
			endrule
		end
		end
	end

	//The master destination is determined by the signal in the D channel - d_source
	for(Integer m = 0; m < valueOf(num_masters); m = m+1) begin
		Rules rl_to_master = emptyRules();
		for(Integer s = 0; s < valueOf(num_slaves); s = s+1) begin
			Rules rs_to_master = (rules
			rule rl_fabric_responses(xactors_slaves[s].fabric_side_response.fabric_d_channel.d_source==fromInteger(m)
									 	&&	xactors_slaves[s].fabric_side_response.fabric_d_channel_valid);
				let resp = xactors_slaves[s].fabric_side_response.fabric_d_channel; 
				if(	xactors_masters[m].fabric_side_response.fabric_d_channel_ready) begin
					xactors_slaves[s].fabric_side_response.fabric_d_channel_ready(True);
					xactors_masters[m].fabric_side_response.fabric_d_channel(resp);
					`ifdef verbose $display($time, "\tTILELINK : Beat exchanged from slave %d to master %d", s, m); `endif
				end
				//else if() //TODO send the slave error
			endrule
			endrules);
			rl_to_master = rJoinPreempts(rs_to_master, rl_to_master);
		end
		addRules(rl_to_master);
	end

   Vector #(num_masters, Ifc_master_tilelink_core_side#(a,w,z))  temp_v_from_masters;

   Vector #(num_slaves,  Ifc_slave_tilelink_core_side#(a,w,z)) temp_v_to_slaves;

	for(Integer m=0; m < valueOf(num_masters); m=m+1) begin 

		temp_v_from_masters[m] = xactors_masters[m].v_from_masters;

	end

	for(Integer s=0; s < valueOf(num_slaves); s=s+1) begin

		temp_v_to_slaves[s] = xactors_slaves[s].v_to_slaves;

	end

	//interface to connect to trasactor masters
	interface v_from_masters = temp_v_from_masters;
	//interface to connect to trasactor slaves
	interface v_to_slaves = temp_v_to_slaves;

endmodule

endpackage
