package Tilelink;

import Tilelink_Types.bsv ::*;
import defined_types.bsv ::*;

// ================================================================
// The interface for the fabric module

interface Tilelink_Fabric_IFC #(numeric type num_masters,
				 numeric type num_slaves);
				 
   method Action reset;
   method Action set_verbosity (Bit #(4) verbosity);

   // From masters
   interface Vector #(num_masters, Ifc_Master_tilelink)  v_from_masters;

   // To slaves
   interface Vector #(num_slaves,  Ifc_Slave_link) v_to_slaves;
endinterface

module mkTilelinkFabric(Tilelink_Fabric_IFC);

	// Transactors facing masters
	Vector #(num_masters, AXI4_Slave_Xactor_IFC  #(wd_addr, wd_data, wd_user))
	   xactors_masters <- replicateM (mkMasterFabric);

	// Transactors facing slaves
	Vector #(num_slaves,  AXI4_Master_Xactor_IFC #(wd_addr, wd_data, wd_user))
	    xactors_slaves    <- replicateM (mkSizedFIFO);

	//These rules connect the masters and the slaves. If the sender is valid and the receiver is ready the 
	//the packet is exchanged. In addition the route must valid. 

	//The slave destination is determined by address map function
	for(Integer m = 0; m < Num_masters; m = m+1) begin
		for(Integer s = 0; s < Num_masters; s = s+1) begin
			rule rl_fabric_requests;
				let req = xactors_masters[m].fabric_side_request.fabric_a_channel; 
				{valid, slave_id} = fn_addr_slave_num(req.a_address, fromInteger(m));            //address map function
				if(xactors_masters[m].fabric_side_request.fabric_a_channel_valid &&
						xactors_slaves[s].fabric_side_request.fabric_a_channel_ready &&
							valid && slave_id==s) begin
					xactors_masters[m].fabric_side_request.fabric_a_channel_ready(True);
					xactors_slaves[s].fabric_side_request.fabric_a_channel(req);
				end
				else if() //TODO send the slave error
			endrule
		end
	end

	//The master destination is determined by the signal in the D channel - d_source
	for(Integer m = 0; m < Num_masters; m = m+1) begin
		for(Integer s = 0; s < Num_masters; s = s+1) begin
			rule rl_fabric_responses;
				let resp = xactors_slaves[s].fabric_side_request.fabric_d_channel; 
				if(xactors_slaves[s].fabric_side_request.fabric_d_channel_valid &&
						xactors_masters[m].fabric_side_request.fabric_d_channel_ready &&
							resp.d_source==m) begin
					xactors_slaves[s].fabric_side_request.fabric_a_channel_ready(True);
					xactors_master[m].fabric_side_request.fabric_a_channel(resp);
				end
				else if() //TODO send the slave error
			endrule
		end
	end

endmodule

endpackage
