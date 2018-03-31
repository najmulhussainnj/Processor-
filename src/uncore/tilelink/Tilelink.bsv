package Tilelink;

import Tilelink_Types ::*;
import defined_types ::*;
import TLMemoryMap ::*;
import Vector ::*;
import GetPut ::*;



interface Ifc_Master_tile;
   interface Put#(A_channel_control) xactor_request_c_master;
   interface Put#(A_channel_data) xactor_request_d_master;
   interface Get#(D_channel) xactor_response_master;
endinterface

interface Ifc_Slave_tile;
   interface Get#(A_channel) xactor_request_to_slave;
   interface Put#(D_channel) xactor_response_to_slave;
endinterface

// ================================================================
// The interface for the fabric module

interface Tilelink_Fabric_IFC #(numeric type num_masters,
				 numeric type num_slaves);
				 
   method Action reset;
   method Action set_verbosity (Bit #(4) verbosity);

   // From masters
   interface Vector #(num_masters, Ifc_master_tilelink_core_side)  v_from_masters;

   // To slaves
   interface Vector #(num_slaves,  Ifc_slave_tilelink_core_side) v_to_slaves;
endinterface

(*synthesize*)
module mkTilelink(Tilelink_Fabric_IFC#(Num_Masters, Num_Slaves));

Vector#(Num_Slaves, Bit#(Num_Masters)) master_route; // encoding -----> DMA, Debug, IMEM, DMEM write, DMEM read  
Vector#(Num_Slaves, Bit#(Num_Masters)) master_slow_route;

	master_route[valueOf(Sdram_slave_num)]        = truncate(5'b11101);
	master_route[valueOf(Sdram_slave_num_wr)]           = truncate(5'b11010);
	`ifdef SDRAM master_route[valueOf(Sdram_cfg_slave_num)]      = truncate(5'b11011); `endif
	`ifdef TCM master_route[valueOf(TCM_slave_num)]              = truncate(5'b11111); `endif
	`ifdef BOOTROM master_route[valueOf(BootRom_slave_num)]      = truncate(5'b11101); `endif
	`ifdef DEBUG master_route[valueOf(Debug_slave_num)]          = truncate(5'b11111); `endif
	`ifdef DMA master_route[valueOf(Dma_slave_num)]              = truncate(5'b11011); `endif
	master_route[valueOf(SlowPeripheral_slave_num)]  = truncate(5'b11011);
	master_slow_route[valueOf(Uart0_slave_num)]              = truncate(5'b11011);
	master_slow_route[valueOf(Uart1_slave_num)]              = truncate(5'b11011);
	master_slow_route[valueOf(CLINT_slave_num)]              = truncate(5'b01011);
	master_slow_route[valueOf(Plic_slave_num)]               = truncate(5'b01011);
	master_slow_route[valueOf(I2c0_slave_num)]               = truncate(5'b11011);
	master_slow_route[valueOf(I2c1_slave_num)]               = truncate(5'b11011);
	master_slow_route[valueOf(Qspi0_slave_num)]              = truncate(5'b11011);
	master_slow_route[valueOf(Qspi1_slave_num)]              = truncate(5'b11011);
	master_slow_route[valueOf(AxiExp1_slave_num)]            = truncate(5'b11011);

	// Transactors facing masters
	Vector #(Num_Masters,  Ifc_Master_tilelink)
	   xactors_masters <- replicateM (mkMasterFabric);

	// Transactors facing slaves
	Vector #(Num_Slaves,   Ifc_Slave_tilelink)
	    xactors_slaves    <- replicateM (mkSlaveFabric);

	function Bool fn_route_to_slave(Integer mj, Integer sj);
		Bool route_legal = False;
		let {legal, slave_num} = fn_addr_to_slave_num(xactors_masters[mj].fabric_side_request.fabric_a_channel.a_opcode,
											xactors_masters[mj].fabric_side_request.fabric_a_channel.a_address, fromInteger(mj));
		if(legal && slave_num == fromInteger(sj))
			route_legal = True;
		return route_legal;
	endfunction

	//These rules connect the masters and the slaves. If the sender is valid and the receiver is ready the 
	//the packet is exchanged. In addition the route must valid. 

	//The slave destination is determined by address map function
	for(Integer s = 0; s < valueOf(Num_Slaves); s = s+1) begin
		for(Integer m =0; m <valueOf(Num_Masters); m = m+1) begin
		if(master_route[s][m]==1) begin
			rule rl_fabric_requests(fn_route_to_slave(m, s) && xactors_masters[m].fabric_side_request.fabric_a_channel_valid
															&& xactors_slaves[s].fabric_side_request.fabric_a_channel_ready);
				let req = xactors_masters[m].fabric_side_request.fabric_a_channel; 
				//let {valid, slave_id} = fn_addr_to_slave_num(req.a_address, fromInteger(m));            //address map function
					xactors_masters[m].fabric_side_request.fabric_a_channel_ready(True);
					xactors_slaves[s].fabric_side_request.fabric_a_channel(req);
					`ifdef verbose $display($time, "\tTILELINK : Beat exchanged from master %d to slave %d", m, s); `endif
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
					`ifdef verbose $display($time, "\tTILELINK : Beat exchanged from slave %d to master %d", s, m); `endif
				end
				//else if() //TODO send the slave error
			endrule
			endrules);
			rl_to_master = rJoinPreempts(rs_to_master, rl_to_master);
		end
		addRules(rl_to_master);
	end

   Vector #(num_masters, Ifc_master_tilelink_core_side)  temp_v_from_masters;

   Vector #(num_slaves,  Ifc_slave_tilelink_core_side) temp_v_to_slaves;

	for(Integer m=0; m < valueOf(Num_Masters); m=m+1) begin 

		temp_v_from_masters[m] = xactors_masters[m].v_from_masters;

	end

	for(Integer s=0; s < valueOf(Num_Slaves); s=s+1) begin

		temp_v_to_slaves[s] = xactors_slaves[s].v_to_slaves;

	end

	interface v_from_masters = temp_v_from_masters;
	interface v_to_slaves = temp_v_to_slaves;

endmodule

endpackage
