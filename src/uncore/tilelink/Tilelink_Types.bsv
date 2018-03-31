package Tilelink_Types;

`include "defined_parameters.bsv"
import GetPut ::*;
import FIFO ::*;
import SpecialFIFOs ::*;
import Connectable ::*;

//`ifdef TILEUH
//`define LANE_WIDTH 8
//`define XLEN 8
`define TILEUH

Integer v_lane_width = valueOf(`LANE_WIDTH);

typedef enum {	Get_data
				,GetWrap
				,PutPartialData
				,PutFullData
`ifdef TILEUH
				,ArithmeticData
				,LogicalData
				,Intent				
`endif
`ifdef TILEUC
				,Acquire
`endif
} Opcode deriving(Bits, Eq, FShow);			
			
typedef enum {	AccessAck
				,AccessAckData
`ifdef TILEUH
				,HintAck
`endif
`ifdef TILEUC
				,Grant
				,GrantData
`endif
} D_Opcode deriving(Bits, Eq, FShow);			

typedef enum { Min,
			   Max,
			   MinU,
			   MaxU,
			   ADD 
} Param_arith deriving(Bits, Eq, FShow);

typedef enum { Min,
			   Max,
			   MinU,
			   MaxU,
			   ADD 
} Param_logical deriving(Bits, Eq, FShow);

typedef Bit#(3) Param;
typedef Bit#(4) Data_size; //In bytes
typedef Bit#(2) M_source;
typedef Bit#(5) S_sink;
typedef Bit#(`PADDR) Address_width;
typedef Bit#(`LANE_WIDTH) Mask;
typedef Bit#(TMul#(8,`LANE_WIDTH)) Data;

/* The A-channel is responsible for the master requests. The channel is A is split in control section(c) 
data section(d) where the read masters only use control section and write masters use both. For the slave side
where it receives the request has the channel A intact.
*/
typedef struct { 
		Opcode 			     a_opcode;                 //The opcode specifies if write or read requests
`ifdef TILEUH
		Param  			     a_param;                  //Has the encodings for atomic transfers
`endif
		Data_size			 a_size;                   //The transfer size in 2^a_size bytes. The burst is calculated from here. if this is >3 then its a burst
		M_source 		     a_source;                 //Master ID
		Address_width		 a_address;                //Address for the request
} A_channel_control deriving(Bits, Eq, FShow);
		
typedef struct { 
		Mask						 a_mask;           //8x(bytes in data lane) 1 bit mask for each byte 
		Data						 a_data;			//data for the request	
} A_channel_data deriving(Bits, Eq, FShow);

typedef struct { 
		Opcode 			     a_opcode;
`ifdef TILEUH
		Param  			     a_param;
`endif
		Data_size			 a_size;
		M_source 		     a_source;
		Address_width		 a_address;
		Mask				 a_mask;
		Data				 a_data;	
} A_channel deriving(Bits, Eq, FShow);

//cache-coherence channels
typedef struct {                                        
		Opcode 			     b_opcode;
		Param  			     b_param;
		Data_size			 b_size;
		M_source 		     b_source;
		Address_width		 b_address;
		Mask				 b_mask;
		Data				 b_data;	
} B_channel deriving(Bits, Eq, FShow);

//cache-coherence channels
typedef struct { 
		Opcode 			     c_opcode;
		Param  			     c_param;
		Data_size			 c_size;
		M_source 		     c_source;
		Address_width		 c_address;
		Data				 c_data;	
		Bool				 c_client_error;
} C_channel deriving(Bits, Eq);

//The channel D is responsible for the slave responses. It has the master ids and slave ids carried through the channel
typedef struct { 
		D_Opcode 			d_opcode;                     //Opcode encodings for response with data or just ack
		Param  			d_param;
		Data_size		d_size;
		M_source 		d_source;
		S_sink			d_sink;
		Data			d_data;	
		Bool			d_error;
} D_channel deriving(Bits, Eq, FShow);

typedef struct { 
		S_sink					 d_sink;
} E_channel deriving(Bits, Eq);

interface Ifc_core_side_master_link;

	//Towards the master
	interface Put#(A_channel_data) master_request_data;
	interface Put#(A_channel_control) master_request_control;
	interface Get#(D_channel) master_response;

endinterface

interface Ifc_fabric_side_master_link;

	//Towards the fabric
	interface Get#(A_channel_data) fabric_request_data;
	interface Get#(A_channel_control) fabric_request_control;
	interface Put#(D_channel) fabric_response;

endinterface

//--------------------------------------Master Xactor--------------------------------------//
/* This is a xactor interface which connects core and master side of the fabric*/
interface Ifc_Master_link;

interface Ifc_core_side_master_link core_side;
interface Ifc_fabric_side_master_link fabric_side;

endinterface

/* Master transactor - should be instantiated in the core side and the fabric side interface of
of the xactor should be exposed out of the core*/
module mkMasterXactor#(Bool xactor_guarded, Bool fabric_guarded)(Ifc_Master_link);

//Created a pipelined version that will have a critical path all along the bus. If we want to break the path we can 
//make the bus stall-less 
`ifdef TILELINK_LIGHT
	FIFOF#(A_channel_control) ff_xactor_request_c <- mkGFIFOF(xactor_guarded, fabric_guarded, 2); //control split of A-channel
	FIFOF#(A_channel_data) ff_xactor_request_d <- mkGFIFOF(xactor_guarded, fabric_guarded, 2);   //data split of A-channel
	FIFOF#(D_channel) ff_xactor_response <- mkGFIFOF(xactor_guarded, fabric_guarded, 2); //response channel D-channel exposed out
`else
	FIFO#(A_channel_control) ff_xactor_request_c <- mkSizedFIFO(2);
	FIFO#(A_channel_data) ff_xactor_request_d <- mkSizedFIFO(2);
	FIFO#(D_channel) ff_xactor_response <- mkSizedFIFO(2);
`endif

	Reg#(Data_size) rg_burst_counter <- mkReg(0);
	Reg#(Bool) rg_burst[2] <- mkCReg(2,False);

// If it is a burst dont ask for address again. This rule about calculating the burst and telling the control split of A-channel to keep
//quite till the burst finishes.
	rule rl_xactor_to_fabric_data;
		let req_addr = ff_xactor_request_c.first;
		Data_size burst_size = 1;										  //The total number of bursts
		Data_size transfer_size = req_addr.a_size;                        //This is the total transfer size including the bursts
		if(!rg_burst[0]) begin
			if(transfer_size > 3) begin
				rg_burst[0] <= True;
				transfer_size = transfer_size - 3;
				burst_size = burst_size << transfer_size;
				rg_burst_counter <= burst_size - 1;
			end
		end
		else begin
			rg_burst_counter <= rg_burst_counter - 1;                         
			if(rg_burst_counter==1)
				rg_burst[0] <= False;
		end

	endrule
	
	interface core_side = interface Ifc_core_side_master_link
		interface master_request_data = toPut(ff_xactor_request_d);
		interface master_request_control = toPut(ff_xactor_request_c);
		interface master_response = toGet(ff_xactor_response);
	endinterface;

	interface fabric_side = interface Ifc_fabric_side_master_link 
		interface fabric_request_control = interface Get
											 method ActionValue#(A_channel_control) get;        //Deque the control split of a channel if only burst is finished 
												A_channel_control req_addr = ff_xactor_request_c.first;
												if(!rg_burst[1])
													ff_xactor_request_c.deq;
												return req_addr;
											 endmethod
										   endinterface;
		interface fabric_request_data = toGet(ff_xactor_request_d);
		interface fabric_response = toPut(ff_xactor_response);
	endinterface;
	
endmodule

//------------------------------------------------------------------------------------------------------------------//


//------------------------------------------------------Slave Xactor------------------------------------------------//

interface Ifc_core_side_slave_link;
	interface Get#(A_channel) xactor_request;
	interface Put#(D_channel) xactor_response;
endinterface

interface Ifc_fabric_side_slave_link;
	interface Put#(A_channel) fabric_request;
	interface Get#(D_channel) fabric_response;
endinterface

interface Ifc_Slave_link;
	interface Ifc_core_side_slave_link core_side;
	interface Ifc_fabric_side_slave_link fabric_side;
endinterface

module mkSlaveXactor#(Bool xactor_guarded, Bool fabric_guarded)(Ifc_Slave_link);

`ifdef TILELINK_LIGHT
	FIFOF#(A_channel) ff_xactor_request <- mkGFIFOF(xactor_guarded, fabric_guarded, 2);
	FIFOF#(D_channel) ff_xactor_response <- mkGFIFOF(xactor_guarded, fabric_guarded, 2);
`else
	FIFO#(A_channel) ff_xactor_request <- mkSizedFIFO(2);
	FIFO#(D_channel) ff_xactor_response <- mkSizedFIFO(2);
`endif

	//rule rl_xactor_to_fabric(!isValid(rg_d_channel));
	//	let lv_response = ff_xactor_response.first;
	//	rg_d_channel <= tagged Valid lv_response;
	//	ff_xactor_response.deq;
	//endrule

	//rule rl_fabric_to_xactor(rg_a_channel matches tagged Valid .req);
	//	let lv_req = req;
	//	ff_xactor_request.enq(req);
	//	rg_a_channel <= tagged Invalid
	//endrule

interface core_side = interface Ifc_core_side_slave_link;
	interface xactor_request = toGet(ff_xactor_request);
	interface xactor_response = toPut(ff_xactor_response);
endinterface;

interface fabric_side = interface Ifc_fabric_side_slave_link;
	interface fabric_request = toPut(ff_xactor_request);
	interface fabric_response = toGet(ff_xactor_response);
endinterface;

endmodule

//----------------------------------------------- Master Fabric -------------------------------------//

interface Ifc_Master_fabric_side_a_channel;
	(* always_ready *)
	method A_channel fabric_a_channel;
	(* always_ready *)
	method Bool fabric_a_channel_valid;
	(* always_ready, always_enabled *)
	method Action fabric_a_channel_ready(Bool req_ready);
endinterface

interface Ifc_Master_fabric_side_d_channel;
	(* always_ready, always_enabled *)
	method Action fabric_d_channel(D_channel resp);
	(* always_ready *)
	method Bool fabric_d_channel_ready;
endinterface

	//Communication with the xactor
interface Ifc_master_tilelink_core_side;
	interface Put#(A_channel_control) xactor_request_control;
	interface Put#(A_channel_data) xactor_request_data;
	interface Get#(D_channel) xactor_response;
endinterface

interface Ifc_Master_tilelink;

	interface Ifc_master_tilelink_core_side v_from_masters;

	//communication with the fabric
	interface Ifc_Master_fabric_side_d_channel fabric_side_response;
	interface Ifc_Master_fabric_side_a_channel fabric_side_request;
endinterface

module mkMasterFabric(Ifc_Master_tilelink);

    Reg#(A_channel_control) rg_a_channel_c[3] <- mkCReg(3, A_channel_control { a_opcode : ?,
												`ifdef TILEUH	 a_param  : ?, `endif
																 a_size : ?,
																 a_source : ?,
																 a_address :  ? });
    Reg#(Maybe#(A_channel_data)) rg_a_channel_d[3] <- mkCReg(3, tagged Invalid);
    Reg#(Maybe#(D_channel)) rg_d_channel[3] <- mkCReg(3, tagged Invalid);

	interface v_from_masters = interface Ifc_master_tilelink_core_side
		interface xactor_request_control = interface Put
												method Action put(A_channel_control req_control);
													rg_a_channel_c[0] <= req_control;
													`ifdef verbose $display($time, "\tTILELINK : Request from Xactor control signals", fshow(req_control)); `endif
												endmethod
										   endinterface;

		interface xactor_request_data = interface Put
												method Action put(A_channel_data req_data);
													rg_a_channel_d[0] <= tagged Valid req_data;
													`ifdef verbose $display($time, "\tTILELINK : Request from Xactor data signals", fshow(req_data)); `endif
												endmethod
										endinterface;
												
		interface xactor_response = interface Get;
												method ActionValue#(D_channel) get if(isValid(rg_d_channel[1]));
													let resp = validValue(rg_d_channel[1]);
													rg_d_channel[1] <= tagged Invalid;
													`ifdef verbose $display($time, "\tTILELINK : Response to Xactor data signals", fshow(resp)); `endif
													return resp;
												endmethod
										endinterface;
	endinterface;
												
	interface fabric_side_response = interface Ifc_Master_fabric_side_d_channel
										method Action fabric_d_channel(D_channel resp);
											rg_d_channel[0] <= tagged Valid resp; 
										endmethod
										method Bool fabric_d_channel_ready;
											return !isValid(rg_d_channel[0]);
										endmethod
									endinterface;

	//while sending it to the fabric the control section and the data section should be merged
	interface fabric_side_request = interface Ifc_Master_fabric_side_a_channel
										method A_channel fabric_a_channel;
											A_channel req = A_channel {a_opcode : rg_a_channel_c[1].a_opcode,
														`ifdef TILEUH	a_param : rg_a_channel_c[1].a_param, `endif
																		a_size : rg_a_channel_c[1].a_size,
																		a_source : rg_a_channel_c[1].a_source,
																		a_address : rg_a_channel_c[1].a_address,
																		a_mask : validValue(rg_a_channel_d[1]).a_mask,
																		a_data : validValue(rg_a_channel_d[1]).a_data};
											return req;
										endmethod
										method Bool fabric_a_channel_valid;           //master valid signal to the fabric
											return isValid(rg_a_channel_d[1]);
										endmethod
										method Action fabric_a_channel_ready(Bool req_ready); //master ready signal to the fabric
											if(req_ready)
												rg_a_channel_d[1] <= tagged Invalid;
										endmethod
									endinterface;

endmodule


//----------------------------------------------- Slave Fabric -------------------------------------//

interface Ifc_slave_tilelink_core_side;
	//communication with the xactors
	interface Get#(A_channel) xactor_request;
	interface Put#(D_channel) xactor_response;
endinterface
interface Ifc_Slave_fabric_side_a_channel;
	(* always_ready, always_enabled *)
	method Action fabric_a_channel(A_channel req);
	(* always_ready *)
	method Bool fabric_a_channel_ready;
endinterface

interface Ifc_Slave_fabric_side_d_channel;
	(* always_ready *)
	method D_channel fabric_d_channel;
	(* always_ready *)
	method Bool fabric_d_channel_valid;
	(* always_ready, always_enabled *)
	method Action fabric_d_channel_ready(Bool req_ready);
endinterface

interface Ifc_Slave_tilelink;

	interface Ifc_slave_tilelink_core_side v_to_slaves;

	//communication with the fabric
	interface Ifc_Slave_fabric_side_d_channel fabric_side_response;
	interface Ifc_Slave_fabric_side_a_channel fabric_side_request;

endinterface

module mkSlaveFabric(Ifc_Slave_tilelink);

    Reg#(Maybe#(A_channel)) rg_a_channel[3] <- mkCReg(3, tagged Invalid);
    Reg#(Maybe#(D_channel)) rg_d_channel[3] <- mkCReg(3, tagged Invalid);


	interface v_to_slaves = interface Ifc_slave_tilelink_core_side ;
		interface xactor_request = interface Get
												method ActionValue#(A_channel) get if(isValid(rg_a_channel[1]));
													let req = validValue(rg_a_channel[1]);
													rg_a_channel[1] <= tagged Invalid;
													`ifdef verbose $display($time, "\tTILELINK : Slave side request to Xactor ", fshow(req)); `endif
													return req;
												endmethod
										   endinterface;

		interface xactor_response = interface Put
												method Action put(D_channel resp) if(!isValid(rg_d_channel[0]));
													`ifdef verbose $display($time, "\tTILELINK : Slave side response from Xactor ", fshow(resp)); `endif
													rg_d_channel[0] <= tagged Valid resp;
												endmethod
										   endinterface;
	endinterface;
												

	interface fabric_side_response = interface Ifc_Slave_fabric_side_d_channel
										method D_channel fabric_d_channel;
											return validValue(rg_d_channel[1]);
										endmethod
										method Bool fabric_d_channel_valid;
											return isValid(rg_d_channel[1]);
										endmethod
					//if the beat has been exchanged the packet can be invalidated on the sending side	
										method Action fabric_d_channel_ready(Bool req_ready);
											if(req_ready)
												rg_d_channel[1] <= tagged Invalid;
										endmethod
									endinterface;

	interface fabric_side_request = interface Ifc_Slave_fabric_side_a_channel
					//if the beat has been exchanged the packet can be invalidated on the sending side	
										method Action fabric_a_channel(A_channel req);
											rg_a_channel[0] <= tagged Valid req;
										endmethod
										method Bool fabric_a_channel_ready; 
											return !isValid(rg_a_channel[0]);
										endmethod
									endinterface;
endmodule

instance Connectable#(Ifc_fabric_side_master_link, Ifc_master_tilelink_core_side);
	
	module mkConnection#(Ifc_fabric_side_master_link xactor, Ifc_master_tilelink_core_side fabric)(Empty);
		
		rule rl_connect_control_request;
			let x <-  xactor.fabric_request_control.get;
			fabric.xactor_request_control.put(x); 
		endrule
		rule rl_connect_data_request;
			let x <-  xactor.fabric_request_data.get;
			fabric.xactor_request_data.put(x); 
		endrule
		rule rl_connect_data_response;
			let x <- fabric.xactor_response.get; 
			xactor.fabric_response.put(x);
		endrule
	endmodule

endinstance

instance Connectable#( Ifc_slave_tilelink_core_side, Ifc_fabric_side_slave_link);
	
	module mkConnection#(Ifc_slave_tilelink_core_side fabric, Ifc_fabric_side_slave_link xactor)(Empty);
		
		rule rl_connect_request;
			let x <- fabric.xactor_request.get;
			xactor.fabric_request.put(x);
		endrule
		rule rl_connect_data_response;
			let x <- xactor.fabric_response.get;
			fabric.xactor_response.put(x);
		endrule
	endmodule

endinstance

endpackage
