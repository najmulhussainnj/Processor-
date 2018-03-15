
`include "defined_parameters"

`define LANE_WIDTH 8
`define XLEN 8

typedef enum {	Get
				,GetWrap
				,PutPartialdata
				,PutFulldata
`ifdef TILEUH
				,ArithmeticData
				,LogicalData
				,Intent				
`endif
`ifdef TILEUC
				,Acquire
`endif
} Opcode deriving(Bits, Eq, FShow);			
			
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

typedef Bit#(3) Param
typedef Bit#(4) Data_size //In bytes
typedef Bit#(2) M_source
typedef Bit#(5) S_sink
typedef Bit#(`PADDR) Address_width
typedef Bit#(`LANE_WIDTH) Mask
typedef Bit#(TMul#(8,`LANE_WIDTH)) Data 

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
} A_channel_control deriving(Bits, Eq);
		
typedef struct { 
		Mask						 a_mask;           //8x(bytes in data lane) 1 bit mask for each byte 
		Data						 a_data;			//data for the request	
} A_channel_data deriving(Bits, Eq);

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
} A_channel deriving(Bits, Eq);

//cache-coherence channels
typedef struct {                                        
		Opcode 			     b_opcode;
		Param  			     b_param;
		Data_size			 b_size;
		M_source 		     b_source;
		Address_width		 b_address;
		Mask				 b_mask;
		Data				 b_data;	
} B_channel deriving(Bits, Eq);

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
		Opcode 			d_opcode;                     //Opcode encodings for response with data or just ack
		Param  			d_param;
		Data_size		d_size;
		M_source 		d_source;
		S_sink			d_sink;
		Data			d_data;	
		Bool			d_client_error;
} D_channel deriving(Bits, Eq);

typedef struct { 
		S_sink					 d_sink;
} E_channel deriving(Bits, Eq);


//--------------------------------------MASTER XACTOR--------------------------------------//
/* This is a xactor interface which connects core and master side of the fabric*/
interface Ifc_Master_link;

//Towards the master
interface Put#(A_channel_control) master_request_control;
interface Put#(A_channel_data) master_request_data;
interface Get#(D_channel) master_response;

//Towards the fabric
interface Get#(A_channel_control) fabric_request_control;
interface Get#(A_channel_data) fabric_request_data;
interface Put#(D_channel) fabric_response;

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
`elsif
	FIFO#(A_channel_control) ff_xactor_request_c <- mkSizedFIFO(2);
	FIFO#(A_channel_data) ff_xactor_request_d <- mkSizedFIFO(2);
	FIFO#(D_channel) ff_xactor_response <- mkSizedFIFO(2);
`endif

	Reg#(Data_size) rg_burst_counter <- mkReg(0);
	Reg#(Bool) rg_burst[2] <- mkCReg(2,False);

// If it is a burst dont ask for address again. This rule about calculating the burst and telling the control split of A-channel to keep
//quite till the burst finishes.
	rule rl_xactor_to_fabric_data;
		let req_addr = ff_xactor_c_request.first;
		Data_size burst_size = 1;										  //The total number of bursts
		Data_size transfer_size = req_addr.a_size;                        //This is the total transfer size including the bursts
		if(!rg_burst[0]) begin
			if(transfer_size > 3) begin
				rg_burst[0] <= True;
				transfer_size = transfer_size - 3;
				rg_burst_counter <= burst_size << transfer_size;
			end
		end
		else begin
			rg_burst_counter <= rg_burst_counter - 1;                         
			if(rg_burst_counter==1)
				rg_burst[0] <= False;
		end

	endrule

	interface master_request_control = Get#(ff_xactor_c_request);
	interface master_request_data = Get#(ff_xactor_d_request);
	interface master_request_control = interface Get
										 method ActionValue#(A_channel_control) get;        //Deque the control split of a channel if only burst is finished 
											let req_addr = ff_xactor_c_request.first;
											if(!rg_burst[1])
												ff_xactor_c_request.deq;
											return req_addr;
										 endmethod
									   endinterface
	interface fabric_request_data = Get#(ff_xactor_d_request);
	interface fabric_response = Put#(ff_xactor_d_response);
	
endmodule

//------------------------------------------------------------------------------------------------------------------//


//------------------------------------------------------Slave Xactor------------------------------------------------//

interface Ifc_Slave_link;

interface Put#(A_channel) xactor_request;
interface Get#(D_channel) xactor_response;

interface Get#(A_channel) xactor_request;
interface Put#(D_channel) xactor_response;

endinterface

module mkSlave_xactor(Ifc_Slave_link);

`ifdef TILELINK_LIGHT
	FIFOF#(A_channel) ff_xactor_request <- mkGFIFOF(xactor_guarded, fabric_guarded, 2);
	FIFOF#(D_channel) ff_xactor_response <- mkGFIFOF(xactor_guarded, fabric_guarded, 2);
`elsif
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

interface master_request = Put#(ff_xactor_request);
interface master_response = Get#(ff_xactor_response);

interface fabric_request = Get#(ff_xactor_request);
interface fabric_response = Put#(ff_xactor_response);

endmodule

//----------------------------------------------- Master Fabric -------------------------------------//

interface Ifc_Master_fabric_side_a_channel;
	(* always_ready *)
	method A_channel fabric_a_channel;
	(* always_ready *)
	method Bool fabric_a_channel_valid;
	(* always_ready, always_enabled *);
	method Action fabric_a_channel_ready;
endinterface

interface Ifc_Master_fabric_side_d_channel;
	(* always_ready, always_enabled *);
	method Action fabric_d_channel;
	(* always_ready *)
	method Bool fabric_d_channel_ready;
endinterface

interface Ifc_Master_tilelink;

	//Communication with the xactor
	interface Put#(A_channel_control) xactor_request_control;
	interface Put#(A_channel_data) xactor_request_data;
	interface Get#(D_channel) xactor_response;

	//communication with the fabric
	interface Ifc_Master_fabric_side_d_channel fabric_side_response;
	interface Ifc_Master_fabric_side_a_channel fabric_side_request;
endinterface

module mkMasterFabric(Ifc_Master_Fabric);

    Reg#(A_channel_control) rg_a_channel[3] <- mkCReg(3, A_channel { a_opcode : ?,
															     a_param  : ?,
																 a_size : ?,
																 a_source : ?,
																 a_address :  ? });
    Reg#(Maybe#(A_channel_data)) rg_a_channel_d[3] <- mkCReg(3, tagged Invalid);
    Reg#(Maybe#(D_channel)) rg_d_channel[3] <- mkCReg(3, tagged Invalid);


	interface xactor_request_control = interface Put
											method Action put(A_channel_control req_control);
												rg_a_channel_c <= req_control;
											endmethod
									   endinterface

	interface xactor_request_data = interface Put
											method Action put(A_channel_data req_data);
												rg_a_channel_d <= req_data;
											endmethod
									endinterface
											
	interface xactor_response = interface Get;
											method ActionValue#(D_channel) get if(isValid(rg_d_channel));
												let resp = validValue(rg_d_channel);
												rg_d_channel <= tagged Invalid;
												return resp;
											endmethod
												

	interface fabric_side_response = interface Ifc_Master_fabric_side_d_channel
											(* always_ready, always_enabled *);
										method Action fabric_d_channel(D_channel resp);
											rg_d_channel <= tagged Valid resp; 
										endmethod
													(* always_ready *)
										method Bool fabric_d_channel_ready;
											return !isValid(rg_d_channel);
										endmethod
									endinterface;
	//while sending it to the fabric the control section and the data section should be merged
	interface fabric_side_request = interface Ifc_Master_fabric_side_a_channel
													(* always_ready *)
										method A_channel fabric_a_channel;
											A_channel req = A_channel {a_opcode : rg_a_channel_c.a_opcode,
																		a_param : rg_a_channel_c.a_param,
																		a_size : rg_a_channel_c.a_size,
																		a_source : rg_a_channel_c.a_source,
																		a_address : rg_a_channel_c.a_address,
																		a_mask : rg_c_channel_c.a_mask,
																		a_data : rg_c_channel_c.a_data};
											return validValue(req);
										endmethod
													(* always_ready *)
										method Bool fabric_a_channel_valid;           //master valid signal to the fabric
											return isValid(rg_a_channel_d);
										endmethod
											(* always_ready, always_enabled *);
										method Action fabric_a_channel_ready(Bool req_ready); //master ready signal to the fabric
											if(req_ready)
											rg_a_channel <= tagged Invalid;
										endmethod
									endinterface
endmodule


//----------------------------------------------- Slave Fabric -------------------------------------//

interface Ifc_Master_fabric_side_a_channel;
	(* always_ready, always_enabled *);
	method Action fabric_a_channel(A_channel req);
	(* always_ready *)
	method Bool fabric_a_channel_ready;
endinterface

interface Ifc_Master_fabric_side_d_channel;
	(* always_ready *)
	method D_channel fabric_d_channel;
	(* always_ready *)
	method Bool fabric_d_channel_valid;
	(* always_ready, always_enabled *);
	method Action fabric_a_channel_ready;
endinterface

interface Ifc_Slave_tilelink;

	//communication with the xactors
	interface Get#(A_channel) xactor_request;
	interface Put#(D_channel) xactor_response;

	//communication with the fabric
	interface Ifc_slave fabric_side_response;
	interface Ifc_Slave_fabric_side_a_channel fabric_side_request;

endinterface

module mkSlaveFabric(Ifc_Slave_Fabric);

    Reg#(Maybe#(A_channel)) rg_a_channel_d[3] <- mkCReg(3, tagged Invalid);
    Reg#(Maybe#(D_channel)) rg_d_channel[3] <- mkCReg(3, tagged Invalid);


	interface xactor_request_control = interface Get
											method ActionValue#(A_channel) get if(isValid(rg_a_channel);
												let req = validValue(rg_a_channel);
												rg_a_channel <= tagged Invalid;
												return req;
											endmethod
									   endinterface

	interface xactor_response = interface Put;
											method Action put(D_channel resp) if(!isValid(rg_d_channel));
												rg_d_channel <= tagged resp;
											endmethod
												

	interface fabric_side_response = interface Ifc_Slave_fabric_side_d_channel
												(* always_ready *)
										method D_channel fabric_d_channel(D_channel resp);
											return validValue(rg_d_channel);
										endmethod
												(* always_ready *)
										method Bool fabric_d_channel_valid;
											return isValid(rg_d_channel);
										endmethod
										(* always_ready, always_enabled *);
					//if the beat has been exchanged the packet can be invalidated on the sending side	
										method Action fabric_d_channel_ready(Bool req_ready);
											if(req_ready)
											rg_d_channel <= tagged Invalid;
										endmethod
									endinterface;

	interface fabric_side_request = interface Ifc_Master_fabric_side_a_channel
					//if the beat has been exchanged the packet can be invalidated on the sending side	
										(* always_ready, always_enabled *);
										method Action fabric_a_channel(A_channel req);
											rg_a_channel <= req;
										endmethod
												(* always_ready *)
										method Bool fabric_a_channel_ready; 
											return !isValid(rg_a_channel_d);
										endmethod
									endinterface
endmodule



