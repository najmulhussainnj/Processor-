
`include "defined_parameters"

`define LANE_WIDTH 8
`define XLEN 8

//typedef Bit#(3) Opcode
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
} deriving(Bits, Eq, FShow);			
			
typedef Bit#(3) Param
typedef Bit#(4) Data_size //In bytes
typedef Bit#(2) M_source
typedef Bit#(5) S_sink
typedef Bit#(`PADDR) Address_width
typedef Bit#(`LANE_WIDTH) Mask
typedef Bit#(TMul#(8,`LANE_WIDTH)) Data 

typedef struct { 
		Opcode 			     a_opcode;
`ifdef TILEUH
		Param  			     a_param;
`endif
		Data_size			 a_size;
		M_source 		     a_source;
		Address_width		 a_address;
} A_channel_control deriving(Bits, Eq);
		
typedef struct { 
		Mask						 a_mask;
		Data						 a_data;	
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

typedef struct { 
		Opcode 			     b_opcode;
		Param  			     b_param;
		Data_size			 b_size;
		M_source 		     b_source;
		Address_width		 b_address;
		Mask				 b_mask;
		Data				 b_data;	
} B_channel deriving(Bits, Eq);

typedef struct { 
		Opcode 			     c_opcode;
		Param  			     c_param;
		Data_size 	     c_size;
		M_source 		     c_source;
		Address_width    c_address;
		Data						 c_data;	
		Bool						 c_client_error;
} C_channel deriving(Bits, Eq);

typedef struct { 
		Opcode 			d_opcode;
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


function Bit#(`PADDR) address_incr(Bit#(`LANE_WIDTH) mask, Bit#(`PADDR) address, Bit#(4) Data_size, Bit#(3) Burst_type);
	Bit#(`PADDR) result_address;
	if(Burst_type==0)
		result_address = address;
	else if(Burst_type==1) begin
		result_address 
	end
	else begin // WRAP
		let new_addr=address;
		case (arlen) 
			1: new_addr[arsize]=~address[arsize];
			3: begin 
				if(arsize==0)
					new_addr[1:0]=new_addr[1:0]+1;
				else if(arsize==1)
					new_addr[2:1]=new_addr[2:1]+1;
				else if(arsize==2)
					new_addr[3:2]=new_addr[3:2]+1;
				else if(arsize==3)
					new_addr[4:3]=new_addr[4:3]+1;
			end
			7: begin 
				if(arsize==0)
					new_addr[2:0]=new_addr[2:0]+1;
				else if(arsize==1)
					new_addr[3:1]=new_addr[3:1]+1;
				else if(arsize==2)
					new_addr[4:2]=new_addr[4:2]+1;
				else if(arsize==3)
					new_addr[5:3]=new_addr[5:3]+1;
			end
			15:begin //Bit#(4) x = address[arsize+3:arsize]+1;new_addr[arsize+3:arsize]=x;end
				if(arsize==0)
					new_addr[3:0]=new_addr[3:0]+1;
				else if(arsize==1)
					new_addr[4:1]=new_addr[4:1]+1;
				else if(arsize==2)
					new_addr[5:2]=new_addr[5:2]+1;
				else if(arsize==3)
					new_addr[6:3]=new_addr[6:3]+1;
			end
		endcase
		return new_addr;
	end
endfunction


//--------------------------------------MASTER XACTOR--------------------------------------//

interface Ifc_Master_link;

interface Get#(A_channel_control) xactor_request_control;
interface Get#(A_channel_data) xactor_request_data;
interface Put#(D_channel) xactor_response;

interface Ifc_Master_fabric_side_a_channel fabric_request;
interface Ifc_Master_fabric_side_d_channel fabric_response;

endinterface

module mkMasterXactor#(Bool xactor_guarded, Bool fabric_guarded)(Ifc_Master_link);

`ifdef TILELINK_LIGHT
	FIFOF#(A_channel_control) ff_xactor_request_c <- mkGFIFOF(xactor_guarded, fabric_guarded, 2);
	FIFOF#(A_channel_data) ff_xactor_request_d <- mkGFIFOF(xactor_guarded, fabric_guarded, 2);
	FIFOF#(D_channel) ff_xactor_response <- mkGFIFOF(xactor_guarded, fabric_guarded, 2);
`elsif
	FIFO#(A_channel_control) ff_xactor_request_c <- mkSizedFIFO(2);
	FIFO#(A_channel_data) ff_xactor_request_d <- mkSizedFIFO(2);
	FIFO#(D_channel) ff_xactor_response <- mkSizedFIFO(2);
`endif

	Reg#(Data_size) rg_burst_counter <- mkReg(0);
	Reg#(Data_size) rg_burst_counter <- mkReg(0);
	Reg#(Bool) rg_burst <- mkReg(False);

	rule rl_xactor_to_fabric_address(!isValid(rg_a_channel_d) && !rg_burst);
		let lv_a_channel_c = ff_xactor_request_c.first;
		Data_size transfer_size = lv_a_channel_c.a_size;
		Data_size burst_size;
		rg_a_channel_c = lv_a_channel_c;
		if(transfer_size > 3) begin
			rg_burst <= True;
			transfer_size = transfer_size - 3;
			burst_size = 1 << transfer_size;
			rg_burst_counter <= burst_size;
		end
		else begin
			ff_xactor_request_c.deq;
		end
	endrule

	rule rl_xactor_to_fabric_data(!isValid(rg_a_channel_d) && rg_burst);
		rg_a_channel_d <= tagged Valid ff_xactor_d_request.first;
		rg_burst_counter = rg_burst_counter - 1;
		if(rg_burst_counter == 1) begin
			rg_burst <= False;
			ff_xactor_request_c.deq;
		end
	endrule

	rule rl_xactor_to_fabric_response(rg_d_channel matches tagged valid .resp);
		ff_xactor_response.enq(resp);
		rg_d_channel <= tagged Invalid;
	endrule

	rule rl_xactor_to_fabric_request(!isValid(
	interface xactor_request_control = Get#(ff_xactor_c_request);
	interface xactor_request_data = Get#(ff_xactor_d_request);
	interface fabric_side_request = interface Ifc_Master_fabric_side_a_channel
				method A_channel fabric_a_channel;
					return validValue(rg_a_channel); //Check out the type here 
				endmethod
				method Bool fabric_a_channel_valid;
					return IsValid(rg_a_channel_d);
				endmethod
				method Action fabric_a_channel_ready(Bool a_ready);
					if(a_ready)
						rg_a_channel_d <= tagged Invalid;	
				endmethod                        //TODO how to deal with ready and the data at the same time
			endinterface;

	interface fabric_side_response = interface Ifc_Master_fabric_side_d_channel
										method Action fabric_d_channel(D_channel response);
											rg_d_channel <= tagged Valid response;
										endmethod
										method Bool fabric_d_channel_ready;
											return !isValid(rg_d_channel);
										endmethod
									endinterface
endmodule

//------------------------------------------------------------------------------------------------------------------//


//------------------------------------------------------Slave Xactor------------------------------------------------//

interface Ifc_Slave_fabric_side_a_channel;
	method Action fabric_side_request(A_channel req);
	method Bool fabric_side_a_ready;
endinterface

interface Ifc_Slave_fabric_side_d_channel;
	method D_channel fabric_side_request;
	method Bool fabric_side_d_valid;
	method Action fabric_side_resp_read(Bool resp_ready);
endinterface

interface Ifc_Slave_link;

interface Put#(A_channel) xactor_request;
interface Get#(D_channel) xactor_response;

interface Ifc_Slave_fabric_side_a_channel fabric_side_a;
interface Ifc_Slave_fabric_side_d_channel fabric_side_d;

endinterface

module mkSlave_xactor(Ifc_Slave_link);

`ifdef TILELINK_LIGHT
	FIFOF#(A_channel) ff_xactor_request <- mkGFIFOF(xactor_guarded, fabric_guarded, 2);
	FIFOF#(D_channel) ff_xactor_response <- mkGFIFOF(xactor_guarded, fabric_guarded, 2);
`elsif
	FIFO#(A_channel) ff_xactor_request <- mkSizedFIFO(2);
	FIFO#(D_channel) ff_xactor_response <- mkSizedFIFO(2);
`endif

    Reg#(Maybe#(A_channel)) rg_a_channel <- mkCReg(tagged Invalid);
    Reg#(Maybe#(D_channel)) rg_d_channel <- mkCReg(tagged Invalid);

	rule rl_xactor_to_fabric(!isValid(rg_d_channel));
		let lv_response = ff_xactor_response.first;
		rg_d_channel <= tagged Valid lv_response;
		ff_xactor_response.deq;
	endrule

	rule rl_fabric_to_xactor(rg_a_channel matches tagged Valid .req);
		let lv_req = req;
		ff_xactor_request.enq(req);
		rg_a_channel <= tagged Invalid
	endrule

interface xactor_request = Put#(ff_xactor_request);
interface xactor_request = Get#(ff_xactor_response);

interface fabric_side_a = interface Ifc_Slave_fabric_side_a_channel
									method Action fabric_side_request(A_channel req);
										rg_a_channel <= tagged Valid req;
									endmethod
									method Bool fabric_side_a_ready;
										return !isValid(rg_a_channel);
									endmethod
						  endinterface;

interface fabric_side_d = interface Ifc_Slave_fabric_side_a_channel;
									method D_channel fabric_side_request;
										return validValue(rg_d_channel);
									endmethod
									method Bool fabric_side_d_valid;
										return isValid(rg_d_channel);
									endmethod
									method Action fabric_side_resp_read(Bool resp_ready);
										if(resp_ready)
											rg_d_channel <= tagged Invalid;
									endmethod
						  endinterface


endmodule

//----------------------------------------------- Slave Fabric -------------------------------------//

interface Ifc_Master_fabric_side_a_channel;
	method A_channel fabric_a_channel;
	method Bool fabric_a_channel_valid;
	method Action fabric_a_channel_ready;
endinterface

interface Ifc_Master_fabric_side_d_channel;
	method Action fabric_d_channel;
	method Bool fabric_d_channel_ready;
endinterface

interface Ifc_Master_tilelink;

	interface Put#(A_channel_control) xactor_request_control;
	interface Put#(A_channel_data) xactor_request_data;
	interface Get#(D_channel) xactor_response;

endinterface

module mkMasterFabric(Ifc_Master_Fabric);
    Reg#(A_channel_control) rg_a_channel <- mkCReg(A_channel { a_opcode : ?,
															     a_param  : ?,
																 a_size : ?,
																 a_source : ?,
																 a_address :  ? });
    Reg#(Maybe#(A_channel_data)) rg_a_channel_d <- mkCReg(tagged Invalid);
    Reg#(Maybe#(D_channel)) rg_d_channel <- mkCReg(tagged Invalid);
endmodule
