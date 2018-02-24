package axi_addr_generator;
	import defined_types::*;
	`include "defined_parameters.bsv"
	function Bit#(`PADDR) burst_address_generator(Bit#(8) arlen, Bit#(3) arsize, Bit#(2) arburst, Bit#(`PADDR) address);
	//	Bit#(3) wrap_size;
	//	case(arlen)
	//		3: wrap_size= 1;
	//		7: wrap_size= 2;
	//		15: wrap_size=3;
	//		default:wrap_size=0;
	//	endcase

	//	if(arburst==0) // FIXED
	//		return address;
	//	else if(arburst==1)begin // INCR
	//		return address+ (('b1)<<arsize);
	//	end
	//	else begin // WRAP
	//		let new_addr=address;
	//		new_addr[arsize+wrap_size:arsize]=address[arsize+wrap_size:arsize]+1;
	//		return new_addr;
	//	end
	if(arburst==0) // FIXED
		return address;
	else if(arburst==1)begin // INCR
		return address+ (('b1)<<arsize);
	end
	else begin // WRAP
		let new_addr=address;
		case (arlen) 
			1: new_addr[arsize]=~address[arsize];
			3: begin 
				if(arsize==0)
					new_addr[1:0]=new_addr[1:0]+1; // +1
				else if(arsize==1)
					new_addr[2:1]=new_addr[2:1]+1; // +2
				else if(arsize==2)
					new_addr[3:2]=new_addr[3:2]+1; // +4
				else if(arsize==3)
					new_addr[4:3]=new_addr[4:3]+1; // +8
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
endpackage
