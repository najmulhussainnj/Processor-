
package tilelink_addr_generator;
	/*=== Project imports ===*/
	import Tilelink_Types ::*;
	`include "defined_parameters.bsv"
	/*=======================*/

	// This function is used by the slaves on the AXI4 bus to generate the sequential addresses in burst mode.
	// the different modes supported are :
	// FIXED: the same address is sent in all transactions. Typically used in polling modes.
	// INCR: The address is simply incremented arlen number of times from the starting address.
	// WRAP: This mode supports only 4 valid lengths: 2, 4 8 and 16 bursts. the increments happen in a wrap arouind fashion.
	Integer byte_addr = valueOf(`LANE_WIDTH);
	Integer byte_addr_bit = valueOf(TLog#(`LANE_WIDTH));
	function Tuple2#(Bit#(TAdd#(TLog#(`LANE_WIDTH),1)) ,Bit#(`PADDR)) burst_address_generator(Opcode get_type, Mask mask, Bit#(`PADDR) address, Data_size total_size);

		let leading_zeroes = countZerosMSB(mask);	
		let trailing_zeroes = countZerosLSB(mask);	
		Bit#(`PADDR) address_mask = '1;
		address_mask = address_mask << total_size;
		Bit#(TAdd#(TLog#(`LANE_WIDTH),1)) transfer_size = fromInteger(byte_addr) - (pack(leading_zeroes) + pack(trailing_zeroes));
		Bit#(TAdd#(TLog#(`LANE_WIDTH),1)) byte_address = transfer_size + pack(trailing_zeroes);
		Bit#(`PADDR) new_address = address + zeroExtend(byte_address);
		if(get_type==GetWrap)
			address = (new_address & ~address_mask) | (address & address_mask);
		else if(get_type == Get_data)
			address = new_address;
		return tuple2(transfer_size, address);
	endfunction

	function Mask burst_mask_generator(Bit#(2) transfer_size, Bit#(`PADDR) address);
		Bit#(4) byte_transfer_size = 4'b0001;
		byte_transfer_size = byte_transfer_size << transfer_size;
		Int#(`LANE_WIDTH) byte_mask = unpack({1'b1,0}); 
		byte_mask = byte_mask >> byte_transfer_size;
		Bit#(`LANE_WIDTH) bits_byte_mask; 
		bits_byte_mask = reverseBits(pack(byte_mask));
		bits_byte_mask = bits_byte_mask << address[2:0];
		return bits_byte_mask;
	endfunction

endpackage
