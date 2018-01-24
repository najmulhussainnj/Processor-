package mem_config1;
	import BRAMCore::*;
	import defined_types::*;
	`include "defined_parameters.bsv"
	import Assert::*;
	import DReg::*;
	interface Ifc_dcache_data;
		method Action read_request(Bit#(TLog#(`DCACHE_SETS)) address);
		method Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) read_response;
		method Action write_request(Bit#(TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE))we, Bit#(TLog#(`DCACHE_SETS)) address, Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) data);
	endinterface


	module mkdcache_data(Ifc_dcache_data);
		BRAM_DUAL_PORT_BE#(Bit#(TLog#(`DCACHE_SETS)),Bit#(128),16) dataa <-mkBRAMCore2BE(`DCACHE_SETS,False);
		BRAM_DUAL_PORT_BE#(Bit#(TLog#(`DCACHE_SETS)),Bit#(128),16) datab <-mkBRAMCore2BE(`DCACHE_SETS,False);
		
		Wire#(Bit#(TLog#(`DCACHE_SETS))) read_address <-mkWire();
		Wire#(Bit#(TLog#(`DCACHE_SETS))) write_address<-mkWire();
		let set_bits=valueOf(TLog#(`DCACHE_SETS));			// number of bits to select a set from the cache. = 

		
		rule print_address;
			`ifdef verbose $display("\tASSERT: DATA read_address: %d write_address: %d",read_address,write_address); `endif
			dynamicAssert(read_address!=write_address,"ASSERT: DATA read and write address are the same");
		endrule


		method Action read_request(Bit#(TLog#(`DCACHE_SETS)) address);
			dataa.a.put(0,address,?);
			datab.a.put(0,address,?);
			read_address<=address[set_bits-1:0];
		endmethod
		method Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) read_response;
			return {dataa.a.read,datab.a.read};
		endmethod
		method Action write_request(Bit#(TMul#(`DCACHE_WORD_SIZE,`DCACHE_BLOCK_SIZE))we, Bit#(TLog#(`DCACHE_SETS)) address, Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) data);
			dataa.b.put(we[31:16],address,data[255:128]);
			datab.b.put(we[15:0],address,data[127:0]);
			if(we!=0)
				write_address<=address[set_bits-1:0];
		endmethod
	endmodule

	interface Ifc_dcache_tag;
		method Action read_request(Bit#(TLog#(`DCACHE_SETS)) address);
		method Bit#(TAdd#(2,`DCACHE_TAG_BITS)) read_response;
		method Action write_request(Bool we, Bit#(TLog#(`DCACHE_SETS)) address, Bit#(TAdd#(2,`DCACHE_TAG_BITS)) data);
	endinterface

	module mkdcache_tag(Ifc_dcache_tag);
		let byte_bits=valueOf(TLog#(`DCACHE_WORD_SIZE));	// number of bits to select a byte within a word. = 2
		let word_bits=valueOf(TLog#(`DCACHE_BLOCK_SIZE));	// number of bits to select a word within a block. = 4
		let set_bits=valueOf(TLog#(`DCACHE_SETS));			// number of bits to select a set from the cache. = 

		BRAM_DUAL_PORT#(Bit#(TLog#(TDiv#(`DCACHE_SETS,2))),Bit#(TAdd#(`DCACHE_TAG_BITS,2))) taga<-mkBRAMCore2(`DCACHE_SETS/2,False) ;
		BRAM_DUAL_PORT#(Bit#(TLog#(TDiv#(`DCACHE_SETS,2))),Bit#(TAdd#(`DCACHE_TAG_BITS,2))) tagb<-mkBRAMCore2(`DCACHE_SETS/2,False) ;
		Reg#(Bit#(TLog#(`DCACHE_SETS))) rg_addr <-mkReg(0);

		Wire#(Bit#(TLog#(`DCACHE_SETS))) read_address <-mkWire();
		Wire#(Bit#(TLog#(`DCACHE_SETS))) write_address<-mkWire();
		
		rule print_address;
			`ifdef verbose $display("\tASSERT: read_address: %d write_address: %d",read_address,write_address); `endif
			dynamicAssert(read_address!=write_address,"ASSERT: Tag read and write address are the same");
		endrule


		method Action read_request(Bit#(TLog#(`DCACHE_SETS)) address);
			if(address[set_bits-1]==1)
				tagb.a.put(False,address[set_bits-2:0],?);
			else
				taga.a.put(False,address[set_bits-2:0],?);
			rg_addr<=address;
			read_address<=address[set_bits-1:0];
		endmethod
		method Bit#(TAdd#(2,`DCACHE_TAG_BITS)) read_response;
			if(rg_addr[set_bits-1]==1)
				return tagb.a.read;
			else
				return taga.a.read;
		endmethod
		method Action write_request(Bool we, Bit#(TLog#(`DCACHE_SETS)) address, Bit#(TAdd#(2,`DCACHE_TAG_BITS)) data);
			if(address[set_bits-1]==1)
				tagb.b.put(we,address[set_bits-2:0],data);
			else
				taga.b.put(we,address[set_bits-2:0],data);
			if(we)
				write_address<=address[set_bits-1:0];
		endmethod
	endmodule
endpackage
