package QuadMem;
	import defined_types::*;
	import BUtils::*;
	`include "defined_parameters.bsv"

	interface Ifc_QuadMem;
		method Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) response_portA;
		method Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) response_portB;
		method Action write_portA(Bit#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE)) we, Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) data);
		method Action write_portB(Bit#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE)) we, Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) data);
	endinterface

	(*synthesize*)
	module mkQuadMem(Ifc_QuadMem);
		Reg#(Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE))) data_reg[3] <-mkCReg(3,0);

		method Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) response_portA;
			return data_reg[0];
		endmethod
		method Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) response_portB;
			return data_reg[1];
		endmethod
		method Action write_portA(Bit#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE)) we, Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) data);
         Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) mask=0;
         for(Integer i=0;i<32;i=i+1)begin
            Bit#(8) ex_we=duplicate(we[i]);
            mask[(i*8)+7:i*8]=ex_we;
         end
         Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) x = mask& data;
         Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) y = ~mask& data_reg[0];
         data_reg[0]<=x|y;
      endmethod
		method Action write_portB(Bit#(TMul#(`DCACHE_BLOCK_SIZE,`DCACHE_WORD_SIZE)) we, Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) data);
         Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) mask=0;
         for(Integer i=0;i<32;i=i+1)begin
            Bit#(8) ex_we=duplicate(we[i]);
            mask[(i*8)+7:i*8]=ex_we;
         end
         Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) x = mask& data;
         Bit#(TMul#(TMul#(8,`DCACHE_WORD_SIZE),`DCACHE_BLOCK_SIZE)) y = ~mask& data_reg[1];
         data_reg[1]<=x|y;
      endmethod
			
	endmodule
endpackage
