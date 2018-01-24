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
