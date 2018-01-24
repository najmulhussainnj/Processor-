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
package Stack;
	import defined_types::*;
	import RegFile::*;
	`include "defined_parameters.bsv"
	interface Ifc_Stack;
		method Action push(Bit#(`VADDR) addr);
		method ActionValue#(Bit#(`VADDR)) top;
		method Bool empty;
		method Action flush;
	endinterface

//	(*synthesize*)
	module mkStack(Ifc_Stack);
		Reg#(Bit#(TLog#(`RAS_DEPTH))) top_index[2] <-mkCReg(2,0);
		RegFile#(Bit#(TLog#(`RAS_DEPTH)),Bit#(`VADDR)) array_reg <-mkRegFileWCF(0,fromInteger(`RAS_DEPTH-1));
		method ActionValue#(Bit#(`VADDR)) top;
			top_index[0]<=top_index[0]-1;
			return array_reg.sub(top_index[0]-1);
		endmethod
		method Action push(Bit#(`VADDR) addr);
			array_reg.upd(top_index[1],addr);
			top_index[1]<=top_index[1]+1;
		endmethod
		method Bool empty;
			return (top_index[0]==0);
		endmethod
	endmodule
endpackage
