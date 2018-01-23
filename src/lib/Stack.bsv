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
