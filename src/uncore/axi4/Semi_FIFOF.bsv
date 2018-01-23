// Copyright (c) 2017 Bluespec, Inc.  All Rights Reserved

package Semi_FIFOF;

// ================================================================
// Separate interfaces for input-side and output-side of FIFOF.
// Conversion functions to these, from FIFOF interfaces.

// ================================================================
// BSV library imports

import FIFOF       :: *;
import Connectable :: *;

// ================================================================
// Semi-FIFOF interfaces

interface FIFOF_I #(type t);
   method Action  enq (t x);
   method Bool    notFull ();
endinterface

interface FIFOF_O #(type t);
   method t       first ();
   method Action  deq ();
   method Bool    notEmpty ();
endinterface

// ================================================================
// Converters from FIFOF

function FIFOF_I #(t) to_FIFOF_I (FIFOF #(t) f);
   return interface FIFOF_I;
	     method enq (x) = f.enq (x);
	     method notFull = f.notFull;
	  endinterface;
endfunction

function FIFOF_O #(t) to_FIFOF_O (FIFOF #(t) f);
   return interface FIFOF_O;
	     method first    = f.first;
	     method deq      = f.deq;
	     method notEmpty = f.notEmpty;
	  endinterface;
endfunction

// ================================================================
// Connections

// ----------------
// FIFOF_O to a FIFOF_I

instance Connectable #(FIFOF_O #(t), FIFOF_I #(t));
   module mkConnection #(FIFOF_O #(t) fo, FIFOF_I #(t) fi) (Empty);
      rule rl_connect;
	 fi.enq (fo.first);
	 fo.deq;
      endrule
   endmodule
endinstance

// ----------------
// FIFOF_O to a FIFOF

instance Connectable #(FIFOF_O #(t), FIFOF #(t));
   module mkConnection #(FIFOF_O #(t) fo, FIFOF #(t) fi) (Empty);
      rule rl_connect;
	 fi.enq (fo.first);
	 fo.deq;
      endrule
   endmodule
endinstance

// ----------------
// FIFOF to a FIFOF_I

instance Connectable #(FIFOF #(t), FIFOF_I #(t));
   module mkConnection #(FIFOF #(t) fo, FIFOF_I #(t) fi) (Empty);
      rule rl_connect;
	 fi.enq (fo.first);
	 fo.deq;
      endrule
   endmodule
endinstance

// ================================================================
// Convenience function combining first/enq

function ActionValue #(t) pop_o (FIFOF_O #(t) f);
   actionvalue
      f.deq;
      return f.first;
   endactionvalue
endfunction

// ================================================================

endpackage
