/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted 
provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions 
   and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of 
   conditions and the following disclaimer in the documentation and/or other materials provided 
   with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or 
   promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR 
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER 
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
-------------------------------------------------------------------------------------------------

Code inpired by the pwm module at: https://github.com/freecores/pwm

*/
package pwm;
  `define PWMWIDTH 32
  /*=== Project imports ==*/
  /*======================*/
  /*== Package imports ==*/
  import defined_types::*;
  `include "defined_parameters.bsv"
  import ConcatReg::*;
  /*======================*/
  interface UserInterface;
    method ActionValue#(Bool) write(Bit#(`PADDR) addr, Bit#(`Reg_width) data);
    method Tuple2#(Bool, Bit#(`Reg_width)) read(Bit#(`PADDR) addr);
  endinterface

  module mkpwm(UserInterface);

    Reg#(Bit#(`PWMWIDTH)) period <- mkReg(0);
    Reg#(Bit#(`PWMWIDTH)) duty_cycle <- mkReg(0);
    
    /*== Control registers ==*/
    Reg#(Bit#(1)) clock_selector <- mkReg(0);     // bit-0
    Reg#(Bit#(1)) pwm_enable <- mkReg(0);         // bit-1
    Reg#(Bit#(1)) pwm_start  <- mkReg(0);         // bit-2
    Reg#(Bit#(1)) continous_once <- mkReg(0);     // bit-3
    Reg#(Bit#(1)) pwm_output_enable <- mkReg(0);  // bit-4
    Reg#(Bit#(1)) interrupt <- mkReg(0);          // bit-5
    Reg#(Bit#(1)) reset_counter <- mkReg(0);      // bit-7
    Reg#(Bit#(8)) control = concatReg8(reset_counter, readOnlyReg(0), readOnlyReg(interrupt), 
                                       pwm_output_enable, continous_once, pwm_start, pwm_enable, 
                                       clock_selector);

    method ActionValue#(Bool) write(Bit#(`PADDR) addr, Bit#(`Reg_width) data);
      Bool err = False;
      case(addr[3:2])
        0: period <= truncate(data);
        1: duty_cycle <= truncate(data);
        2: controler <= truncate(data);
        default: err = True;
      endcase
      return err;
    endmethod

    method Tuple2#(Bool, Bit#(`Reg_width)) read(Bit#(`PADDR) addr);
      Bool err = False;
      Bit#(`Reg_width) data;
      case(addr[3:2])
        0: data = zeroExtend(period);
        1: data = zeroExtend(duty_cycle);
        2: data = zeroExtend(controler);
        default: err = True;
      endcase
      return tuple2(err,data);
    endmethod

  endmodule
endpackage
