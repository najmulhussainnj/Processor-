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
  import Clocks::*;
  /*======================*/
  /*== Package imports ==*/
  import defined_types::*;
  `include "defined_parameters.bsv"
  import ConcatReg::*;
  /*======================*/

  // =========================== Clock divider module ================ //
  interface ClockDiv;
    interface Clock slowclock;
    method Action divisor(Bit#(`PWMWIDTH) in);
  endinterface

  (*synthesize*)
  module mkClockDiv(ClockDiv);
    let defclock <- exposeCurrentClock;
    Reg#(Bit#(1)) clk <- mkReg(0);
    Reg#(Bit#(`PWMWIDTH)) rg_divisor <- mkReg(0);
    Reg#(Bit#(`PWMWIDTH)) rg_counter <- mkReg(0);
    MakeClockIfc#(Bit#(1)) new_clock <- mkUngatedClock(0);
    MuxClkIfc clock_selector <- mkUngatedClockMux(new_clock.new_clk,defclock);
    Bool clockmux_sel = rg_divisor!=0;
    rule increment_counter;
      if(rg_divisor!=0 && rg_counter >= rg_divisor)begin
        rg_counter <= 0;
        clk <= ~ clk;
      end
      else
        rg_counter <= rg_counter + 1;
    endrule

    rule generate_clock;
      new_clock.setClockValue(clk);
    endrule

    rule select_clock;
      clock_selector.select(clockmux_sel);
    endrule

    method Action divisor(Bit#(`PWMWIDTH) in);
      rg_divisor <= in != 0 ? in - 1 : 0;
    endmethod

    interface slowclock=clock_selector.clock_out;
  endmodule
  // ============================================================== //
  
  interface UserInterface;
    method ActionValue#(Bool) write(Bit#(`PADDR) addr, Bit#(`Reg_width) data);
    method Tuple2#(Bool, Bit#(`Reg_width)) read(Bit#(`PADDR) addr);
  endinterface

  interface PWMIO;
    method Bit#(1) pwm_o;
  endinterface

  interface PWM;
    interface UserInterface user;
    interface PWMIO io;
  endinterface


  (*synthesize*)
  module mkpwm#(Clock ext_clock)(PWM);

    let bus_clock <- exposeCurrentClock;
    let bus_reset <- exposeCurrentReset;
    

    Reg#(Bit#(`PWMWIDTH)) period <- mkReg(0);
    Reg#(Bit#(`PWMWIDTH)) duty_cycle <- mkReg(0);
    Reg#(Bit#(`PWMWIDTH)) clock_divisor <- mkReg(0);
    // =========== Control registers ================== //
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
    // ================================================ //
    MakeResetIfc control_reset <- mkReset(1,False, bus_clock);
    rule generate_reset;
      if(reset_counter==1)
        control_reset.assertReset;
    endrule
    Reset overall_reset <- mkResetEither(bus_reset,control_reset.new_rst);

    // == Select between bus clock or external clock == //
    MuxClkIfc clock_selection <- mkUngatedClockMux(ext_clock,bus_clock);
    Reset async_reset <- mkAsyncResetFromCR(0,clock_selection.clock_out);
    rule select_busclk_extclk;
      clock_selection.select(clock_selector==1);
    endrule
    // The following register is required to transfer the divisor value from bus_clock to 
    // external clock domain. This is necessary if the clock divider needs to operate on the
    // external clock. In this case, the divisor value should also come from the same clock domain.
    Reg#(Bit#(`PWMWIDTH)) clock_divisor_sync <- mkSyncRegFromCC(0, clock_selection.clock_out); 
    rule transfer_data_from_clock_domains;
      clock_divisor_sync <= clock_divisor;
    endrule
    // ================================================ //
    
    // == Downclock using the divisor register == //
    ClockDiv clock_divider <- mkClockDiv(clocked_by clock_selection.clock_out, 
                                         reset_by async_reset);
    let downclock = clock_divider.slowclock; 
    Reset downreset <- mkAsyncReset(0,overall_reset,downclock);
    Reg#(Bool) dummy <-mkReg(False,clocked_by downclock,reset_by downreset);
    rule dummyrule;
      dummy<=!dummy;
    endrule
    rule generate_slow_clock;
      clock_divider.divisor(clock_divisor_sync);
    endrule
    // ========================================== //

    // ======= Actual Counter and PWM signal generation ======== //
    Reg#(Bit#(1)) pwm_output <- mkReg(0);
    Reg#(Bit#(`PWMWIDTH)) rg_counter <-mkReg(0); 
    let temp = period==0?0:period-1;
    Wire#(Bool) clear_int <- mkDWire(False);

    rule generate_interrupt_in_timer_mode;
      if(pwm_enable==0)
        interrupt <= pwm_output;
      else if(clear_int)
        interrupt <= 0;
      else
        interrupt <= 0;
    endrule

    rule compare_and_generate_pwm(pwm_start==1);
      let cntr = rg_counter+1;
      if(pwm_enable==1)begin // PWM mode enabled
        if(rg_counter >= temp)
          rg_counter <= 0;
        else 
          rg_counter <= cntr;
        if(rg_counter < duty_cycle)
          pwm_output <= 1;
        else
          pwm_output <= 0;
      end
      else begin  // Timer mode enabled.
        if(continous_once==0) begin // One time mode.
          if(rg_counter >= temp)begin
              pwm_output <= 1;
          end
          else
            rg_counter <= cntr;
        end
        else begin // Continous mode.
          if(rg_counter >= temp)begin
            pwm_output <= 1;
            rg_counter <= 0;
          end
          else begin
            rg_counter <= cntr;
          end
        end
      end
    endrule

    // ========================================================= //
    interface user = interface UserInterface
      method ActionValue#(Bool) write(Bit#(`PADDR) addr, Bit#(`Reg_width) data);
        Bool err = False;
        case(addr[4:2])
          0: period <= truncate(data);
          1: duty_cycle <= truncate(data);
          2: begin control <= truncate(data); if(data[7]==1) clear_int <= True; end
          3: clock_divisor <= truncate(data);
          default: err = True;
        endcase
        return err;
      endmethod

      method Tuple2#(Bool, Bit#(`Reg_width)) read(Bit#(`PADDR) addr);
        Bool err = False;
        Bit#(`Reg_width) data;
        case(addr[4:2])
          0: data = zeroExtend(period);
          1: data = zeroExtend(duty_cycle);
          2: data = zeroExtend(control);
          3: data =  zeroExtend(clock_divisor);
          default: begin err = True; data = 0; end
        endcase
        return tuple2(err,data);
      endmethod
    endinterface;
    interface io = interface PWMIO
      method pwm_o=pwm_output_enable==1?pwm_output:0;
    endinterface;
  endmodule

  (*synthesize*)
  module mkTb(Empty);
    let clk <- exposeCurrentClock;
    PWM pwm <- mkpwm(clk);
    Reg#(Bit#(5)) rg_state <- mkReg(0);
    
    rule state1(rg_state==0);
      rg_state<=1;
      let x <- pwm.user.write(0,'d4);
    endrule
    rule state2(rg_state==1);
      rg_state<=2;
      let x <- pwm.user.write('d4,'d2);
    endrule
    rule state3(rg_state==2);
      rg_state<=3;
      let x <- pwm.user.write('hc,'d4);
    endrule
    rule state4(rg_state==3);
      rg_state<=4;
      let x <- pwm.user.write(8,'b0001_0110);
    endrule
  endmodule
endpackage
