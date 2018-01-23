/*
Authors     : Vinod.G, Arjun Menon
Email       : g.vinod1993@gmail.com, c.arjunmenon@gmail.com
Last update : 27th November 2017
See LICENSE for more details
Description:
TODO
*/
package fpu;
/*==== Project imports ==== */
`include "defined_parameters.bsv"	
import defined_types::*;
import fpu_compare_min_max::*;						
import fpu_int_to_sp::*;
import fpu_int_to_dp::*;
import fpu_sign_injection::*;	
import fpu_divider::*;
import fpu_sqrt::*;
import fpu_sp_to_int::*;
import fpu_dp_to_int::*;
import fpu_fm_add_sub::*;
import fpu_convert_sp_dp::*;
import fpu_fclass::*;
/*========================= */
/*===== Package imports ==== */
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;
import DReg::*;
import UniqueWrappers::*;
import SpecialFIFOs::*;
import Clocks::*;
/*========================= */

interface Ifc_fpu;							//interface to module mk_fpu
	method Action _start(Bit#(`FLEN) operand1, Bit#(`FLEN) operand2, Bit#(`FLEN) operand3, Bit#(4) opcode, Bit#(7) funct7, Bit#(3) funct3, Bit#(2) imm,  Bit#(3) fsr, Bool issp); 
	method ActionValue#(Floating_output#(`FLEN)) get_result;
	method Action flush;
endinterface

typedef struct{
        Bit#(`FLEN) operand1;
        Bit#(`FLEN) operand2;
        Bit#(`FLEN) operand3;
        Bit#(4) opcode;
        Bit#(7) funct7;
        Bit#(3) funct3;
        Bit#(2) imm;
        Bit#(3) fsr;
        Bool    issp;
    }Input_Packet deriving (Bits,Eq);

(*synthesize*)
module mkfpu(Ifc_fpu);
  // ============================================
  //  Decode and Maintenance Registers
  // ============================================
	FIFO# (Floating_output#(`FLEN)) ff_result <- mkFIFO1;
    FIFO# (Input_Packet) ff_input <- mkFIFO1;
	Wire#(Bool) wr_flush<-mkDWire(False);
  // =============================================
 
  // ==============================================
  //  Module Instantiations
  // ==============================================
     
     `ifdef fpu_hierarchical
         Ifc_fpu_compare_min_max32                  inst_fpu_compare_min_max  <- mkfpu_compare_min_max32();
         Ifc_fpu_sign_injection32                   inst_spfpu_sign_injection <- mkfpu_sign_injection32();
         Ifc_fpu_divider32                          inst_spfpu_divider        <- mkfpu_divider32();
         Ifc_fpu_sqrt32                             inst_spfpu_sqrt           <- mkfpu_sqrt32();
         Ifc_fpu_fm_add_sub32                       inst_spfm_add_sub         <- mkfpu_fm_add_sub32();
         Ifc_fpu_fclass32                           inst_spfpu_fclass         <- mkfpu_fclass32();
     
         Ifc_fpu_compare_min_max64                  inst_dpfpu_compare_min_max <- mkfpu_compare_min_max64();
         Ifc_fpu_sign_injection64                   inst_dpfpu_sign_injection  <- mkfpu_sign_injection64();
         Ifc_fpu_divider64                          inst_dpfpu_divider         <- mkfpu_divider64();
         Ifc_fpu_sqrt64                             inst_dpfpu_sqrt            <- mkfpu_sqrt64();
         Ifc_fpu_fm_add_sub64                       inst_dpfm_add_sub          <- mkfpu_fm_add_sub64();
         Ifc_fpu_fclass64                           inst_dpfpu_fclass          <- mkfpu_fclass64();
     
     `else
         Ifc_fpu_compare_min_max#(32,23,8)		    inst_fpu_compare_min_max  <- mkfpu_compare_min_max(); // No Flush
         Ifc_fpu_sign_injection#(32,23,8)		    inst_spfpu_sign_injection <- mkfpu_sign_injection(); // No Flush 
         Ifc_fpu_divider#(32,23,8)				    inst_spfpu_divider        <- mkfpu_divider();
         Ifc_fpu_sqrt#(32,23,8)					    inst_spfpu_sqrt           <- mkfpu_sqrt();
         Ifc_fpu_fm_add_sub#(32,23,8)		        inst_spfm_add_sub         <- mkfpu_fm_add_sub();
         Ifc_fpu_fclass#(32,23,8)					inst_spfpu_fclass         <- mkfpu_fclass();						// No Flush
     
         Ifc_fpu_compare_min_max#(64,52,11)	        inst_dpfpu_compare_min_max <- mkfpu_compare_min_max();   // No Flush
         Ifc_fpu_sign_injection#(64,52,11)		    inst_dpfpu_sign_injection  <- mkfpu_sign_injection();	 // No Flush
         Ifc_fpu_divider#(64,52,11)				    inst_dpfpu_divider         <- mkfpu_divider();						
         Ifc_fpu_sqrt#(64,52,11)			        inst_dpfpu_sqrt            <- mkfpu_sqrt();
         Ifc_fpu_fm_add_sub#(64,52,11)		        inst_dpfm_add_sub          <- mkfpu_fm_add_sub();
         Ifc_fpu_fclass#(64,52,11)				    inst_dpfpu_fclass          <- mkfpu_fclass();		     // No Flush
     `endif

         Ifc_fpu_sp_to_int						    inst_spfp_to_int   <- mkfpu_sp_to_int();						// No Flush
         Ifc_fpu_convert_sp_dp						inst_spfpu_cnvt    <- mkfpu_convert_sp_dp();				// No Flush 
         Ifc_fpu_int_to_sp							inst_fpu_int_to_fp <- mkfpu_int_to_sp();				  // No Flush
         
         Ifc_fpu_dp_to_int						    inst_dpfp_to_int     <- mkfpu_dp_to_int();						// No Flush
         Ifc_fpu_convert_dp_sp						inst_dpfpu_cnvt      <- mkfpu_convert_dp_sp();				// No Flush
         Ifc_fpu_int_to_dp							inst_dpfpu_int_to_fp <- mkfpu_int_to_dp();				 // No Flush 


// ==============================================

// ==============================================
//  Function definitions
// ==============================================
    function Tuple3#(Bit#(5), Bit#(5), Bit#(5)) condFlags (Tuple2#(Bit#(m), Bit#(e)) x, Tuple2#(Bit#(m), Bit#(e)) y, Tuple2#(Bit#(m),Bit#(e)) z);
        let s = valueOf(m);
        let mantissa1  = tpl_1(x);
        let exponent1 = tpl_2(x);
        let mantissa2  = tpl_1(y);
        let exponent2 = tpl_2(y);
        let mantissa3  = tpl_1(z);
        let exponent3 = tpl_2(z);
        bit man10 = |mantissa1; bit man20 = |mantissa2; bit man30 = |mantissa3;
        bit exp10 = |exponent1; bit exp20 = |exponent2; bit exp30 = |exponent3;
        bit man11 = &mantissa1; bit man21 = &mantissa2; bit man31 = &mantissa3;
        bit exp11 = &exponent1; bit exp21 = &exponent2; bit exp31 = &exponent3;
        Bit#(5) flags1, flags2,flags3;
        Bool expZ1 = (exp10 == 0);
        Bool manZ1 = (man10  == 0);
        Bool expO1 = (exp11 == '1);
        Bool manO1 = (man11  == '1);
        Bool topB1 = (mantissa1[s-1] == 1);
        Bool expZ2 = (exp20 == 0);
        Bool manZ2 = (man20  == 0);
        Bool expO2 = (exp21 == '1);
        Bool manO2 = (man21  == '1);
        Bool topB2 = (mantissa2[s-1] == 1 && man20 ==1);
        Bool expZ3 = (exp30 == 0);
        Bool manZ3 = (man30  == 0);
        Bool expO3 = (exp31 == 1);
        Bool manO3 = (man31 == 1);
        Bool topB3 = (mantissa3[s-1] == 1 && man30 ==1);
        flags1 = {pack(expZ1 && !manZ1),pack(manZ1 && expZ1),pack(expO1 && topB1),pack(expO1 && manZ1),pack(expO1 && !topB1 && !manZ1)}; //Denormal, isZero, QNaN, Infinity, SNaN
        flags2 = {pack(expZ2 && !manZ2),pack(manZ2 && expZ2),pack(expO2 && topB2),pack(expO2 && manZ2),pack(expO2 && !topB2 && !manZ2)}; //Denormal, isZero, QNaN, Infinity, SNaN
        flags3 = {pack(expZ3 && !manZ3),pack(manZ3 && expZ3),pack(expO3 && topB3),pack(expO3 && manZ3),pack(expO3 && !topB3 && !manZ3)}; //Denormal, isZero, QNaN, Infinity, SNaN
        return tuple3(flags1,flags2,flags3);
    endfunction

    function Tuple3#(Bit#(m),Bit#(m), Bit#(m)) getMantissa (Bit#(n) op1, Bit#(n) op2, Bit#(n) op3)
        provisos(Add#(TAdd#(m,1),e,n),
                 Add#(7,a__,e)
                );
        let expo = valueOf(e);
        let man  = valueOf(m);
        return tuple3(op1[man-1:0],op2[man-1:0],op3[man-1:0]);
    endfunction

    function Tuple3#(Bit#(e), Bit#(e), Bit#(e)) getExp (Bit#(n) op1, Bit#(n) op2, Bit#(n) op3)
        provisos(Add#(TAdd#(m,1),e,n),
                 Add#(7,a__,e)
                );
        let inp = valueOf(n);
        let man  = valueOf(m);
        return tuple3(op1[inp-2:man], op2[inp-2:man], op3[inp-2:man]);
    endfunction

    function Bool isNaNBox(Bit#(64) op);
        return (&(op[63:32])==1);
    endfunction

    function Tuple3#(Bit#(32),Bit#(32),Bit#(32)) setCanNaN (Bit#(64) op1, Bit#(64) op2, Bit#(64) op3);
        return tuple3(isNaNBox(op1)? truncate(op1) : 32'h7fc00000, isNaNBox(op2)? truncate(op2) : 32'h7fc00000, isNaNBox(op3)? truncate(op3) : 32'h7fc00000);
    endfunction
   
// ================================================
//  Function Wrappers
// ================================================
    Wrapper3#(Tuple2#(Bit#(23), Bit#(8)),Tuple2#(Bit#(23), Bit#(8)), Tuple2#(Bit#(23), Bit#(8)),  Tuple3#(Bit#(5),Bit#(5),Bit#(5)))    condFlags32     <- mkUniqueWrapper3(condFlags);
    Wrapper3#(Tuple2#(Bit#(52), Bit#(11)),Tuple2#(Bit#(52), Bit#(11)),Tuple2#(Bit#(52), Bit#(11)), Tuple3#(Bit#(5),Bit#(5),Bit#(5)))   condFlags64     <- mkUniqueWrapper3(condFlags);
    Wrapper3#(Bit#(32),Bit#(32),Bit#(32),Tuple3#(Bit#(23),Bit#(23),Bit#(23)))                                                          getMant32       <- mkUniqueWrapper3(getMantissa);
    Wrapper3#(Bit#(32),Bit#(32),Bit#(32),Tuple3#(Bit#(8),Bit#(8),Bit#(8)))                                                             getExp32        <- mkUniqueWrapper3(getExp);
    Wrapper3#(Bit#(64),Bit#(64),Bit#(64),Tuple3#(Bit#(52),Bit#(52),Bit#(52)))                                                          getMant64       <- mkUniqueWrapper3(getMantissa);
    Wrapper3#(Bit#(64),Bit#(64),Bit#(64),Tuple3#(Bit#(11),Bit#(11),Bit#(11)))                                                          getExp64        <- mkUniqueWrapper3(getExp);
    Wrapper3#(Bit#(64),Bit#(64),Bit#(64),Tuple3#(Bit#(32),Bit#(32),Bit#(32)))                                                          setCanonicalNaN <- mkUniqueWrapper3(setCanNaN);
	Reg#(Bool) rg_multicycle_op <-mkReg(False);


 (*mutually_exclusive="rl_get_output_from_spfpu_divider, rl_get_output_from_dpfpu_divider,rl_get_output_from_spfpu_sqrt, rl_get_output_from_dpfpu_sqrt,rl_get_output_from_fm_add_sub,rl_get_output_from_dpfm_add_sub"*)


 rule start_stage;
///		Bool issp = (funct7[0] == 0);     
        let input_packet = ff_input.first;
        Bit#(`FLEN) operand1 = input_packet.operand1;
        Bit#(`FLEN) operand2 = input_packet.operand2;
        Bit#(`FLEN) operand3 = input_packet.operand3;
        Bit#(4) opcode       = input_packet.opcode;
        Bit#(7) funct7       = input_packet.funct7;
        Bit#(3) funct3       = input_packet.funct3;
        Bit#(2) imm          = input_packet.imm;
        Bit#(3) fsr          = input_packet.fsr;
        Bool    issp         = input_packet.issp;
        ff_input.deq;
		funct3 = (funct3 == 'b111) ? fsr : funct3;
		if(((funct7[6:2]==`FCMP_f5) || funct7[6:2] == `FMMAX_f5) && opcode == `FP_OPCODE)begin // compare min max 
			if(issp) begin
            let {op1,op2,op3} <- setCanonicalNaN.func(operand1,operand2,'1);
				let {man1,man2,man3}   <- getMant32.func(op1, op2,0);
            let {exp1,exp2,exp3}   <- getExp32.func(op1, op2,0);
            let {flags1,flags2,flags3} <- condFlags32.func(tuple2(man1,exp1),tuple2(man2,exp2),tuple2(0,0));
            let sign1 = op1[31];
            let sign2 = op2[31];
            let x<-inst_fpu_compare_min_max._start(op1,op2,funct3,funct7[2],tuple2(flags1,flags2));  
				Floating_output#(`Reg_width) y=?;
				`ifdef dpfpu
					y.final_result=funct7[2]==0?zeroExtend(x.final_result):{'1,x.final_result};
				`else
					y.final_result=x.final_result;
				`endif
				y.fflags=x.fflags;
				ff_result.enq(y);
			end
			else begin
            let {man3,man4,man5} <- getMant64.func(operand1, operand2,0);
            let {exp3,exp4,exp5} <- getExp64.func(operand1,operand2,0);
            let {f1,f2,f3}       <- condFlags64.func(tuple2(man3,exp3),tuple2(man4,exp4),tuple2(0,0));
            let sign3 = operand1[63];
            let sign4 = operand2[63];
            let x<-inst_dpfpu_compare_min_max._start(operand1,operand2,funct3,funct7[2],tuple2(f1,f2));
				ff_result.enq(x);
			end
   	  `ifdef verbose $display($time,"\tGiving inputs to fpu_compare_min_max %h operand2 %h funct7 : %h",operand1, operand2,funct7); `endif
		end
		else if((funct7[6:2]==`FCVT_F_I_f5) && opcode == `FP_OPCODE) begin
			if(issp)begin
				let x <-inst_fpu_int_to_fp._start(operand1, imm[0],imm[1], funct3);
				Floating_output#(`Reg_width) y=?;
				`ifdef dpfpu
					y.final_result={'1,x.final_result};
				`else
					y.final_result=x.final_result;
				`endif
				y.fflags=x.fflags;
				ff_result.enq(y);
			end
			else begin
				let x<-inst_dpfpu_int_to_fp._start(operand1,imm[0],imm[1],funct3);
				ff_result.enq(x);
			end
   	  `ifdef verbose $display($time,"\tGiving inputs to fpu_int_to_fp %h operand2[0] %h operand2[1] : %h",operand1, operand2[0],operand2[1]); `endif
		end
		else if((funct7[6:2] == `FSGNJN_f5) && opcode == `FP_OPCODE)begin
			if(issp)begin
				let {op1,op2,op3} <- setCanonicalNaN.func(operand1,operand2,'1);
			   let x <-inst_spfpu_sign_injection._start(op1, op2, funct3);
				Floating_output#(`Reg_width) y=?;
				`ifdef dpfpu
					y.final_result={'1,x.final_result};
				`else
					y.final_result=x.final_result;
				`endif
				y.fflags=x.fflags;
				ff_result.enq(y);
         end
         else begin
				let x<-inst_dpfpu_sign_injection._start(operand1,operand2,funct3);
				ff_result.enq(x);
			end
			`ifdef verbose $display($time,"\tGiving inputs to the fpu sign injection"); `endif
		end
		else if((funct7[6:2] == `FCVT_I_F_f5) && opcode == `FP_OPCODE) begin
			if(issp) begin
				let {op1,op2,op3} <- setCanonicalNaN.func(operand1,'0,'0);
				let {man1,man2,man3} <- getMant32.func(op1, 0,0);
				let {exp1,exp2,exp3} <- getExp32.func(op1, 0,0);
				let {flags1,flags2,flags3} <- condFlags32.func(tuple2(man1,exp1),tuple2(0,0),tuple2(0,0));
				let x <- inst_spfp_to_int._start(op1[31],exp1,man1, imm[0],imm[1],funct3,flags1);
				ff_result.enq(x);
      	 end
      	 else begin
      	  let {man4,man5,man6} <- getMant64.func(operand1, 0,0);
      	  let {exp4,exp5,exp6} <- getExp64.func(operand1, 0,0);
      	  let {flags4,flags5,flags6} <- condFlags64.func(tuple2(man4,exp4),tuple2(0,0),tuple2(0,0));
      	  let x<-inst_dpfp_to_int._start(operand1[63],exp4,man4,imm[0],imm[1],funct3,flags4);
			  ff_result.enq(x);
      	  end
			 `ifdef verbose $display($time,"\tGiving Inputs to fpu to int Conversion Module");   `endif
		end
		else if(((funct7[6:2] == `FCLASS_f5)&&(funct3=='b001))&&(opcode == `FP_OPCODE))begin
			if(issp) begin
				let {op1,op2,op3}        <- setCanonicalNaN.func(operand1,'1,'1);
				let {man1,man2,man3}     <- getMant32.func(op1, 0,0);
				let {exp1,exp2,exp3}     <- getExp32.func(op1, 0,0);
				let {x1,x2,x3}           <- condFlags32.func(tuple2(man1,exp1),tuple2(0,0),tuple2(0,0));
				let x<-inst_spfpu_fclass._start(op1[31],man1,exp1,x1);
				Floating_output#(`FLEN) y = Floating_output{final_result:zeroExtend(x.final_result),fflags:x.fflags};
				ff_result.enq(y);
       end
       else begin
				let {man1,man2,man3} <- getMant64.func(operand1, 0,0);
				let {exp1,exp2,exp3} <- getExp64.func(operand1, 0,0);
				let {x1,x2,x3}       <- condFlags64.func(tuple2(man1,exp1),tuple2(0,0),tuple2(0,0));
				let x<-inst_dpfpu_fclass._start(operand1[63],man1,exp1,x1);
				ff_result.enq(x);
       end
		`ifdef verbose $display($time,"\tGiving inputs to floating classify module"); `endif
		end
		else if((funct7[6:2] == `FCVT_S_D_f5) && opcode == `FP_OPCODE)begin
      	if(!issp) begin
			`ifdef verbose $display("Giving inputs to Convert SP to DP"); `endif
      	     let {op1,op2,op3}        <- setCanonicalNaN.func(operand1,'1,'1);
      	     let {man1,man2,man3}     <- getMant32.func(op1, 0,0);
      	     let {exp1,exp2,exp3}     <- getExp32.func(op1, 0,0);
      	     let {x1,x2,x3}           <- condFlags32.func(tuple2(man1,exp1),tuple2(0,0),tuple2(0,0));
      	     let x<-inst_spfpu_cnvt._start(op1[31],exp1,man1,funct3,x1);
				  ff_result.enq(x);
      	end
      	else begin
					`ifdef verbose $display("Giving inputs to Convert DP to SP"); `endif
      	     let {man1,man2,man3}     <- getMant64.func(operand1, 0,0);
      	     let {exp1,exp2,exp3}     <- getExp64.func(operand1, 0,0);
      	     let {x1,x2,x3}           <- condFlags64.func(tuple2(man1,exp1),tuple2(0,0),tuple2(0,0));
      	     let x<-inst_dpfpu_cnvt._start(operand1[63],exp1,man1,funct3,x1);
				  Floating_output#(`Reg_width) y=?;
				`ifdef dpfpu
        		    y.final_result = {'hffffffff,x.final_result[31:0]};
        		`else
        		    y.final_result = x.final_result;
        		`endif
				y.fflags=x.fflags;
				ff_result.enq(y);
      	end
		end
		else if(((funct7 == `FMV_X_S_f7 || funct7 == `FMV_S_X_f7) && funct3 == 'b000) && opcode == `FP_OPCODE)begin
			`ifdef verbose $display($time,"\tGiving inputs to FMV"); `endif
			Bit#(`FLEN) final_result=0;
			if(funct7==`FMV_X_S_f7) // sp to integer FMV.X.W
				final_result = signExtend(operand1[31:0]);
  	 		else // integer to sp FMV.W.X
  	 			`ifdef dpfpu
  	 				final_result = {'1,operand1[31:0]};
  	 			`else
  	 				final_result= operand1[31:0];
  	 			`endif
			ff_result.enq(Floating_output{final_result:final_result, fflags : 0});
		end
		else if(((funct7 == `FMV_X_D_f7 || funct7 == `FMV_D_X_f7) && funct3 == 'b000) && opcode == `FP_OPCODE)begin // TODO merge with above condition
			`ifdef verbose $display($time,"\tGiving inputs to FMV"); `endif
			Bit#(`FLEN) final_result=0;
			if(funct7==`FMV_X_D_f7) // sp to integer FMV.X.W
				final_result = operand1;
			else // integer to sp FMV.W.X
						final_result= operand1;
			ff_result.enq(Floating_output{final_result:final_result, fflags : 0});
		end
		else if(((funct7[6:2] == `FADD_f5 || funct7[6:2] == `FSUB_f5) && opcode == `FP_OPCODE))begin // add sub
			rg_multicycle_op<=True;
			if(issp) begin
            let {op1,op2,op3}      <- setCanonicalNaN.func(operand1,operand2,'1);
			   let {man1,man2,man3}   <- getMant32.func(32'h3f800000,op1, op2);
            let {exp1,exp2,exp3}   <- getExp32.func(32'h3f800000,op1, op2);
            let x                  <- condFlags32.func(tuple2(man1,exp1),tuple2(man2,exp2),tuple2(man3,exp3));
            let sign1 = 0;
            let sign2 = op1[31];
            let sign3 = op2[31];
            inst_spfm_add_sub._start(tuple3(sign1,exp1,man1),tuple3(sign2,exp2,man2),tuple3(sign3,exp3,man3),funct3,funct7[2],1'b0,1'b0,0,x);
         end
         else begin
				let {man1,man2,man3}   <- getMant64.func(64'h3ff0000000000000,operand1, operand2);
          	let {exp1,exp2,exp3}   <- getExp64.func(64'h3ff0000000000000,operand1, operand2);
          	let x                  <- condFlags64.func(tuple2(man1,exp1),tuple2(man2,exp2),tuple2(man3,exp3));
          	let sign1 = 0;
          	let sign2 = operand1[63];
          	let sign3 = operand2[63];
     		 	inst_dpfm_add_sub._start(tuple3(sign1,exp1,man1),tuple3(sign2,exp2,man2), tuple3(sign3,exp3,man3), funct3, funct7[2], 1'b0, 1'b0,0,x);
         end
		`ifdef verbose $display($time,"\tGiving inputs to the fpu add_sub"); `endif
		end
		else if((funct7[6:2] == `FDIV_f5) && opcode == `FP_OPCODE)begin// spfpu divider
			rg_multicycle_op<=True;
        if(issp) begin
            let {op1,op2,op3} <- setCanonicalNaN.func(operand1,operand2,'1);
            let {man1,man2,man3} <- getMant32.func(op1, op2,op3);
            let {exp1,exp2,exp3} <- getExp32.func(op1, op2,op3);
            let {f1,f2,f3}       <- condFlags32.func(tuple2(man1,exp1),tuple2(man2,exp2),tuple2(man3,exp3));
            inst_spfpu_divider._start(op1[31]^op2[31],man1,exp1,man2,exp2,funct3,tuple2(f1,f2));
        end
        else begin
            let {man3,man4,man5} <- getMant64.func(operand1, operand2,0);
            let {exp3,exp4,exp5} <- getExp64.func(operand1,operand2,0);
            let {y1,y2,y3}       <- condFlags64.func(tuple2(man3,exp3),tuple2(man4,exp4),tuple2(0,0));
            inst_dpfpu_divider._start(operand1[63]^operand2[63],man3,exp3,man4,exp4,funct3, tuple2(y1,y2));
        end
		`ifdef verbose $display($time,"\tGiving inputs to the spfpu divider"); `endif
		end
		else if((funct7[6:2] == `FSQRT_f5) && opcode == `FP_OPCODE)begin// sqrt
			rg_multicycle_op<=True;
			if(issp) begin
				let {op1,op2,op3} <- setCanonicalNaN.func(operand1,'1,'1);
				let {man1,man2,man3} <- getMant32.func(op1, 0,0);
				let {exp1,exp2,exp3} <- getExp32.func(op1, 0,0);
				let x           <- condFlags32.func(tuple2(man1,exp1),tuple2(0,0),tuple2(0,0));
				inst_spfpu_sqrt._start(operand1[31], man1, exp1, funct3, tpl_1(x));
      	end
      	else begin
				let {man3,man4,man5} <- getMant64.func(operand1, 0,0);
				let {exp3,exp4,exp5} <- getExp64.func(operand1,0,0);
				let y           <- condFlags64.func(tuple2(man3,exp3),tuple2(0,0),tuple2(0,0));
				inst_dpfpu_sqrt._start(operand1[63], man3, exp3, funct3, tpl_1(y));
				`ifdef verbose $display($time,"\tGiving inputs to the spfpu sqrt"); `endif
      	end
		end
		else if((funct7[6:2] == `FMUL_f5) && opcode == `FP_OPCODE)begin
			rg_multicycle_op<=True;
		  `ifdef verbose $display("funct3 : %h",funct3); `endif
         if(issp) begin
            let {op1,op2,op3}      <- setCanonicalNaN.func(operand1,operand2,'1);
			   let {man1,man2,man3}   <- getMant32.func(op1, op2,0);
            let {exp1,exp2,exp3}   <- getExp32.func(op1, op2,0);
            let x                  <- condFlags32.func(tuple2(man1,exp1),tuple2(man2,exp2),tuple2(man3,exp3));
            let sign1 = op1[31];
            let sign2 = op2[31];
            let sign3 = 0;
            inst_spfm_add_sub._start(tuple3(sign1,exp1,man1),tuple3(sign2,exp2,man2),tuple3(sign3,exp3,man3),funct3,1'b1,1'b0,1'b1,0,x);
			end
         else begin
                let {man1,man2,man3}   <- getMant64.func(operand1, operand2,0);
                let {exp1,exp2,exp3}   <- getExp64.func(operand1, operand2,0);
                let x                  <- condFlags64.func(tuple2(man1,exp1),tuple2(man2,exp2),tuple2(man3,exp3));
                let sign1 = operand1[63];
                let sign2 = operand2[63];
                let sign3 = 0;
     		    inst_dpfm_add_sub._start(tuple3(sign1,exp1,man1),tuple3(sign2,exp2,man2),tuple3(sign3,exp3,man3), funct3, 1'b1, 1'b0, 1'b1,0,x);
         end
		 `ifdef verbose $display($time,"\tGiving inputs to the spfloating multiplier module"); `endif
		end
		else if((opcode == 'b0000) || (opcode == 'b0001) || (opcode == 'b0010) || opcode == 'b0011)begin
			rg_multicycle_op<=True;
         if(issp) begin
				`ifdef verbose $display($time,"\tGiving Inputs to sp fused multiply add Conversion Module"); `endif
            let {op1,op2,op3} <- setCanonicalNaN.func(operand1,operand2,operand3);
				let {man1,man2,man3}   <- getMant32.func(op1, op2,op3);
            let {exp1,exp2,exp3}   <- getExp32.func(op1, op2,op3);
            let x                  <- condFlags32.func(tuple2(man1,exp1),tuple2(man2,exp2),tuple2(man3,exp3));
            let sign1 = op1[31];
            let sign2 = op2[31];
            let sign3 = op3[31];
            inst_spfm_add_sub._start(tuple3(sign1,exp1,man1),tuple3(sign2,exp2,man2),tuple3(sign3,exp3,man3),funct3,opcode[0]^opcode[1],opcode[1],1'b0,1,x);
            end
        else begin
				`ifdef verbose $display($time,"\tGiving Inputs to dp fused multiply add Conversion Module"); `endif
            let {man1,man2,man3}   <- getMant64.func(operand1, operand2,operand3);
            let {exp1,exp2,exp3}   <- getExp64.func(operand1, operand2,operand3);
            let x                  <- condFlags64.func(tuple2(man1,exp1),tuple2(man2,exp2),tuple2(man3,exp3));
            let sign1 = operand1[63];
            let sign2 = operand2[63];
            let sign3 = operand3[63];
     		inst_dpfm_add_sub._start(tuple3(sign1,exp1,man1),tuple3(sign2,exp2,man2),tuple3(sign3,exp3,man3), funct3, opcode[0]^opcode[1],opcode[1], 1'b0,1,x);
        end
       `ifdef verbose $display($time,"\tOperand 1: %h Operand 2: %h Operand 3: %h",operand1, operand2, operand3); `endif
		end
 endrule

  //rule to get output from spfpu divider
  rule rl_get_output_from_spfpu_divider(!wr_flush && rg_multicycle_op);
    `ifdef verbose $display($time,"\tGot output from spfpu divider");  `endif
    let x= inst_spfpu_divider.final_result_;
    Floating_output#(`FLEN) y=?;
    y.fflags=x.fflags;
    `ifdef dpfpu
        y.final_result={'1,x.final_result};
    `else
        y.final_result=x.final_result;
    `endif
    ff_result.enq(y);
	  rg_multicycle_op<=False;
  endrule
  
  //rule to get output from spfpu divider
  rule rl_get_output_from_dpfpu_divider(!wr_flush && rg_multicycle_op);
    `ifdef verbose $display($time,"\tGot output from spfpu divider");  `endif
    let x= inst_dpfpu_divider.final_result_;
    Floating_output#(`FLEN) y=?;
    y.fflags=x.fflags;
    y.final_result=x.final_result;
    ff_result.enq(y);
	  rg_multicycle_op<=False;
  endrule
     
	//rule to give inputs to spfpu square root module

  //rule to get output spfpu square root module
  rule rl_get_output_from_spfpu_sqrt(inst_spfpu_sqrt.get_result matches tagged Valid .res &&& !wr_flush &&& rg_multicycle_op); // TODO check for inexact and underflow
    `ifdef verbose $display($time,"\tGot output from spfpu sqrt");  `endif
    let x = res;
		Floating_output#(`FLEN) y=?;
		y.fflags=x.fflags;
		`ifdef dpfpu
			y.final_result={'1,x.final_result};
		`else
			y.final_result=x.final_result;
		`endif
		ff_result.enq(y);
	  rg_multicycle_op<=False;
  endrule

  //rule to get output spfpu square root module
 rule rl_get_output_from_dpfpu_sqrt(inst_dpfpu_sqrt.get_result matches tagged Valid .res &&& !wr_flush &&& rg_multicycle_op); // TODO check for inexact and underflow
    `ifdef verbose $display($time,"\tGot output from spfpu sqrt");  `endif
    let x = res;
	Floating_output#(`FLEN) y=?;
	y.fflags=x.fflags;
	y.final_result=x.final_result;
	ff_result.enq(y);
	  rg_multicycle_op<=False;
  endrule


  //rule to get output from fused multiply add sub
	 rule rl_get_output_from_fm_add_sub(!wr_flush && rg_multicycle_op);
		`ifdef verbose $display($time,"\tGot output from sp fused multiple add conversion Module"); `endif
		let x= inst_spfm_add_sub.get_result;
		Floating_output#(`FLEN) y=?;
		y.fflags=x.fflags;
		`ifdef dpfpu
			y.final_result={'hffffffff,x.final_result[31:0]};
		`else
			y.final_result=x.final_result;
		`endif
		 ff_result.enq(y);
     `ifdef verbose $display($time,"\tFMA Result : %16h", y.final_result); `endif
	  rg_multicycle_op<=False;
	 endrule

     //rule to get output from fused multiply add sub
	 rule rl_get_output_from_dpfm_add_sub(!wr_flush && rg_multicycle_op);
		`ifdef verbose $display($time,"\tGot output from sp fused multiple add conversion Module"); `endif
		let x= inst_dpfm_add_sub.get_result;
		Floating_output#(`FLEN) y=?;
		y.fflags=x.fflags;
		y.final_result=x.final_result;
		ff_result.enq(y);
     `ifdef verbose $display($time,"\tFMA Result : %16h", y.final_result); `endif
	  rg_multicycle_op<=False;
	 endrule
	 
	rule flush_fifo(wr_flush);
		ff_result.clear;
		rg_multicycle_op<=False;
	endrule
     //rule to give inputs to spfloating multiplier

	// input method to start the floating point operation

	method Action _start(Bit#(`FLEN) operand1, Bit#(`FLEN) operand2, Bit#(`FLEN) operand3, Bit#(4) opcode, Bit#(7) funct7, Bit#(3) funct3, Bit#(2) imm,  Bit#(3) fsr, Bool issp) if(!rg_multicycle_op); 
	    ff_input.enq ( Input_Packet {
                                        operand1 : operand1,
                                        operand2 : operand2,
                                        operand3 : operand3,
                                        opcode   : opcode,
                                        funct7   : funct7,
                                        funct3   : funct3,
                                        imm      : imm,
                                        fsr      : fsr,
                                        issp     : issp
                                    });
    endmethod
 
	method ActionValue#(Floating_output#(`FLEN)) get_result;
		ff_result.deq;
		return ff_result.first;
	endmethod
	method Action flush;
		  wr_flush<=True;
        inst_spfpu_divider.flush();
        inst_dpfpu_divider.flush();
        inst_spfpu_sqrt.flush();
        inst_dpfpu_sqrt.flush();
        inst_spfm_add_sub.flush();
        inst_dpfm_add_sub.flush();
	endmethod

endmodule
endpackage
