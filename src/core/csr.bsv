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
package csr;

/*  ############################ changes from 1.9.1 ############################
		1. base field in misa is changed to readOnlyfield of mxl as per the encoding
			 and the same is reflected in sxl and uxl fields of mstatus
		2. change of fields in mstatus.
*/

	import defined_types::*;
	`include "defined_parameters.bsv"
	import ConfigReg::*;
	import GetPut            ::*;
	import ConcatReg ::*;
	import Vector::*;

	interface Ifc_csr;
		method Bit#(3) roundingmode;
		method Action set_external_interrupt(Tuple2#(Bool,Bool) i);
		method Action flush;
		(*always_ready,always_enabled*)
		method Action boot_sequence(Bit#(1) bootseq);
		/*======= MMU related interfaces ===== */
		`ifdef MMU
			method Bit#(`Reg_width) send_satp;
			method Chmod perm_to_TLB;
		`endif
		method Bit#(`Reg_width) mmu_cache_disable;
		/*=========================================== */
		/*=========== Debug related interfaces ====== */
		`ifdef Debug
			method Bool halted;
			method ActionValue#(Bit#(`Reg_width)) rw_debug_csr(Bit#(12) r, Bool write, Bit#(`Reg_width) data);
			method TriggerData load_triggerdata;
			method TriggerData store_triggerdata;
			method Bool step_now;
			method Bool reset_mode;
		`endif
		/*=========================================== */
		method ActionValue#(Tuple2#(Bit#(3),Trap_type)) check_for_trap(`ifdef Debug Bool haltreq, Bool resumereq, Bool resetreq, `endif Bit#(`VADDR) pc, Bit#(32) instruction); 
	method ActionValue#(Tuple4#(Bool,Bit#(`VADDR),Bit#(`Reg_width),Bool)) system_instruction(WriteBackType wbdata ,Bit#(`VADDR) pc, Bit#(`PERFMONITORS) perfmonitor_incr `ifdef simulate , Bit#(32) instruction, Operand_type rd_type, Bit#(5) destination `endif );
		method ActionValue#(Tuple2#(Bit#(`VADDR), Bool)) take_trap(Trap_type exception, Bit#(3) lv_debugcause, Bit#(`VADDR) pc, Bit#(`VADDR) badaddr);
		method Bit#(`Reg_width) misa;
		method Bit#(2) powercontrol;
		method Action poweracknowledge(Bit#(2) pa);
		`ifdef CLINT
			method Action clint_msip(Bit#(1) intrpt);
			method Action clint_mtip(Bit#(1) intrpt);
			method Action clint_mtime(Bit#(`Reg_width) c_mtime);
		`endif
  endinterface

  function String csr_funct(Bit#(3) funct3);
		case(funct3)
			'd1:return	"CSRRW";
			'd2:return  "CSRRS";
			'd3:return  "CSRRC";
			'd5:return  "CSRRWI" ;
			'd6:return  "CSRRSI"; 
			'd7:return  "CSRRCI" ;
			default: return "NOIDEA";
		endcase
  endfunction

  function Reg#(t) readOnlyReg(t r);
    return (interface Reg;
       method t _read = r;
       method Action _write(t x) = noAction;
    endinterface);
  endfunction

  function Reg#(Bit#(a)) extInterruptReg(Reg#(Bit#(a)) r1, Reg#(Bit#(a)) r2);
    return (interface Reg;
            method Bit#(a) _read = r1 | r2;
            method Action _write(Bit#(a) x); 
            	r1._write(x);
				endmethod
        endinterface);
  endfunction

  function Reg#(t) writeSideEffect(Reg#(t) r, Action a);
    return (interface Reg;
            method t _read = r._read;
            method Action _write(t x);
                r._write(x);
                a;
            endmethod
        endinterface);
endfunction

  (*synthesize*)
	(*conflict_free="take_trap, set_external_interrupt"*)
  (*conflict_free="check_for_trap,system_instruction"*)
  module mkcsr(Ifc_csr);

	Reg#(Bool) rg_initialize[2]<-mkCReg(2,False);
	Reg#(Bit#(12)) rg_index<-mkReg(0);
	`ifdef simulate
		Reg#(Bool) wr_endsimulation <-mkReg(False);
  	    Reg#(Bit#(1)) rg_cnt <-mkReg(0);
  	    let dump <- mkReg(InvalidFile) ;
  	    rule open_file(rg_cnt==0);
  	 				String dumpFile = "rtl.dump" ;
  	        File lfh <- $fopen( dumpFile, "w" ) ;
  	        if ( lfh == InvalidFile )begin
  	            `ifdef verbose $display("cannot open %s", dumpFile); `endif
  	            $finish(0);
  	        end
  	        dump <= lfh ;
  	        rg_cnt <= 1 ;
  	    endrule
	`endif
  /////////////////////////////// Machine level register /////////////////////////
  // Current Privilege Level
	Reg#(Privilege_mode) rg_prv <- mkConfigReg(Machine); // resets to machine mode

	
	Reg#(Bit#(`Reg_width)) csr_mvendorid = readOnlyReg(0);
  	Reg#(Bit#(`Reg_width)) csr_marchid = readOnlyReg(0);
  	Reg#(Bit#(`Reg_width)) csr_mimpid = readOnlyReg(0);
  	Reg#(Bit#(`Reg_width)) csr_mhartid = readOnlyReg(0);
	//misa fields
	Reg#(Bit#(2))  rg_mxl		= readOnlyReg(`MXL_BITS);
	Bit#(26) temp_misa='d0;
	temp_misa[8]=1;
	temp_misa[20]=1;
	`ifdef atomic	temp_misa[0]=1; `endif
	`ifdef dpfpu	temp_misa[3]=1; `endif
	`ifdef spfpu	temp_misa[5]=1; `endif
	`ifdef muldiv	temp_misa[12]=1; `endif
	`ifdef MMU		temp_misa[18]=1; `endif
	`ifdef Openocd
		Reg#(Bit#(26)) rg_misa	<- mkReg(temp_misa);
	`else 
		Reg#(Bit#(26)) rg_misa	<- mkReg(`MISA_BITS);
	`endif
	Reg#(Bit#(`Reg_width)) csr_misa = concatReg3(rg_mxl,readOnlyReg(0),rg_misa);
   
  // trap vector fields (same as CSR without bottom 2 bits)
	Reg#(Bit#(2))  rg_mode_m		<- mkReg(0); //default value 0 if pc to base or 1 if pc to base + 4xcause
	Reg#(Bit#(TSub#(`PADDR,2))) rg_mtvec <- mkReg(`MTVEC_DEFAULT);
	Reg#(Bit#(`Reg_width)) csr_mtvec=concatReg3(readOnlyReg(0),rg_mtvec, rg_mode_m);

  // mstatus fields
	Reg#(Bit#(1)) rg_tsr	<- mkReg(0); // WARL
  	Reg#(Bit#(1)) rg_tw	 	<- mkReg(0); // WARL
  	Reg#(Bit#(1)) rg_tvm	<- mkReg(0); // WARL
  	Reg#(Bit#(1)) rg_mxr  <- mkReg(0); // Not required
  	Reg#(Bit#(1)) rg_sum  <- mkReg(0); // Not required
  	Reg#(Bit#(1)) rg_mprv <- mkReg(0);
  	Reg#(Bit#(2)) rg_xs	 	=  readOnlyReg(0);
  	Reg#(Bit#(2)) rg_fs	 	<- mkReg(2'b00);
  	Reg#(Bit#(2)) rg_mpp	<- mkReg(2'b0);
  	Reg#(Bit#(2)) rg_hpp	=  readOnlyReg(0);
  	Reg#(Bit#(1)) rg_spp	<-  mkReg(0);
  	Reg#(Bit#(1)) rg_mpie <- mkReg(0);
  	Reg#(Bit#(1)) rg_hpie =  readOnlyReg(0);
  	Reg#(Bit#(1)) rg_spie <- mkReg(0);
  	Reg#(Bit#(1)) rg_upie <- mkReg(0);
	`ifdef Openocd
	  	Reg#(Bit#(1)) rg_mie	<- mkReg(1);
	`else
	  	Reg#(Bit#(1)) rg_mie	<- mkReg(0);
	`endif
  	Reg#(Bit#(1)) rg_hie	=  readOnlyReg(0);
  	Reg#(Bit#(1)) rg_sie	<- mkReg(0);
  	Reg#(Bit#(1)) rg_uie	<- mkReg(0);
  	Reg#(Bit#(1)) rg_sd	 	=  readOnlyReg(pack((rg_xs == 2'b11) || (rg_fs == 2'b11)));
  	Reg#(Bit#(`Reg_width)) csr_mstatus =  concatReg24(
           rg_sd,
           readOnlyReg(0),
           rg_mxl, rg_mxl,          //sxl and uxl fields are hardwired to mxl in misa
           readOnlyReg(9'b0),
					 rg_tsr, rg_tw, rg_tvm,
           rg_mxr, rg_sum, rg_mprv, // memory privilege
           rg_xs, rg_fs, // coprocessor states
           rg_mpp, rg_hpp, rg_spp, // previous privileges
           rg_mpie, rg_hpie, rg_spie, rg_upie, // previous interrupt enables
           rg_mie, rg_hie, rg_sie, rg_uie); // interrupt enables

	// trap delegation fields
  	Reg#(Bit#(16)) rg_medeleg<-mkReg(0);
  	Reg#(Bit#(15)) rg_mideleg<-mkReg(0);
  	Reg#(Bit#(`Reg_width)) csr_medeleg = concatReg2(readOnlyReg(0),rg_medeleg);
  	Reg#(Bit#(`Reg_width)) csr_mideleg = concatReg2(readOnlyReg(0),rg_mideleg);

	// mie fields
  	Reg#(Bit#(1)) rg_meie <- mkReg(0);
  	Reg#(Bit#(1)) rg_heie =  readOnlyReg(0);
  	Reg#(Bit#(1)) rg_seie <-  mkReg(0);
  	Reg#(Bit#(1)) rg_ueie <- mkReg(0);
  	Reg#(Bit#(1)) rg_mtie <- mkReg(0);
  	Reg#(Bit#(1)) rg_htie =  readOnlyReg(0);
  	Reg#(Bit#(1)) rg_stie <-  mkReg(0);
  	Reg#(Bit#(1)) rg_utie <- mkReg(0);
  	Reg#(Bit#(1)) rg_msie <- mkReg(0);
  	Reg#(Bit#(1)) rg_hsie =  readOnlyReg(0);
  	Reg#(Bit#(1)) rg_ssie <-  mkReg(0);
  	Reg#(Bit#(1)) rg_usie <- mkReg(0);
	`ifdef Openocd
		Reg#(Bit#(1)) rg_dhalt<-mkReg(1);
		Reg#(Bit#(1)) rg_dresume<-mkReg(1);
	`else
		Reg#(Bit#(1)) rg_dhalt<-mkReg(0);
		Reg#(Bit#(1)) rg_dresume<-mkReg(0);
	`endif
	Reg#(Bit#(1)) rg_dreset<-mkReg(0);
  	Reg#(Bit#(`Reg_width)) csr_mie     =  concatReg16(
           readOnlyReg(0),
			  rg_dreset,rg_dresume,rg_dhalt,
           rg_meie, rg_heie, rg_seie, readOnlyReg(rg_ueie),
           rg_mtie, rg_htie, rg_stie, readOnlyReg(rg_utie),
           rg_msie, rg_hsie, rg_ssie, readOnlyReg(rg_usie));
		Reg#(Bool) rg_nmi <- mkReg(True);

	// mip fields
  	Reg#(Bit#(1)) rg_meip <-  mkConfigReg(0);
  	Reg#(Bit#(1)) rg_heip =  readOnlyReg(0);
  	Reg#(Bit#(1)) rg_seips <-   mkReg(0);
  	Reg#(Bit#(1)) rg_seipe <-   mkReg(0);
  	Reg#(Bit#(1)) rg_ueips <-   mkReg(0);
  	Reg#(Bit#(1)) rg_ueipe <-   mkReg(0);
  	Reg#(Bit#(1)) rg_seip =   extInterruptReg(rg_seips,rg_seipe);
  	Reg#(Bit#(1)) rg_ueip =   extInterruptReg(rg_ueips,rg_ueipe);
  	Reg#(Bit#(1)) rg_mtip <-mkReg(0);
  	Reg#(Bit#(1)) rg_htip =  readOnlyReg(0);
  	Reg#(Bit#(1)) rg_stip <-  mkReg(0);
  	Reg#(Bit#(1)) rg_utip <- mkReg(0);
	Reg#(Bit#(1)) rg_msip <-  mkReg(0);
  	Reg#(Bit#(1)) rg_hsip =  readOnlyReg(0);
  	Reg#(Bit#(1)) rg_ssip <-  mkReg(0);
  	Reg#(Bit#(1)) rg_usip <- mkReg(0);
 
	`ifdef RV64
		Reg#(Bit#(`Reg_width)) csr_mcycle[2]<-mkCReg(2,0);
		Reg#(Bit#(`Reg_width)) csr_minstret[2]<-mkCReg(2,0);
	`else
		Reg#(Bit#(`Reg_width)) csr_mcycle[2]<-mkCReg(2,0);
		Reg#(Bit#(`Reg_width)) csr_minstret[2]<-mkCReg(2,0);
		Reg#(Bit#(`Reg_width)) csr_mcycleh[2]<-mkCReg(2,0);
		Reg#(Bit#(`Reg_width)) csr_minstreth[2]<-mkCReg(2,0);
	`endif

	// Machine Trap Handling
	Reg#(Bit#(`Reg_width)) 		 rg_mepc  		<- mkReg(0);
	Reg#(Bit#(`VADDR))	 rg_mtval  		<- mkReg(0);
	Reg#(Bit#(`Reg_width)) csr_mscratch <- mkReg(0);
	Reg#(Bit#(`Reg_width)) csr_mepc     = rg_mepc;
	Reg#(Bit#(`Reg_width)) csr_mcause   <- mkReg(0);
	Reg#(Bit#(`Reg_width)) csr_mtval		= concatReg2(readOnlyReg(0), rg_mtval);
	Reg#(Bit#(`Reg_width)) csr_mip      =  concatReg13(
           readOnlyReg(0),
           readOnlyReg(rg_meip), readOnlyReg(rg_heip), rg_seip, rg_ueip,
           readOnlyReg(rg_mtip), readOnlyReg(rg_htip), rg_stip, rg_utip,
           readOnlyReg(rg_msip), readOnlyReg(rg_hsip), rg_ssip, rg_usip);
  
	Reg#(Bit#(`Reg_width)) mip      =  concatReg13(
           readOnlyReg(0),
           rg_meip, rg_heip, rg_seip, rg_ueip,
           rg_mtip, rg_htip, rg_stip, rg_utip,
           rg_msip, rg_hsip, rg_ssip, rg_usip);
	//////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////Physical Memory Protection////////////////////////////
	Reg#(Bit#(`Reg_width)) csr_pmpcfg0 <- mkReg(0);
	`ifndef RV64
	Reg#(Bit#(`Reg_width)) csr_pmpcfg1 <- mkReg(0);
	`endif
	Reg#(Bit#(`Reg_width)) csr_pmpcfg2 <- mkReg(0);
	`ifndef RV64
	Reg#(Bit#(`Reg_width)) csr_pmpcfg3 <- mkReg(0);
	`endif

	Reg#(Bit#(TSub#(`PADDR,2)))  rg_pmpaddr[`PMPADDREND - `PMPADDRSTART +1];
	Reg#(Bit#(`Reg_width)) csr_pmpaddr[`PMPADDREND - `PMPADDRSTART +1];
	for(Integer i=0; i<(`PMPADDREND - `PMPADDRSTART +1); i =i+1) begin
		rg_pmpaddr[i] <- mkReg(0);
		csr_pmpaddr[i] = concatReg2(readOnlyReg(0), rg_pmpaddr[i]);
	end
	//////////////////////////////////////////////////////////////////////////////////////////
	// Counter enables
	Reg#(Bit#(1)) rg_u_ir <- mkReg(0);
  	Reg#(Bit#(1)) rg_u_tm <- mkReg(0);
  	Reg#(Bit#(1)) rg_u_cy <- mkReg(0);
  	// Machine Counter Setup
	Reg#(Bit#(32)) reg_mcounteren<-mkReg(0);
  	Reg#(Bit#(`Reg_width)) csr_mcounteren=concatReg2(readOnlyReg(32'd0),reg_mcounteren);
	Reg#(Bit#(1)) rg_boot_seq<-mkReg(0);
	Reg#(Bit#(`Reg_width)) csr_boot_seq =concatReg2(readOnlyReg(0),readOnlyReg(rg_boot_seq));
	Reg#(Bit#(2)) power_control_out <-mkReg(0);
	Reg#(Bit#(2)) power_control_in <-mkReg(0);
	Reg#(Bit#(`Reg_width)) csr_power_control = concatReg3(readOnlyReg(0),readOnlyReg(power_control_in),power_control_out);

	//////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////SUPERVISOR LEVEL REGISTERS//////////////////////////////////
`ifdef MMU
	Reg#(Bit#(`Reg_width)) csr_sstatus =  concatReg20(
  	         rg_sd,
  	         readOnlyReg(0), rg_mxl, readOnlyReg(12'b0), //uxl field
  	         rg_mxr, rg_sum, readOnlyReg(1'b0), // memory privilege // 
  	         rg_xs, rg_fs, // coprocessor states 
  	         readOnlyReg(2'b0), readOnlyReg(2'b0), rg_spp, // previous privileges
  	         readOnlyReg(1'b0), readOnlyReg(1'b0), rg_spie, rg_upie, // previous interrupt enables
  	         readOnlyReg(1'b0), readOnlyReg(1'b0), rg_sie, rg_uie); // interrupt enables

  	Reg#(Bit#(12)) rg_sedeleg<-mkReg(0);
  	Reg#(Bit#(15)) rg_sideleg<-mkReg(0);
  	Reg#(Bit#(`Reg_width)) csr_sedeleg = concatReg2(readOnlyReg(0),rg_sedeleg);
  	Reg#(Bit#(`Reg_width)) csr_sideleg = concatReg2(readOnlyReg(0),rg_sideleg);

  	Reg#(Bit#(`Reg_width)) csr_sie     =  concatReg13(
           readOnlyReg(0),
           readOnlyReg(1'b0), readOnlyReg(1'b0), rg_seie, readOnlyReg(rg_ueie),
           readOnlyReg(1'b0), readOnlyReg(1'b0), rg_stie, readOnlyReg(rg_utie),
           readOnlyReg(1'b0), readOnlyReg(1'b0), rg_ssie, readOnlyReg(rg_usie));

	Reg#(Bit#(2))  rg_mode_s		<- mkReg(0); //default value 0 if pc to base or 1 if pc to base + 4xcause
	Reg#(Bit#(TSub#(`Reg_width,2))) rg_stvec <- mkReg(`STVEC_DEFAULT);
  Reg#(Bit#(`Reg_width)) csr_stvec=concatReg2(rg_stvec,rg_mode_s);
  Reg#(Bit#(32)) rg_scounteren <- mkReg(0);
  Reg#(Bit#(`Reg_width)) csr_scounteren = concatReg2(readOnlyReg(0),rg_scounteren);

	//Supervisor Trap Handling Register
	Reg#(Bit#(`VADDR)) rg_stval			<- mkReg(0);
	Reg#(Bit#(`Reg_width)) csr_sscratch <- mkReg(0);
   Reg#(Bit#(`Reg_width)) csr_sepc     <- mkReg(0);
  	Reg#(Bit#(`Reg_width)) csr_scause   <- mkReg(0);
	Reg#(Bit#(`Reg_width)) csr_stval		= concatReg2(readOnlyReg(0), rg_stval);
	Reg#(Bit#(`Reg_width)) csr_sip      =  concatReg13(
           readOnlyReg(0),
					 readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(rg_seip), rg_ueip,
           readOnlyReg(1'b0), readOnlyReg(1'b0), readOnlyReg(rg_stip), readOnlyReg(rg_utip),
           readOnlyReg(1'b0), readOnlyReg(1'b0), rg_ssip, readOnlyReg(rg_usip));

	//Supervisor Protection and Translation
	Reg#(Bit#(`Reg_width)) csr_satp		<- mkReg(0);
`endif
  //////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// User level registers ///////////////////////////////////
	`ifdef RV64
		Reg#(Bit#(`Reg_width)) csr_uinstret=readOnlyReg(csr_minstret[1]);
		Reg#(Bit#(`Reg_width)) csr_ucycle=readOnlyReg(csr_mcycle[1]);
	`else
		Reg#(Bit#(`Reg_width)) csr_uinstret=readOnlyReg(csr_minstret[1]);
		Reg#(Bit#(`Reg_width)) csr_ucycle=readOnlyReg(csr_mcycle[1]);
		Reg#(Bit#(`Reg_width)) csr_uinstreth=readOnlyReg(csr_minstreth[1]);
		Reg#(Bit#(`Reg_width)) csr_ucycleh=readOnlyReg(csr_mcycleh[1]);
	`endif

	Reg#(Bit#(`Reg_width)) rg_clint_mtime <-mkReg(0);
	Reg#(Bit#(5)) rg_fflags<-mkReg(0);
	Reg#(Bit#(3)) rg_frm<-mkReg(0);
	Reg#(Bit#(`Reg_width)) csr_fcsr = writeSideEffect(concatReg3(readOnlyReg(0),rg_frm,rg_fflags),rg_fs._write(2'b11));
	Reg#(Bit#(`Reg_width)) csr_fflags=writeSideEffect(concatReg2(readOnlyReg(0),rg_fflags),rg_fs._write(2'b11));
	Reg#(Bit#(`Reg_width)) csr_frm =  writeSideEffect(concatReg2(readOnlyReg(0),rg_frm),rg_fs._write(2'b11));
	Reg#(Bit#(4)) 				 rg_memse <- mkReg(0); //0th-bit set -> immu disable,1th-bit set -> icache,2nd-bit -> dmmu, 3rd bit -> dcache 
	Reg#(Bit#(`Reg_width)) csr_memse = concatReg2(readOnlyReg(0),rg_memse);  

  //////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////// PErformance Counters //////////////////////////
	`ifdef perf
	Array#(Reg#(Bit#(64))) csr_mhpmcounter[`MHPMCOUNTEND-`MHPMCOUNTSTART+1];
	Reg#(Bit#(`Reg_width)) csr_mhpmevent[`MHPMCOUNTEND-`MHPMCOUNTSTART+1];
	for(Integer i=0;i<=(`MHPMCOUNTEND-`MHPMCOUNTSTART);i=i+1)begin
		csr_mhpmcounter[i]<-mkCReg(2,0);
	end
	csr_mhpmevent[0]<-mkReg('h20000000);
	csr_mhpmevent[1]<-mkReg('h20000);
	csr_mhpmevent[2]<-mkReg('h4000);
	`endif

  //////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////// Debug Registers /////////////////////////////////
	`ifdef Debug
		`ifdef Openocd
			Reg#(Bool) resetmode[2]<-mkCReg(2,False); // TODO
			Reg#(Bit#(1)) ebreakm<-mkReg(1);
			Reg#(Bit#(1)) ebreaks<-mkReg(1);
			Reg#(Bit#(1)) ebreaku<-mkReg(1);
		`else
			Reg#(Bool) resetmode[2]<-mkCReg(2,False); // TODO
			Reg#(Bit#(1)) ebreakm<-mkReg(0);
			Reg#(Bit#(1)) ebreaks<-mkReg(0);
			Reg#(Bit#(1)) ebreaku<-mkReg(0);
		`endif
		Reg#(Bit#(1)) stopcount<-mkReg(0);
		Reg#(Bit#(1)) stoptime <-mkReg(0);
		Reg#(Bit#(3)) debugcause<-mkReg(0);
		Reg#(Bit#(1)) step<-mkReg(0);
		Reg#(Bit#(2)) debugprv<-mkReg(0);
		Reg#(Bit#(32)) dcsr=concatReg13(readOnlyReg(4'd4), readOnlyReg(12'd0),
		ebreakm,readOnlyReg(1'b0),ebreaks,ebreaku,
		readOnlyReg(1'b0),
		stopcount,stoptime,
		readOnlyReg(debugcause),readOnlyReg(3'd0),
		step,debugprv);
		Reg#(Bit#(`VADDR)) dpc <-mkReg(0);
		Reg#(Bit#(`Reg_width)) csr_dpc=concatReg2(readOnlyReg('d0),dpc);
		Reg#(Bit#(`Reg_width)) dscratch0<-mkReg(0);
		Reg#(Bool) debugmode_active[2]<-mkCReg(2,False);
		Reg#(Bit#(`Reg_width)) debugentry<-mkReg(`DebugBase);
		//////////////////////////////////////////////////////////////////////////////////////////
		//////////////////////// Trigger Registers ////////////////////////////////////////
		Reg#(Bit#(`Reg_width)) tselect=readOnlyReg('d0);
		Reg#(Bit#(4)) trigger_type=readOnlyReg('d2); 
		Reg#(Bit#(1)) dmode=readOnlyReg(1'd0);
		Reg#(Bit#(6)) maskmax=readOnlyReg(6'd0);
		Reg#(Bit#(1)) select<-mkReg(0);
		Reg#(Bit#(1)) timing=readOnlyReg(1'b0);
		Reg#(Bit#(6)) triggeraction=readOnlyReg(6'd0); // always enter debug mode.
		Reg#(Bit#(1)) chain<-mkReg(0);
		Reg#(Bit#(4)) triggermatch<-mkReg(0);
		Reg#(Bit#(1)) triggerm<-mkReg(0);
		Reg#(Bit#(1)) triggers<-mkReg(0);
		Reg#(Bit#(1)) triggeru<-mkReg(0);
		Reg#(Bit#(1)) execute<-mkReg(0);
		Reg#(Bit#(1)) store<-mkReg(0);
		Reg#(Bit#(1)) load<-mkReg(0);
		Reg#(Bit#(`Reg_width)) tdata1=concatReg16(trigger_type,dmode,maskmax,
		readOnlyReg(33'd0),select,timing,
		triggeraction,chain,triggermatch,
		triggerm,readOnlyReg(1'b0),triggers,triggeru,
		execute,store,load);
		Reg#(Bit#(`Reg_width)) tdata2<-mkReg(0);
		Wire#(TriggerData) executetrigger<-mkDWire(TriggerData{ttype:tagged None,matchscheme:0});
		Wire#(TriggerData) loadtrigger<-mkDWire(TriggerData{ttype:tagged None,matchscheme:0});
		Wire#(TriggerData) storetrigger<-mkDWire(TriggerData{ttype:tagged None,matchscheme:0});
		Reg#(Bool) rg_step_now<-mkReg(False);
		rule generate_trigger_info_decode;
			if(execute==1 && ((rg_prv==User &&triggeru==1)||(rg_prv==Supervisor &&triggers==1)||(rg_prv==Machine &&triggerm==1)) )
				executetrigger<=TriggerData{ttype:select==0?tagged Address tdata2:tagged Data tdata2,matchscheme:triggermatch};
			if(load==1&& ((rg_prv==User &&triggeru==1)||(rg_prv==Supervisor &&triggers==1)||(rg_prv==Machine &&triggerm==1)) )
				loadtrigger<=TriggerData{ttype:select==0?tagged Address tdata2:tagged Data tdata2,matchscheme:triggermatch};
			if(store==1&& ((rg_prv==User &&triggeru==1)||(rg_prv==Supervisor &&triggers==1)||(rg_prv==Machine &&triggerm==1)) )
				storetrigger<=TriggerData{ttype:select==0?tagged Address tdata2:tagged Data tdata2,matchscheme:triggermatch};
		endrule
	`endif
`ifdef Debug
	function Bool checktrigger(TriggerData tdata, Bit#(`VADDR) pc1, Bit#(32) instruction);
			Bit#(`Reg_width) pc=zeroExtend(pc1);
			if(tdata.ttype matches tagged Address .addr)
				if(tdata.matchscheme==0 && addr==pc)
					return True;
				else if(tdata.matchscheme==2 && addr>=pc)
					return True;
				else if(tdata.matchscheme==3 && addr<=pc)
					return True;
				else if(tdata.matchscheme==4 && addr[31:0]==(addr[63:32]&pc[31:0]))
					return True;
				else if(tdata.matchscheme==5 && addr[31:0]==(addr[`Reg_width-1:32]&pc[`Reg_width-1:32]))
					return True;
				else
					return False;
			else if(tdata.ttype matches tagged Data .data)
				if(data[31:0]==instruction)
					return True;
				else
					return False;
			else 
				return False;
		endfunction
`endif
/////////// Functions to access CSRs /////////////////////////////////////////////////////////////
	function Reg#(Bit#(`Reg_width)) read_user_sro_registers(Bit#(8) addr);
		Reg#(Bit#(`Reg_width)) csr=(case(addr)
			`UCYCLE				:csr_ucycle;				
			`UTIME				:readOnlyReg(rg_clint_mtime);  
			`UINSTRET			:csr_uinstret;
			`ifndef RV64 
				`UCYCLEH			:csr_ucycleh; 
				`UINSTRETH		:csr_uinstreth; 
			`endif
			default:readOnlyReg(0);
			endcase);
		return csr;
	endfunction
	function Reg#(Bit#(`Reg_width)) read_user_srw_registers(Bit#(8) addr);
		Reg#(Bit#(`Reg_width)) csr=(case(addr)
			`FFLAGS	 :csr_fflags	;
			`FRM		 :csr_frm	;
			`FCSR		 :csr_fcsr	;
			`UMEMSE	 :csr_memse;
			default: readOnlyReg(0);
		endcase);
		return csr;
	endfunction
	`ifdef MMU
	function Reg#(Bit#(`Reg_width)) read_supervisor_srw_registers(Bit#(8) addr);
		Reg#(Bit#(`Reg_width)) csr=(case(addr)
			`SSTATUS    :csr_sstatus; 
			`SEDELEG    :csr_sedeleg; 
			`SIDELEG    :csr_sideleg; 
			`SIE        :csr_sie; 
			`STVEC      :csr_stvec; 
			`SCOUNTEREN :csr_scounteren; 
			`SSCRATCH   :csr_sscratch; 
			`SEPC       :csr_sepc; 
			`SCAUSE     :csr_scause; 
			`STVAL      :csr_stval; 
			`SIP        :csr_sip; 
			`SATP       :csr_satp;
			default:readOnlyReg(0);
			endcase);
		return csr;
	endfunction
	`endif
	function Reg#(Bit#(`Reg_width)) read_machine_srw_registers(Bit#(8) address);
		Reg#(Bit#(`Reg_width)) csr=(case(address)
			`MSTATUS			:csr_mstatus;		
			`MISA				:csr_misa;
			`MEDELEG			:csr_medeleg;
			`MIDELEG			:csr_mideleg;
			`MIE				:csr_mie;
			`MTVEC			:csr_mtvec;
			`MCOUNTEREN		:csr_mcounteren;
			`MSCRATCH		:csr_mscratch;
			`MEPC				:csr_mepc;
			`MCAUSE			:csr_mcause;
			`MTVAL			:csr_mtval;
			`MIP				:csr_mip;
			`MPOWERCONTROL	:csr_power_control;
			`PMPCFG0		:csr_pmpcfg0;
		`ifndef RV64
			`PMPCFG1		:csr_pmpcfg1;
		`endif
			`PMPCFG2		:csr_pmpcfg2;
		`ifndef RV64
			`PMPCFG3		:csr_pmpcfg3;
		`endif
			default: begin
			`ifdef perf
				if(address>=`PMPADDRSTART && address<=`PMPADDREND) // lower 4 bits of the counter
					csr_pmpaddr[address[3:0]];
				else if(address>=`MHPMEVENTSTART && address<=`MHPMEVENTEND) // lower 32 bits of the counter
					csr_mhpmcounter[(address-3)[1:0]][1];
				else 
			`endif
					readOnlyReg(0);
			end
			endcase);
		return csr;
	endfunction
	function Reg#(Bit#(`Reg_width))read_machine_sro_registers(Bit#(8) address);
		Reg#(Bit#(`Reg_width)) csr=(case(address)
			`MVENDORID:csr_mvendorid;	
			`MARCHID	 :csr_marchid;
			`MIMPID	 :csr_mimpid;
			`MHARTID	 :csr_mhartid;
			`MBOOTSEQ : csr_boot_seq;
			default:readOnlyReg(0);
			endcase);
		return csr;
	endfunction

	function Reg#(Bit#(`Reg_width)) read_machine_counters(Bit#(8) address);
		Reg#(Bit#(`Reg_width)) csr=(case(address)
			`MCYCLE			:csr_mcycle[1];				
			`MINSTRET		:csr_minstret[1];
			`ifndef RV64
				`MCYCLEH		:csr_mcycleh[1];
				`MINSTRETH	:csr_minstreth[1];
			`endif
			default: begin
			`ifdef perf
				if(address>=`MHPMCOUNTSTART && address<=`MHPMCOUNTEND) // lower 32 bits of the counter
					csr_mhpmcounter[(address-3)[1:0]][1];
				else 
			`endif
					readOnlyReg(0);
			end
			endcase);
		return csr;
	endfunction
`ifdef Debug
	function Reg#(Bit#(`Reg_width)) read_debug_registers(Bit#(8) address);
		Reg#(Bit#(`Reg_width)) csr=(case(address)
			`DCSR: concatReg2(readOnlyReg(32'd0),dcsr);
			`DPC:	 csr_dpc;
			`DSCRATCH0: dscratch0;
			`DENTRY	 : debugentry;
			`TSELECT	 : tselect;
			`TDATA1	 : tdata1;
			`TDATA2	 : tdata2;
			default:readOnlyReg(0); 
		endcase);
		return csr;
	endfunction
`endif
  function Reg#(Bit#(`Reg_width)) read_csr(Bit#(12) addr);
   Reg#(Bit#(`Reg_width)) csr=(
    case(addr[11:8])
		'h0: read_user_srw_registers(truncate(addr)); // user standard read-write
		'hC: read_user_sro_registers(truncate(addr)); // user standard read-only
		`ifdef MMU 'h1: read_supervisor_srw_registers(truncate(addr)); `endif // supervisor read-write 
		'h3: read_machine_srw_registers(truncate(addr)); // machine standard read-write
		'hF: read_machine_sro_registers(truncate(addr)); // machine standard read-only
		'hB: read_machine_counters(truncate(addr)); // machine standard counters
		`ifdef Debug 'h7: read_debug_registers(truncate(addr)); `endif
      default: readOnlyReg(0);//read_perfcounter(address);
    endcase
    ); 
    return csr;
  endfunction
	
  function Bool hasCSRPermission(Bit#(12) address, Bool write);
    Bit#(12) csr_index = pack(address);
		Bool check_counter_permission = True;
		if(address >= 12'hB00 && address <= 12'hB1F) begin
			check_counter_permission = False;
			if(pack(rg_prv) == 3) 
				check_counter_permission = True;
			else if(pack(rg_prv) == 1 && csr_mcounteren[address[4:0]]==1)
				check_counter_permission = True;
			`ifdef MMU
			else if(pack(rg_prv) == 0 && csr_scounteren[address[4:0]]==1)
				check_counter_permission = True;
			`endif
		end
    return ((pack(rg_prv) >= csr_index[9:8]) && check_counter_permission && !(write && csr_index[11:10]==2'b11) );//(!write || (csr_index[11:10] != 2'b11))) || check_counter_permission);
  endfunction
   
  // if the operand is not 0 then the instruction will perform a write on the CSR.
	function Bool valid_csr_access(Bit#(12) csr_addr, Bit#(5) operand, Bit#(2) operation);
		Bool ret = hasCSRPermission(unpack(csr_addr), (operand != 0 || operation=='b01) ? True:False);
		return ret;
	endfunction

	function Bool address_valid(Bit#(12) csr_address);
		case(csr_address[11:8])
			'h0: begin
				if((csr_address[7:0]>'h5 && csr_address[7:0]<'h40) ||
				 (csr_address[7:0]>'h44))
					return False;
				else
					return True;
			end
			'h1:begin
				if((csr_address[7:0]==1)||
				 (csr_address[7:0]>'h6 && csr_address[7:0]<'h40) ||
				 (csr_address[7:0]>'h44 && csr_address[7:0]!='h80))
					return False;
				else 
					return True;
			end
			'h3: begin // machine read-write registers
				if((csr_address[7:0]>'h6 && csr_address[7:0]<'h23) || 
				  (csr_address[7:0]>'h26 && csr_address[7:0]<'h40) ||
				  (csr_address[7:0]>'h44 && csr_address[7:0]<='hA0) ||
				  (csr_address[7:0]>'hA3 && csr_address[7:0]<'hB8) ||
				  (csr_address[7:0]>'hbf))
					return False;
				else
					return True;
			end
			'h7: begin
				if((csr_address[7:0]<'hA0)||
				 (csr_address[7:0]>'hA3 && csr_address[7:0]<'hB0)||
				 (csr_address[7:0]>'hB2)) 
					return False; 
				else
					return True;
			end
			'hB:begin
				if((csr_address[7:0]>'h6 && csr_address[7:0]<'h80 && csr_address[7:0]!='h20)||
				 (csr_address[7:0]>'h86 && csr_address[7:0]<'hA0)||
				 (csr_address[7:0]>'hA6))
					return False;
				else
					return True;
			end
			'hC:begin
				if((csr_address[7:0]>'h6 && csr_address[7:0]<'h83)|| 
				 (csr_address[7:0]>'h86))
					return False; 
				else
					return True;
			end
			'hF:begin
				if(csr_address[7:0]<'h11 || csr_address[7:0]>'h15)
					return False;
				else
					return True;
			end
			default:return False;
		endcase
	endfunction

  rule increment_cycle_counter `ifdef Debug (stopcount==0) `endif ;
		`ifdef RV64
    	csr_mcycle[0]<=csr_mcycle[0]+1;
		`else
			Bit#(64) new_cycle={csr_mcycleh[0],csr_mcycle[0]);
			new_cycle=new_cycle+1;
			csr_mcycle[0]<=new_cycle[31:0];
			csr_mcycleh[0]<=new_cycle[63:32];
		`endif
  endrule
	

	// Check pending interrupts
	function ActionValue#(Trap_type) fn_chk_pending_interrupt(`ifdef Debug Bool haltreq, Bool resumereq , Bool resetreq`endif )=
	actionvalue
		`ifdef Debug
			Bit#(15) pending_debug_interrupt=0;
			if(haltreq && !debugmode_active[0] && !resetmode[0]) 
				pending_debug_interrupt[12]=1;	
			if(resumereq && debugmode_active[0] && !resetmode[0])
				pending_debug_interrupt[13]=1;
			if(resetreq && !resetmode[0] && debugmode_active[0])
				pending_debug_interrupt[14]=1;
		`endif
		Bit#(`Reg_width) lv_csr_mip = csr_mip;
		lv_csr_mip[11]=lv_csr_mip[11]|pack(rg_nmi);
		Bit#(15) pending_interrupts = (truncate(csr_mip)`ifdef Debug |pending_debug_interrupt `endif ) & truncate(csr_mie) ;
        `ifdef verbose $display("Pending_interrupts in the beginning csr_mip : %b pending_interrupt: %b", csr_mip, pending_interrupts); `endif
        // machine mode
		let pending_machine_interrupts = pending_interrupts & ~truncate(csr_mideleg);
		let machine_interrupts_enabled = (rg_mie == 1) || (pack(rg_prv) < pack(Machine));
		//supervisor mode
		`ifdef MMU
		let pending_supervisor_interrupts = pending_interrupts & truncate(csr_mideleg) & ~truncate(csr_sideleg);
		let supervisor_interrupts_enabled = (rg_sie == 1) && (pack(rg_prv) <= pack(Supervisor));
		`endif
		// user mode
		// combined
		pending_interrupts =	(machine_interrupts_enabled ? pending_machine_interrupts : 0)
										`ifdef MMU	|(supervisor_interrupts_enabled ? pending_supervisor_interrupts : 0) `endif ;
										
		// format pendingInterrupt value to return
		Trap_type ret = tagged None;
		if (pending_interrupts != 0) begin
			ret = tagged Interrupt unpack(zeroExtend(pack(countZerosLSB(pending_interrupts))));
		end
		`ifdef verbose $display("Debug interrupts: %h pending_interrupt: %h csr_mie: %h rg_mie: %b ret: ",`ifdef Debug pending_debug_interrupt `else 0 `endif ,pending_interrupts,csr_mie,rg_mie,fshow(ret)); `endif
		return ret;
	endactionvalue;

	
	method ActionValue#(Tuple2#(Bit#(3),Trap_type)) check_for_trap(`ifdef Debug Bool haltreq, Bool resumereq, Bool resetreq, `endif Bit#(`VADDR) pc, Bit#(32) instruction)if(!rg_initialize[1] `ifdef simulate && !wr_endsimulation `endif ); 
		Trap_type trap_type=tagged None;
		Bit#(3) lv_debugcause=0;
		let opcode=instruction[6:2];
		if(opcode==`CSR_op)begin
			case(instruction[14:12])
				'd0:case (instruction[31:20])
					'h000: // ECALL
						trap_type=tagged Exception(case(rg_prv) User: Ecall_from_user; Supervisor:Ecall_from_supervisor;Machine:Ecall_from_machine;endcase); 
						'h001:begin // EBREAK
						`ifdef Debug
							Bit#(4) ebreak={ebreakm,0,ebreaks,ebreaku};
							if((ebreak)[pack(rg_prv)]==1)
								trap_type=tagged Interrupt DebugInterrupt;
							else
						`endif
								trap_type=tagged Exception Breakpoint;
							lv_debugcause=1;
						end
					'h102:begin // SRET
						if(pack(rg_prv)<pack(Supervisor)) trap_type=tagged Exception Illegal_inst;
					end
					'h302:begin // MRET
						if(pack(rg_prv)<pack(Machine)) trap_type= tagged Exception Illegal_inst;
					end
					endcase
				default: begin
					Bool address_is_valid=address_valid(instruction[31:20]);
					Bool access_is_valid=valid_csr_access(instruction[31:20],instruction[19:15], instruction[13:12]);
					`ifdef verbose $display($time,"\tCSR: Address: %h AddressValid: %b AccessValid: %b",instruction[31:20],address_is_valid,access_is_valid); `endif
					if(!(address_is_valid && access_is_valid)) 
						trap_type=tagged Exception Illegal_inst;
				end
			endcase
		end
		if(opcode==`FMADD_op || opcode==`FNMSUB_op || opcode==`FNMADD_op || opcode==`FMSUB_op || opcode==`FLOAT_op)begin
			if(instruction[14:12]=='b111 && rg_frm>4)
				trap_type=tagged Exception Illegal_inst;
		end
		if((opcode[4:3]=='b10 || opcode==`FSTORE_op || opcode==`FLOAD_op) && rg_fs==0)
			trap_type=tagged Exception Illegal_inst;
		`ifdef Debug
			if(checktrigger(executetrigger,pc,instruction))begin
				`ifdef verbose $display("TRAP: Trigger Fired Debug Interupt"); `endif
				trap_type=tagged Exception Breakpoint;
				lv_debugcause=2;
			end
		`endif
		let pending_interrupt <- fn_chk_pending_interrupt(`ifdef Debug haltreq,resumereq, resetreq `endif );// TODO but resume request here
		if(pending_interrupt matches tagged Interrupt .interrupt) begin
			`ifdef verbose $display($time,"\tinterrupt injected in to pipeline"); `endif
			trap_type=tagged Interrupt interrupt;
			`ifdef Debug 
				if(interrupt ==DebugInterrupt)
					lv_debugcause=(step==1)?4:3;
			`endif
    	end
		return tuple2(lv_debugcause,trap_type);
	endmethod

	method ActionValue#(Tuple4#(Bool,Bit#(`VADDR),Bit#(`Reg_width),Bool)) system_instruction(WriteBackType wbdata ,Bit#(`VADDR) pc, Bit#(`PERFMONITORS) perfmonitor_incr  `ifdef simulate , Bit#(32) instruction, Operand_type rd_type, Bit#(5) destination `endif )if(!rg_initialize[1] `ifdef simulate && !wr_endsimulation `endif );
		Bool flush=True; // TODO flush for only writting on certain csr registers
		Bool commit=False;
		Bit#(`VADDR) jump_address=pc+4;
		Bit#(`Reg_width) destination_value=0;
		/*====== Execute the current instruction and generate a halt interrupt on the next ===== */
		`ifdef Debug
		`ifdef verbose $display($time,"CSR: STEP_NOW : %b",rg_step_now); `endif
		if(step==1 && !debugmode_active[0] && !rg_step_now)
			rg_step_now<=True;
		else 
			rg_step_now<=False;
		`endif
		/*======================================================================================= */
		if(wbdata matches tagged SYSTEM .csr)begin
			let csr_reg=read_csr(csr.csr_address);
			if(csr.funct3==0)begin
				case (csr.csr_address[11:8]) matches
					'h3:begin  // MRET
						Privilege_mode next_prv =unpack(rg_mpp);
    		          rg_mpie <= 1;
    		          rg_mpp <= pack(User);
    		          rg_prv <= next_prv;
    		          jump_address=truncate(csr_mepc);
						 rg_mie<=rg_mpie;
					end
					`ifdef MMU
						'h1:begin  // SRET
							if(csr.csr_address[5]==0)begin
	    		      	    Privilege_mode next_prv =unpack({1'b0,rg_spp});
				      	    rg_spie <= 1;
				      	    rg_spp <= pack(User)[0];
					   	    rg_prv <= next_prv;
							    jump_address=truncate(csr_sepc);
								 rg_sie<=rg_spie;
							end
							else begin // SFENCE
								jump_address=pc+4;
								`ifdef simulate
									Bit#(64) pc1=signExtend(pc[38:0]);
									$fwrite(dump, rg_prv," 0x%16h",pc1, " (0x%8h", instruction,")" ); 
									$fwrite(dump," x%d",destination," 0x%16h",destination_value,"\n"); 
								`endif
								`ifdef Debug 
									if(resetmode[0])
										resetmode[0]<=False;
								`endif
							end
    		      	end
					`endif
					`ifdef Debug
						'h7:begin // DRET
							jump_address=dpc;
							rg_prv<=unpack(debugprv);
							debugmode_active[0]<=False;
						end
					`endif
				endcase
				`ifdef Debug 
					if(stopcount==0 && !debugmode_active[0]) begin
						csr_minstret[0]<=csr_minstret[0]+1;
					end
				`endif
				for(Integer i=0;i<=(`MHPMCOUNTEND-`MHPMCOUNTSTART);i=i+1)
					if((csr_mhpmevent[i]&perfmonitor_incr)!=0 `ifdef Debug && stopcount==0 `endif )
						csr_mhpmcounter[i][1]<=csr_mhpmcounter[i][1]+1;
				`ifdef verbose 
				for(Integer i=0;i<=(`MHPMCOUNTEND-`MHPMCOUNTSTART);i=i+1)begin
					$display($time,"\tEVENT: :%h %s",csr_mhpmevent[i],event_name(csr_mhpmevent[i])," : %d",csr_mhpmcounter[i][1]);
				end
				`endif
			end
			else begin
    		   destination_value=csr_reg; 
				`ifdef verbose $display($time,"\tCSR: Dest: %h Value: %h rs1: %h funct3: %d rs1_addr: %d",csr.csr_address,csr_reg,csr.rs1,csr.funct3,csr.rs1_addr); `endif
				commit=True;
    		   case(csr.funct3)
					'd1: csr_reg <= csr.rs1;					// CSRRW
    		   	'd2:if(csr.rs1_addr!=0)  csr_reg <= csr.rs1 | csr_reg;			// CSRRS 
    		   	'd3:if(csr.rs1_addr!=0)  csr_reg <= ~(csr.rs1) & csr_reg;    //	CSRRC 
    		   	'd5: csr_reg <= zeroExtend(csr.rs1_addr);				// CSRRWI 
    		   	'd6:if(csr.rs1_addr!=0)  csr_reg <= zeroExtend(csr.rs1_addr) | csr_reg;		// CSRRSI 
    		   	'd7:if(csr.rs1_addr!=0)  csr_reg <= ~(zeroExtend(csr.rs1_addr)) & csr_reg; //CSRRCI 
    		   endcase
				`ifdef simulate
					Bit#(64) pc1=signExtend(pc[38:0]);
					$fwrite(dump, rg_prv," 0x%16h",pc1, " (0x%8h", instruction,")" ); 
					$fwrite(dump," x%d",destination," 0x%16h",destination_value); 
					$fwrite(dump,"\n");
				`endif
    		end
		end
		else if(wbdata matches tagged RESULT .res)begin
			for(Integer i=0;i<=(`MHPMCOUNTEND-`MHPMCOUNTSTART);i=i+1)
				if((csr_mhpmevent[i]&perfmonitor_incr)!=0 `ifdef Debug && stopcount==0 `endif )
					csr_mhpmcounter[i][1]<=csr_mhpmcounter[i][1]+1;
				`ifdef verbose 
				for(Integer i=0;i<=(`MHPMCOUNTEND-`MHPMCOUNTSTART);i=i+1)begin
					$display($time,"\tEVENT: :%h %s",csr_mhpmevent[i],event_name(csr_mhpmevent[i])," : %d",csr_mhpmcounter[i][1]);
				end
				`endif
			`ifdef simulate
				Bit#(64) pc1=signExtend(pc[38:0]);
				$fwrite(dump, rg_prv," 0x%16h",pc1, " (0x%8h", instruction,")" ); 
			`endif
			commit=True;
			`ifdef Debug 
				if(stopcount==0 && !debugmode_active[0])begin
					csr_minstret[0]<=csr_minstret[0]+1;
				end
			`endif
			flush=False;
			destination_value=res.aluresult;
			let newfflags=res.fflags;
			let fpudirty=False;
			if((newfflags|rg_fflags)!=rg_fflags)begin
				rg_fflags<=newfflags|rg_fflags;
				fpudirty=True;
			end
			if(fpudirty)
				if(rg_fs==2'b0)begin
					`ifdef verbose $display("Error: FPU id Dirty and FX field is 0"); `endif
	 	  		end
			`ifdef simulate
				Bit#(64) dat=signExtend(res.aluresult);
				`ifdef spfpu
					if(rd_type==FloatingRF)
					 	`ifdef dpfpu
					 		$fwrite(dump," f%d",destination," 0x%16h",dat); 
						`else
							$fwrite(dump," f%d",destination," 0x%16h",{32'hffffffff,dat[31:0]}); 
						`endif
					else
				`endif
					$fwrite(dump," x%d",destination," 0x%16h",dat); 
				$fwrite(dump,"\n");
			`endif
		end
		return tuple4(flush,jump_address,destination_value, commit);
	endmethod

	method ActionValue#(Tuple2#(Bit#(`VADDR), Bool)) take_trap(Trap_type exception, Bit#(3) lv_debugcause, Bit#(`VADDR) pc, Bit#(`VADDR) badaddr)if(!rg_initialize[1]);
		Bit#(`VADDR) jump_address=0;
		Bool flush=True;
		if(exception matches tagged Exception .ex)begin
			if(ex==Inst_addr_misaligned || ex==Inst_access_fault || ex==Inst_pagefault)
				badaddr=pc;
			else if(ex==Illegal_inst)
				badaddr=0;
      else badaddr=0;
		end
    else
      badaddr=0;
		`ifdef verbose $display($time,"\tTrap Type: ",fshow(exception)," debugcause: %d",lv_debugcause," BaddAddr: %h",badaddr);  `endif
		`ifdef Debug
		if(exception matches tagged Interrupt .in &&& in==DebugResume)begin
			if(debugmode_active[0])begin
				rg_prv<=unpack(debugprv);
				debugmode_active[0]<=False;
				jump_address=truncate(dpc);
			end
		end
		else if(exception matches tagged Interrupt .in &&& in==DebugInterrupt)begin
			debugmode_active[0]<=True;
			if(!debugmode_active[0])begin
				dpc<=pc;
				debugcause<=lv_debugcause;
				debugprv<=pack(rg_prv);
				rg_prv<=Machine;
				if(lv_debugcause==4)
					rg_step_now<=False;
			end
			jump_address=truncate(debugentry);
		end
		else if(exception matches tagged Interrupt .in &&& in==DebugReset)begin
			resetmode[0]<=True;
			jump_address='h1000;
			rg_prv<=Machine;
		end
		else if(!debugmode_active[0] && !resetmode[0])begin
		`endif
			Bit#(`Reg_width) cause = 0;
			Bit #(TSub #(`Reg_width, 1)) cause_code = 0;
			Bit #(1) cause_type = 0;
			case (exception) matches
				tagged Interrupt .i: begin cause_type = 1; cause_code = zeroExtend(pack(i)); end
				tagged Exception .e: begin cause_type = 0; cause_code = zeroExtend(pack(e)); 
					`ifdef simulate if(e==Endsimulation) begin
						for(Integer i=0;i<=(`MHPMCOUNTEND-`MHPMCOUNTSTART);i=i+1)begin
							$display($time,"\tEVENT: %s",event_name(csr_mhpmevent[i])," : %d",csr_mhpmcounter[i][1]);
						end
						$finish(0) /*wr_endsimulation <=True*/ ;  
						end
					`endif 
				end
			endcase
			cause = {cause_type, cause_code};
			`ifdef MMU
			Bool delegToS = (pack(rg_prv) <= pack(Supervisor)) && (case (exception) matches
				tagged Exception .exceptionCause:begin (((csr_medeleg >> pack(exceptionCause)) & 1) != 0);end
    	      tagged Interrupt .interruptCause: (((csr_mideleg >> pack(interruptCause)) & 1) != 0);
    	   endcase);
			if(delegToS)begin
				//if(exception matches tagged Exception .ex)
				//	if(ex==Inst_addr_misaligned || ex==Inst_access_fault || ex==Inst_pagefault || ex==Illegal_inst 
				//											|| ex==Load_access_fault || ex==Load_addr_misaligned || ex==Load_pagefault
				//															|| ex==Store_addr_misaligned || ex==Store_access_fault || ex==Store_pagefault)
				csr_stval<=zeroExtend(badaddr);
				csr_sepc<=signExtend(pc);
				csr_scause<=cause;
				rg_spp <= pack(rg_prv)[0];
				rg_sie <=0;
				rg_spie <= rg_sie;//(case (rg_prv)  User: rg_uie;  Supervisor : rg_sie;  endcase);
				jump_address=truncate(csr_stvec);
				rg_prv <= Supervisor;
			end
			else begin
			`endif
				rg_prv <= Machine;
				if(exception matches tagged Exception .ex)
					if(ex==Inst_addr_misaligned || ex==Inst_access_fault || ex==Inst_pagefault || ex==Illegal_inst 
															|| ex==Load_access_fault || ex==Load_addr_misaligned || ex==Load_pagefault
																			|| ex==Store_addr_misaligned || ex==Store_access_fault || ex==Store_pagefault)
				csr_mtval<=zeroExtend(badaddr);
				csr_mepc<=signExtend(pc);
				csr_mcause<=cause;
				rg_mie <= 0;
				rg_mpp <= pack(rg_prv);
				jump_address=truncate(csr_mtvec);
				rg_mpie <= rg_mie;//(case (rg_prv)  User: rg_uie; `ifdef MMU Supervisor : rg_sie; `endif Machine: rg_mie;  endcase);
			`ifdef MMU
			end
			`endif

		`ifdef Debug 
			end
			else begin
				flush=False;
			end
		`endif
		return tuple2(jump_address,flush);
	endmethod
	method Bit#(3) roundingmode if(!rg_initialize[1] `ifdef simulate && !wr_endsimulation `endif );
		return rg_frm;
	endmethod
	method Action set_external_interrupt(Tuple2#(Bool,Bool) ex_i) if(!rg_initialize[1] `ifdef simulate && !wr_endsimulation `endif );
		let {i,j} = ex_i;
		rg_nmi <= j;
		if(rg_prv == Machine) begin
  	 	`ifdef verbose $display("CSR : Machine external interrupt pending"); `endif
			rg_meip <= pack(i);
		end
		else if(rg_prv == Supervisor) begin
			rg_seipe <= pack(i);
		end
		else if(rg_prv == User) begin
			rg_ueipe <= pack(i);
		end
	endmethod
	method Action flush;
		rg_initialize[0]<=True;
	endmethod
	`ifdef MMU
	method Bit#(`Reg_width) send_satp;
		return csr_satp;
	endmethod
	method Chmod perm_to_TLB;
		return Chmod {mprv : rg_mprv, sum : rg_sum, mxr : rg_mxr, mpp : unpack(rg_mpp), prv : rg_prv};
	endmethod
	`endif
	method Bit#(`Reg_width) mmu_cache_disable;
		return csr_memse;
	endmethod
	`ifdef Debug
		method Bool halted;
			return debugmode_active[1];
		endmethod
		method load_triggerdata=loadtrigger;
		method store_triggerdata=storetrigger;
		method ActionValue#(Bit#(`Reg_width)) rw_debug_csr(Bit#(12) r, Bool write, Bit#(`Reg_width) data) if(!rg_initialize[1]);
			let y=read_csr(r);
			if(write)
				y<=data;
			return y._read;
		endmethod
		method Bool step_now=rg_step_now;
		method Bool reset_mode=resetmode[1];
	`endif
	method Bit#(`Reg_width) misa=csr_misa._read;
	method Action boot_sequence(Bit#(1) bootseq);
		rg_boot_seq<=bootseq;
	endmethod
	method Bit#(2) powercontrol=power_control_out;
	method Action poweracknowledge(Bit#(2) pa);
		power_control_in<=pa;
	endmethod
	`ifdef CLINT
		method Action clint_msip(Bit#(1) intrpt);
			rg_msip<=intrpt;
		endmethod
		method Action clint_mtip(Bit#(1) intrpt);
			rg_mtip<=intrpt;
		endmethod
		method Action clint_mtime(Bit#(`Reg_width) c_mtime);
			rg_clint_mtime<=c_mtime;
		endmethod
	`endif
  endmodule
endpackage

