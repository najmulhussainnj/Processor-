package DebugModule;
	/*====== Package imports === */
	import FIFO::*;
	import FIFOF::*;
	import SpecialFIFOs::*;
	import Semi_FIFOF::* ;
	import GetPut::*;
	import ClientServer::*;
	import Vector::*;
	import BUtils::*;
	import ConfigReg::*;
	import DReg::*;
	/*========================== */
	/*=== Project imports === */
	import ConcatReg::*;
	import AXI4_Types::*;
	import AXI4_Fabric::*;
	`include "defines.bsv"
	`include "defined_parameters.bsv"
	import defined_types::*;
	import core			:: *;
	/*========================= */

	function Reg#(t) readOnlyReg(t r);
		return (interface Reg;
			method t _read = r;
			method Action _write(t x) = noAction;
		endinterface);
  	endfunction
	interface Ifc_DebugModule;
		method Action set_external_interrupt(Tuple2#(Bool,Bool) i);
		method Action request_from_dtm(Bit#(40) requestfrmDTM);
		method ActionValue#(Bit#(34)) response_to_dtm;
		interface AXI4_Master_IFC#(`PADDR, `Reg_width, `USERSPACE) debug_master;
		interface AXI4_Slave_IFC#(`PADDR, `Reg_width, `USERSPACE) debug_slave;
		/*======= Core related interfaces ====== */
		interface AXI4_Master_IFC#(`PADDR, `Reg_width, `USERSPACE) imem_master;
		interface AXI4_Master_IFC#(`PADDR, `Reg_width, `USERSPACE) dmem_master;
		(*always_ready,always_enabled*)
		method Action boot_sequence(Bit#(1) bootseq);
		`ifdef CLINT
			method Action clint_msip(Bit#(1) intrpt);
			method Action clint_mtip(Bit#(1) intrpt);
			method Action clint_mtime(Bit#(`Reg_width) c_mtime);
		`endif
		/*===========================================*/
	endinterface

	// this function is used to generate a resume request only when written by the
	// debugger
	function Reg#(Bit#(1)) condwriteSideEffect(Reg#(Bit#(1)) r, Action a);
		return (interface Reg;
            method Bit#(1) _read = r._read;
            method Action _write(Bit#(1) x);
                r._write(x);
					 if(x==1)
						a;
            endmethod
        endinterface);
	endfunction
	
	(*synthesize*)
	module mkDebugModule#(Bit#(`VADDR) reset_vector)(Ifc_DebugModule);
		// Instantiating the Core
		Ifc_core_AXI4 c64 <-mkcore_AXI4(reset_vector);
		// Instantiating the AXI4 Master
		AXI4_Master_Xactor_IFC #(`PADDR,`Reg_width,`USERSPACE)  master_xactor <- mkAXI4_Master_Xactor;
		AXI4_Slave_Xactor_IFC #(`PADDR, `Reg_width, `USERSPACE) slave_xactor <- mkAXI4_Slave_Xactor;

		/*========= FIFOs to communicate with the DTM==== */
		FIFOF#(Bit#(34)) response_to_DTM <-mkFIFOF();
		/*================================================ */

		/*======= HART INFO REGISTERS =========== */
		Reg#(Bit#(4)) nscratch =readOnlyReg(4'd2);
		Reg#(Bit#(1)) dataaccess = readOnlyReg(1'b1);
		Reg#(Bit#(4)) datasize = readOnlyReg(4'd12);
		Reg#(Bit#(12)) dataaddr = readOnlyReg(12'h4c);
		Reg#(Bit#(32)) hart_info =concatReg6(readOnlyReg(8'b0),
		nscratch,readOnlyReg(3'b0),dataaccess,datasize,dataaddr);
		/*======================================== */
		//Registers from debug spec .13 start here 
    /*========= DM Control Registers ======== */
		Reg#(Bit#(1)) haltreq<-mkReg(0);
		Reg#(Bool) wr_gen_haltreq<-mkDReg(False);
		Reg#(Bit#(1)) rg_haltreq=condwriteSideEffect(haltreq,wr_gen_haltreq._write(True));
		Reg#(Bit#(1)) resume<-mkReg(0);
		Reg#(Bool) wr_gen_resumereq<-mkDReg(False);
		Reg#(Bit#(1)) rg_resumereq=condwriteSideEffect(resume,wr_gen_resumereq._write(True));
		Reg#(Bit#(1)) rg_hartreset<-mkReg(0);
		Reg#(Bit#(1)) rg_reset<-mkReg(0);
		Reg#(Bit#(1)) rg_dmactive<-mkReg(1'b0);
		Reg#(Bit#(10)) rg_hartsel<-mkReg(0);
		Reg#(Bit#(32)) dm_control =concatReg9(rg_haltreq,rg_resumereq,
        rg_hartreset,readOnlyReg(2'b0),readOnlyReg(1'b0),rg_hartsel,
        readOnlyReg(14'b0),rg_reset,rg_dmactive);
		/*======================================== */

		/*====== DM STATUS REGISTERS ========== */
    	Reg#(Bit#(1)) rg_resumeack[2]<-mkCReg(2,0); // This is the Resume ACK
    	Reg#(Bit#(1)) rg_unavailable<-mkReg(0);
    	Reg#(Bit#(1)) rg_running <-mkReg(0);
    	Reg#(Bit#(1)) rg_halted<-mkReg(0);
		ConfigReg#(Bit#(1)) rg_nonexistent<-mkConfigReg(0);
		Reg#(Bit#(32)) dm_status =concatReg12(readOnlyReg(14'd0),
        readOnlyReg(rg_resumeack[1]),readOnlyReg(rg_resumeack[1]),
        readOnlyReg(rg_nonexistent),readOnlyReg(rg_nonexistent), readOnlyReg(rg_unavailable),readOnlyReg(rg_unavailable),
        readOnlyReg(rg_running), readOnlyReg(rg_running),
        readOnlyReg(rg_halted), readOnlyReg(rg_halted),
        readOnlyReg(8'b10000010)); // TODO Make the 4th bit if using Configstring
		/*======================================== */
		
		/*== ABSTRACT REGISTERS === */
    	Reg#(Bit#(1)) rg_busy[3]<-mkCReg(3,0);
    	Reg#(Bit#(3)) rg_cmderr<-mkReg(0);
    	Reg#(Bit#(32)) abstract_Control_And_Status=concatReg8(readOnlyReg(3'b0),
        readOnlyReg(5'd16),readOnlyReg(11'b0),
        readOnlyReg(rg_busy[2]),readOnlyReg(1'b0),rg_cmderr,
        readOnlyReg(3'b0),readOnlyReg(5'd12));

		Reg#(Bit#(8)) rg_cmdtype <-mkReg(0);
    	Reg#(Bit#(24)) rg_control<-mkReg(0);
		Reg#(Bit#(32)) abstract_Commands =concatReg2(rg_cmdtype,rg_control); //

		Reg#(Bit#(16)) rg_autoexecprogbuf<-mkReg(0);
    	Reg#(Bit#(12)) rg_autoexecdata<-mkReg(0);
		Reg#(Bit#(32)) abstract_Command_Autoexe =concatReg3(rg_autoexecprogbuf,readOnlyReg(4'b0),rg_autoexecdata) ;
		Vector#(12,Array#(Reg#(Bit #(32)))) abstract_Data <- replicateM(mkCReg(2,0));
		/*======================================== */


		//Reg#(Bit#(32)) configuration_String_Addr_0<-mkReg (0) ; // TODO need to fiure this out
		//Reg#(Bit#(32)) configuration_String_Addr_1<-mkReg (0) ;
		//Reg#(Bit#(32)) configuration_String_Addr_2<-mkReg (0) ;
		//Reg#(Bit#(32)) configuration_String_Addr_3<-mkReg (0) ;
		//Reg#(Bit#(32)) serial_Control_And_Status <-mkReg (0) ;
		//Reg#(Bit#(64)) serial_Data <-mkReg(0) ;
    /*======= System Bus Access Registers ======= */
		Reg#(Bit#(3)) rg_sberror<-mkReg(0);
		Reg#(Bit#(1)) rg_sbautoread<-mkReg(0);
		Reg#(Bit#(1)) rg_sbautoincrement<-mkReg(0);
		Reg#(Bit#(3)) rg_sbaccess<-mkReg(0);
		Reg#(Bit#(1)) rg_sbsingleread[2]<-mkCReg(2,0);
		Reg#(Bit#(32)) bus_ctrlstatus=concatReg8(readOnlyReg(11'b0),
				rg_sbsingleread[1],rg_sbaccess,
				rg_sbautoincrement,rg_sbautoread,
				rg_sberror,readOnlyReg(7'h40),
				readOnlyReg(5'b01111));

		Reg#(Bit#(32)) busAddr0[2] <- mkCReg(2,0) ;
		Reg#(Bit#(32)) busData0[2] <- mkCReg(2,0) ;
		Reg#(Bit#(32)) busData1[2] <- mkCReg(2,0) ;
		/*======================================== */
		Vector#(16,Array#(Reg#(Bit #(32) ))) program_Buffer <- replicateM(mkCReg(2,0)) ;

		//not in spec , internally maintained for write
		Reg#(Bool) write_flag[2] <- mkCReg(2,False) ;
		Reg#(Bool) reset_status <-mkReg(True) ; // not in spec internally maintained for reset
		Reg#(Bit#(32)) instruction0<-mkReg('h0000100f); // 0x0000100f fence.i
		Reg#(Bit#(32)) instruction1<-mkReg('h00000013); // 0x00000013 nop (addi x0,x0,0)
		Reg#(Bit#(32)) instruction2<-mkReg('hffdff06f); // 0xffdff06f jump to nop
		Reg#(Bit#(32)) instruction3<-mkReg('h0040006f); // jump to progbuffer
		Reg#(Bool) prog_ebreak_read<-mkReg(False);
		Reg#(Bool) perform_reg_access[2] <-mkCReg(2,False);
		Reg#(Bool) start_program_exec[3] <-mkCReg(3,False);
	// 0 0 0 0100 0 0 0000 0110 1111
		rule generate_nonexistent;
			rg_nonexistent<=rg_hartsel==0?0:1;
		endrule
		rule rl_rd_respond;
			// Get the wr request
			let ar<- pop_o(slave_xactor.o_rd_addr);
			AXI4_Rd_Data#(`Reg_width,`USERSPACE) r = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: ? ,rlast:True,ruser:0,rid:ar.arid};
			`ifdef verbose 
				$display($time,"\tDEBUG:Address:%h size: %d",ar.araddr,ar.arsize);
				$display($time,"\tDEBUG:Abstract Command: %h,%h,%h,start_program_exec %h",ar.araddr,rg_control[18] ,rg_busy[1],start_program_exec[0]);
			`endif
			if(ar.araddr>= (`DebugBase + 'h4c) && ar.araddr<=`DebugEnd)begin
				Bit#(4) index=truncate((ar.araddr-'h4c)>>2);
				`ifdef verbose $display($time,"\tDEBUG: Reading abstract data %d",index); `endif
				if(ar.arsize==3)begin
					r.rdata={abstract_Data[index+1][0],abstract_Data[index][0]};
				end
				else if(ar.arsize==2)begin
					r.rdata=duplicate(abstract_Data[index][0]);
				end
				else
					r.rresp=AXI4_SLVERR;
			end
			else if(ar.araddr>= (`DebugBase + 12) && ar.araddr<= `DebugBase+'h48)
			begin
				let index=(ar.araddr[5:2]-3);
				`ifdef verbose $display($time,"\tDEBUG: Reading program buffer %d",index); `endif
				Bit#(`Reg_width) data0={program_Buffer[index+1][0],program_Buffer[index][0]};
				if(ar.arsize==3)begin
					r.rdata={program_Buffer[index+1][0],program_Buffer[index][0]};
				end
				else if(ar.arsize==2)begin // 32 bit
					r.rdata=duplicate(program_Buffer[index][0]);
				end
      		else if (ar.arsize=='d1)begin // half_word
					if(ar.araddr[`byte_offset:0] ==0)
						r.rdata = duplicate(data0[15:0]);
					else if(ar.araddr[`byte_offset:0] ==2)
						r.rdata = duplicate(data0[31:16]);
					`ifdef RV64
						else if(ar.araddr[`byte_offset:0] ==4)
							r.rdata = duplicate(data0[47:32]);
						else if(ar.araddr[`byte_offset:0] ==6)
							r.rdata = duplicate(data0[63:48]);
					`endif
      		end
      		else if (ar.arsize=='d0) begin// one byte
					if(ar.araddr[`byte_offset:0] ==0)
      		  	  r.rdata = duplicate(data0[7:0]);
      		  	else if(ar.araddr[`byte_offset:0] ==1)
      		  	  r.rdata = duplicate(data0[15:8]);
      		  	else if(ar.araddr[`byte_offset:0] ==2)
      		  	  r.rdata = duplicate(data0[23:16]);
      		  	else if(ar.araddr[`byte_offset:0] ==3)
      		  	  r.rdata = duplicate(data0[31:24]);
				  	`ifdef RV64
      		  		else if(ar.araddr[`byte_offset:0] ==4)
							r.rdata = duplicate(data0[39:32]);
      		  		else if(ar.araddr[`byte_offset:0] ==5)
							r.rdata = duplicate(data0[47:40]);
      		  		else if(ar.araddr[`byte_offset:0] ==6)
							r.rdata = duplicate(data0[55:48]);
      		  		else if(ar.araddr[`byte_offset:0] ==7)
							r.rdata = duplicate(data0[63:56]);
					`endif
      		end
				if((program_Buffer[index][0][14:12]==`EBREAK_f3 && program_Buffer[index][0][6:2]==`CSR_op))
				begin
					start_program_exec[0]<=False;
					`ifdef verbose $display($time,"\tDEBUG: EBREAK encountered"); `endif
				end
			end
			else if(ar.araddr==`DebugBase && ar.arsize==2)
				r.rdata=duplicate(instruction0);
			else if(ar.araddr==`DebugBase+4 && ar.arsize==2)
				r.rdata=duplicate(instruction1);
			else if(ar.araddr==`DebugBase+8 && rg_control[18] == 1 && start_program_exec[0]) // postexec and busy
			begin
				r.rdata=duplicate(instruction3);
				`ifdef verbose $display($time,"\tDEBUG :Redirecting to program buffer"); `endif
			end
			else if(ar.araddr==`DebugBase+8 && ar.arsize==2)begin
				r.rdata=duplicate(instruction2);
				rg_busy[1]<=0;
			end
			else 
				r.rresp=AXI4_SLVERR;
			`ifdef verbose $display($time,"\tDEBUG: Reading Address:%h Data: %h",ar.araddr,r.rdata); `endif
			slave_xactor.i_rd_data.enq(r);
		endrule

		rule rl_wr_response(!perform_reg_access[0]);
			let aw <- pop_o (slave_xactor.o_wr_addr);
     		let w  <- pop_o (slave_xactor.o_wr_data);
	   	let b = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: aw.awuser, bid:aw.awid};
			if(aw.awaddr>= (`DebugBase + 'h4c) && aw.awaddr<=`DebugEnd)begin
				Bit#(4) index=truncate((aw.awaddr-'h4c)>>2);
				if(aw.awsize==3)begin
					abstract_Data[index+1][0]<=w.wdata[63:32];
					abstract_Data[index][0]<=w.wdata[31:0];
				end
				else if(aw.awsize==2)begin
					if(w.wstrb=='hf0)
						abstract_Data[index+1][0]<=w.wdata[63:32];
					else if(w.wstrb=='h0f)
						abstract_Data[index][0]<=w.wdata[31:0];
				end
				else
				 b.bresp=AXI4_SLVERR;
			end
			else if(aw.awaddr>= (`DebugBase + 12) && aw.awaddr<= (`DebugBase+'h48))
			begin
				//let index=(aw.awaddr[7:0]-8)>>2;
				let index=(aw.awaddr[5:2]-3);
				if(w.wstrb=='hf0)
					program_Buffer[index+1][0]<=w.wdata[63:32];
				else if(w.wstrb=='h0f)
					program_Buffer[index][0]<=w.wdata[31:0];
				else if(w.wstrb=='hff)begin
					program_Buffer[index][0]<=w.wdata[31:0];
					program_Buffer[index+1][0]<=w.wdata[63:32];
				end
			end
			else 
				b.bresp=AXI4_SLVERR;
      	slave_xactor.i_wr_resp.enq (b);
		endrule
		

		(*conflict_free = "access_register_or_pc,access_csrs"*)

    /*=== Rules to halt the CPU === */
		rule halt_request(wr_gen_haltreq && rg_halted == 0);	
			`ifdef verbose $display($time,"\tDEBUG: Requesting Halt"); `endif
			c64.stop;
    	endrule

		`ifdef verbose 
		rule display_Stuff;
			`ifdef verbose $display($time,"\tDEBUG: HALT: %b",rg_halted); `endif
		endrule
		`endif

    	rule halt_status_of_cpu;
			rg_halted <= pack(c64.halted) ;
		endrule

		rule resume_status(!wr_gen_resumereq);
			if(resume==1 && !c64.halted)
				rg_resumeack[0] <= 1;
    	endrule

		rule resume_request(wr_gen_resumereq);
			`ifdef verbose $display($time,"\tDEBUG: Requesting Resume\n"); `endif
			// insert set for resume request set to zero here
			rg_resumeack[0]<= 1'b0; // assert low on request
			c64.run_continue;
		endrule
		

		rule reset(rg_hartreset == 1 && reset_status);			
			c64.reset;						
			`ifdef verbose $display($time,"\tDEBUG: Requesting Reset"); `endif
		endrule

		rule rst_status;
			reset_status <= c64.reset_complete ;
		endrule
		rule access_register_or_pc(rg_halted == 1 && perform_reg_access[0]  && rg_control[12]==1);
    		`ifdef verbose $display($time,"\tDEBUG: Access CONTROL: %h rg_halted: %b REGISTER NO.: %h",rg_control,rg_halted,abstract_Commands[15:0]); `endif
    		if(rg_control[16] == 0 && rg_control[17] == 1)// READ Operation 
    		begin
    			`ifdef verbose $display($time,"\tREAD REGISTER OPERATION "); `endif
    			Bit#(`Reg_width) read_data =0;
				if(rg_control[5] ==0)
				begin
					read_data = c64.read_igpr(rg_control[4:0]) ;
					`ifdef verbose $display($time,"\tReading IGPR %d val: %h",rg_control[4:0],read_data); `endif
				end
				`ifdef spfpu
				else 
				begin
					read_data = c64.read_fgpr(rg_control[4:0]) ;
					`ifdef verbose $display($time,"\tReading FGPR %d val: %h",rg_control[4:0],read_data); `endif
				end
				`endif
				if(rg_control[22:20] == 2)begin
					abstract_Data[0][0] <= read_data[31:0] ;    		
				end				
				else if(rg_control[22:20] == 3)begin
					abstract_Data[0][0] <= read_data[31:0] ;
					abstract_Data[1][0] <= read_data[63:32] ;    		
				end
			end
			else if(rg_control[16] == 1 && rg_control[17] == 1) // Write operation
			begin
				Bit#(64) write_data = 0 ;
				if(rg_control[22:20] == 2)begin
					write_data = zeroExtend(abstract_Data[0][0]) ;
				end
				else if(rg_control[22:20] == 3)begin
					write_data[31:0] = abstract_Data[0][0] ; 
					write_data[63:32] = abstract_Data[1][0] ; 
				end

				if(rg_control[5] == 'h0)
				begin
					c64.write_igpr(rg_control[4:0],truncate(write_data) ) ;
					`ifdef verbose $display($time,"\tWriting IGPR %d val: %h",rg_control[4:0],write_data); `endif
				end
				`ifdef spfpu
				else
				begin
					c64.write_fgpr(rg_control[4:0],truncate(write_data)) ;
					`ifdef verbose $display($time,"\tWriting FGPR %d val: %h",rg_control[4:0],write_data); `endif
				end			
				`endif
			end
			perform_reg_access[0]<=False;
			if(rg_control[18] == 1)begin
				start_program_exec[1] <= True ; //clear busy 
				`ifdef verbose $display("DEBUG:  De-assert Abstract Busy\n"); `endif
			end  		
			else
				rg_busy[1]<=0;
    	endrule

		rule access_csrs(rg_halted==1 && perform_reg_access[0] && rg_control[17]==1 && rg_control[12]==0);
    		Bit#(`Reg_width) read_data =0;
			if(rg_control[16]==0)begin// READ Operation
				let x<-c64.rw_csr(rg_control[11:0],False,?);
				read_data=x;
				`ifdef verbose $display($time,"\tDEBUG: READING CSR : address : %h data: %h",rg_control[11:0],read_data); `endif
				if(rg_control[22:20] == 2)begin
					abstract_Data[0][0] <= read_data[31:0] ;    		
				end				
				else if(rg_control[22:20] == 3)begin
					abstract_Data[0][0] <= read_data[31:0] ;
					abstract_Data[1][0] <= read_data[63:32] ;    		
				end
			end
			else begin// WRITE Operation
					Bit#(64) write_data = 0 ;
					if(rg_control[22:20] == 2)begin
						write_data = zeroExtend(abstract_Data[0][0]) ;
					end
					else if(rg_control[22:20] == 3)begin
						write_data[31:0] = abstract_Data[0][0] ; 
						write_data[63:32] = abstract_Data[1][0] ; 
					end
					`ifdef verbose $display($time,"\tDEBUG: WRITING CSR : address : %h data: %h",rg_control[11:0],write_data); `endif
					let x<-c64.rw_csr(rg_control[11:0],True,write_data);
			end
			perform_reg_access[0] <=False; //clear busy
			if(rg_control[18] == 1)begin
				start_program_exec[1] <= True ; //clear busy 
				`ifdef verbose $display("DEBUG:  De-assert Abstract Busy\n"); `endif
			end  		
			else
				rg_busy[1]<=0;
			`ifdef verbose $display($time,"\tDEBUG:  De-assert Abstract Busy\n");   		 `endif
		endrule


    /*==================================== */

    rule access_memory_request (rg_sbsingleread[0]==1 || write_flag[0] == True);
		Bit #(32) address ;
		address[31:0]=busAddr0[0];

		Bit#(8) size =0 ; // size in bytes
		case (rg_sbaccess)
			0: size = 1 ;
			1: size = 2;
			2: size = 4 ;
			3: size = 8 ;
		endcase

		if(rg_sbsingleread[0] == 1)
		begin
			rg_sbsingleread[0]<=0;			
			let read_request = AXI4_Rd_Addr {araddr:truncate(address) , arprot: 0, aruser: 0, arlen: 0, arsize:rg_sbaccess, arburst: 'b01, arid:'d2,arregion:0, arlock: 0, arcache: 0, arqos:0}; // arburst: 00-FIXED 01-INCR 10-WRAP
	  		master_xactor.i_rd_addr.enq(read_request);
  		end

  		else 
  		begin		
			write_flag[0] <= False ;		    
		    Bit#(64) write_data=0;
		    if(size == 8)
	    	begin
	    		write_data[31:0] = busData0[0] ;
	    		write_data[63:32] = busData1[0] ;
	    	end
	    	else 
	    	begin
	    		if (size==4)
	    		begin
	    			write_data[31:0] = busData0[0] ; write_data[63:32]=busData0[0];
	    		end
	    		if (size==2)
	    		begin
	    			write_data[15:0] = busData0[0][15:0];write_data[31:16] = busData0[0][15:0];write_data[47:32] = busData0[0][15:0];write_data[63:48] = busData0[0][15:0];
	    		end
	    		if (size==1)
	    		begin
	    			write_data[7:0] = busData0[0][7:0];write_data[15:8] = busData0[0][7:0];write_data[23:16] = busData0[0][7:0];write_data[31:24] = busData0[0][7:0];
	    			write_data[39:32] = busData0[0][7:0];write_data[47:40] = busData0[0][7:0];write_data[55:48] = busData0[0][7:0];write_data[63:56] = busData0[0][7:0];
	    		end
	    	end
	    	Bit#(8) write_strobe=size==1?8'b1:size==2?8'b11:size==4?8'hf:8'hff;
			if(size!=8)begin			// 8-bit write;
				write_strobe=write_strobe<<(address[`byte_offset:0]);
			end	
			address[2:0]=0;

			`ifdef verbose $display("Debug : Memory Write: Data written : %d Address : %d Write_Strobe : %b",write_data[31:0],address,write_strobe); `endif

			let request_data  = AXI4_Wr_Data {wdata: write_data , wstrb: write_strobe, wlast:True, wid:'d2};
			let request_address = AXI4_Wr_Addr {awaddr: address, awprot:0, awuser:0, awlen: 0, awsize: rg_sbaccess, awburst: 'b01, awid:'d2,awregion:0, awlock: 0, awcache: 0, awqos:0}; // arburst: 00-FIXED 01-INCR 10-WRAP
			master_xactor.i_wr_addr.enq(request_address) ;
			master_xactor.i_wr_data.enq(request_data) ;

  		end

  		if(rg_sbautoincrement == 1)begin
  			Bit #(32) address1 ;
			address1[31:0]=busAddr0[0];
  			address1 = address1 + zeroExtend(size) ;
  			busAddr0[0] <= address1[31:0] ;
  		end
    endrule 

    rule read_memory_response ;
    	let response<-pop_o(master_xactor.o_rd_data);
    	if (response.rresp==AXI4_OKAY && response.rid=='d2) 
    	begin
    		Bit #(64) resp=zeroExtend (response.rdata);
    		busData0[0]<=resp[31:0] ;
    		busData1[0]<=resp[63:32] ;
		end 
    endrule

    rule write_memory_response ;
    	let response <- pop_o(master_xactor.o_wr_resp) ;
    	if(response.bresp == AXI4_OKAY && response.bid=='d2)
    	begin
    		`ifdef verbose $display("Write Done Successfully");	 `endif
    	end
    endrule



    

		/*==== This rule will interact with the external DTM and perform Read/Write to the DM Registers === */
		//method ActionValue#(DTMResponse) toDTM(DTMRequest req)if(rg_busy[0]==0);
		method Action request_from_dtm(Bit#(40) requestfrmDTM);
			Bit#(2) op=requestfrmDTM[1:0];
			Bit#(32) data=requestfrmDTM[33:2];
			Bit#(6) address=requestfrmDTM[39:34];
			`ifdef verbose $display($time,"\tDEBUG: Receiving Request from DTM:\t op: %d data: %h address: %h",op,data,address); `endif
			Bit#(34) response='h0;
			if(op==2)begin // WRITE OPERATION
				case (address)
				`DMCONTROL: 
          			begin 
          				if(data[30] == 1 && data[31] == 1)begin
          					response[1:0] = 2 ;
          				end
          				else begin
          			  		dm_control<=data;
          			  	end
      			  	end
				`DMSTATUS : dm_status<=data;
				`HARTINFO : hart_info<=data;
				`ABSTRACTCNTRL: abstract_Control_And_Status<=data;
				`ABSTRACTCMD  :begin if(rg_busy[2]==1 || start_program_exec[2]) rg_cmderr<=1; else begin abstract_Commands<=data;  rg_busy[2]<=1; perform_reg_access[1]<=True; end end
				`ABSTRACTCMDAUTO:begin abstract_Command_Autoexe<=data; end// ; rg_busy[0] <= 1 ; end
				`BUSCONTROL: bus_ctrlstatus<=data;
				`BUSADDRESS0:busAddr0[1]<=data;
				`BUSDATA0   :begin busData0[1]<=data; write_flag[1] <= True ; end
				`BUSDATA1   :busData1[1]<=data;
				default: begin
						if(address>=`ABSTRACTDATASTART && address<=`ABSTRACTDATAEND)
						begin
							`ifdef verbose $display($time,"DEBUG: Autoexec Write Detected"); `endif
							abstract_Data[address[3:0]-`ABSTRACTDATASTART][1]<=data;
							if(abstract_Command_Autoexe[address[3:0]-`ABSTRACTDATASTART] == 1)
							begin
								`ifdef verbose $display($time,"DEBUG: Autoexecuting command based on write to abstract data "); `endif
								perform_reg_access[1] <=True;
							end
						end
						else if(address>=`PBSTART && address<=`PBEND)
						begin
							`ifdef verbose $display($time,"\tDEBUG: Autoexec Write Detected to PG index: %h",address[3:0]); `endif
							program_Buffer[address[3:0]][1]<=truncate(data);
							if(rg_autoexecprogbuf[address[5:0]-`PBSTART + 16] == 1)
							begin
								`ifdef verbose $display($time,"\tDEBUG: Autoexecuting command based on write to program buffer"); `endif
								perform_reg_access[1] <=True;
							end
						end
						else
							response[1:0]=2;
					end
				endcase
			end
			else if(op==1) begin // READ OPERATION
			case (address)
				`DMCONTROL    :	response[33:2]=dm_control;
				`DMSTATUS     : 	response[33:2]=dm_status;
				`HARTINFO	  :	response[33:2]=hart_info;
				`ABSTRACTCNTRL: 	response[33:2]=abstract_Control_And_Status;
				`ABSTRACTCMD  : 	response[33:2]=abstract_Commands;
				`ABSTRACTCMDAUTO:response[33:2]=abstract_Command_Autoexe;
				`BUSCONTROL     :response[33:2]=bus_ctrlstatus;
				`BUSADDRESS0    :response[33:2]=busAddr0[1];
				`BUSDATA0       :begin
					response[33:2]=busData0[1];
					if(rg_sbautoread == 1)begin
						rg_sbsingleread[1] <= 1; 
					end
				end
          	`BUSDATA1       :response[33:2]=busData1[1];
				default: begin
						if(address>=`ABSTRACTDATASTART && address<=`ABSTRACTDATAEND)
						begin							
							`ifdef verbose $display($time,"DEBUG: Autoexec Read Detected"); `endif
							response[33:2]=zeroExtend(abstract_Data[address[3:0]-`ABSTRACTDATASTART][1]);
							if(abstract_Command_Autoexe[address[3:0]-`ABSTRACTDATASTART] == 1)
							begin
								`ifdef verbose $display($time,"DEBUG: Autoexecuting command based on read in abstract data "); `endif
								perform_reg_access[1] <=True;
							end
						end
						else if(address>=`PBSTART && address<=`PBEND)
						begin
							`ifdef verbose $display($time,"DEBUG1: Autoexec Read Detected"); `endif
							response[33:2]=zeroExtend(program_Buffer[address[3:0]][1]);
							if(abstract_Command_Autoexe[address[5:0]-`PBSTART + 16] == 1)
							begin
								`ifdef verbose $display($time,"DEBUG: Autoexecuting command based on read in program buffer"); `endif
								perform_reg_access[1] <=True;
							end
						end
						else
							response[1:0]=2;
					end
				endcase
			end
			`ifdef verbose $display($time,"\tDebug: Responding with data: %h op: %d",response[33:2],response[1:0]); `endif
			response_to_DTM.enq(response);
		endmethod
		method ActionValue#(Bit#(34)) response_to_dtm;
			response_to_DTM.deq;
			return response_to_DTM.first;
		endmethod
		/*====================================================================================================== */
	
		interface imem_master=c64.imem_master;
		interface dmem_master=c64.dmem_master;

		interface debug_master=master_xactor.axi_side;
		interface debug_slave =slave_xactor.axi_side;
		method Action boot_sequence(Bit#(1) bootseq)=c64.boot_sequence(bootseq);
		method Action set_external_interrupt(Tuple2#(Bool,Bool) i)=c64.set_external_interrupt(i);
		`ifdef CLINT
			method Action clint_msip(Bit#(1) intrpt)=c64.clint_msip(intrpt);
			method Action clint_mtip(Bit#(1) intrpt)=c64.clint_mtip(intrpt);
			method Action clint_mtime(Bit#(`Reg_width) c_mtime)=c64.clint_mtime(c_mtime);
		`endif
	endmodule
endpackage
