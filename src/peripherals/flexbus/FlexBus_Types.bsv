// Copyright (c) 2017 Bluespec, Inc.  All Rights Reserved

package FlexBus_Types;

// ================================================================
// See export list below
// ================================================================
// Exports

export

// RTL-level interfaces (signals/buses)
FlexBus_Slave_IFC (..),
FlexBus_Master_IFC (..),


// Higher-level enums and structs for the FlexBus
FlexBus_States (..),

FlexBus_Payload (..),
FlexBus_Attr (..),
FlexBus_din (..),
FlexBus_Signals (..),

// Higher-level FIFO-like interfaces for the 5 AXI4 channels,
FlexBus_Register_IFC (..),
FlexBus_Register_Output_IFC (..),
FlexBus_Register_Input_IFC (..),

AXI4_Slave_to_FlexBus_Master_Xactor_IFC (..),

// Transactors from RTL-level interfacecs to FIFO-like interfaces.
mkAXI4_Slave_to_FlexBus_Master_Xactor;

// ================================================================
// BSV library imports

import Vector      :: *;
import FIFOF       :: *;
import SpecialFIFOs:: *;
import Connectable :: *;
import ConfigReg :: *;
`include "defined_parameters.bsv"

// ----------------
// BSV additional libs

import Semi_FIFOF :: *;
import AXI4_Types   :: *;

import Memory_AXI4 :: *;

// ****************************************************************
// ****************************************************************
// Section: RTL-level interfaces
// ****************************************************************
// ****************************************************************

// ================================================================
// These are the signal-level interfaces for an FlexBus master.
// The (*..*) attributes ensure that when bsc compiles this to Verilog,
// we get exactly the signals specified in the FlexBus spec.

interface FlexBus_Master_IFC;
   // FlexBus External Signals
  
   // AD inout bus separate for now in BSV
   (* always_ready, result="AD"       *)  method Bit #(32)   m_AD;                                // out

   //(* always_ready, always_enabled    *)  method Action m_din ((* port="din" *) Bit #(32) din);   // in
   method Action m_din ((* port="din" *) Bit #(32) din);   // in

   (* always_ready, result="R_Wn"     *)  method Bit #(1)       m_R_Wn;                                   // out
   (* always_ready, result="TSIZ"     *)  method Bit #(2)       m_TSIZ;                                   // out

   (* always_ready, result="FBCSn"    *)  method Bit #(6)       m_FBCSn;                                  // out
   (* always_ready, result="BEn_BWEn" *)  method Bit #(4)       m_BE_BWEn;                                // out
   (* always_ready, result="TBSTn"    *)  method Bit #(1)       m_TBSTn;                                  // out
   (* always_ready, result="OEn"      *)  method Bit #(1)       m_OEn;                                    // out

   (* always_ready, result="ALE"      *)  method Bit #(1)       m_ALE;                                    // out
   //(* always_ready, always_enabled    *)  method Action m_TAn ((* port="TAn" *) Bit #(1) tAn);            // in
   method Action m_TAn ((* port="TAn" *) Bit #(1) tAn);            // in

endinterface: FlexBus_Master_IFC

interface FlexBus_Register_Input_IFC;
	method Action reset (Bit#(32) ad_bus);
        method Action m_ad_bus (Bit#(32) ad_bus);
        method Action m_data_bus (Bit#(32) data_bus);
endinterface: FlexBus_Register_Input_IFC

interface FlexBus_Register_Output_IFC;
	(* always_ready, always_enabled *)   method Bit#(6) m_FBCSn(); 
	(* always_ready, always_enabled *)   method Bit#(6) m_SWS(); 
        (* always_ready, always_enabled *)   method Bit#(1) m_SWS_EN(); 
	(* always_ready, always_enabled *)   method Bit#(2) m_ASET(); 
	(* always_ready, always_enabled *)   method Bit#(2) m_RDAH(); 
	(* always_ready, always_enabled *)   method Bit#(2) m_WRAH(); 
	(* always_ready, always_enabled *)   method Bit#(6) m_WS(); 
	(* always_ready, always_enabled *)   method Bit#(1) m_AA(); 
	(* always_ready, always_enabled *)   method Bit#(2) m_PS(); 
	(* always_ready, always_enabled *)   method Bit#(1) m_BEM(); 
	(* always_ready, always_enabled *)   method Bit#(1) m_BSTR(); 
	(* always_ready, always_enabled *)   method Bit#(1) m_BSTW();
endinterface: FlexBus_Register_Output_IFC

interface FlexBus_Register_IFC;
	interface FlexBus_Register_Input_IFC inp_side;
	interface FlexBus_Register_Output_IFC op_side;
endinterface: FlexBus_Register_IFC

// ================================================================
// These are the signal-level interfaces for an AXI4-Lite slave.
// The (*..*) attributes ensure that when bsc compiles this to Verilog,
// we get exactly the signals specified in the ARM spec.

interface FlexBus_Slave_IFC ;
  
   (* always_ready, always_enabled    *) method Action m_AD 		( (* port="AD"       *)  Bit #(32)   i_AD);                         // in


   (* always_ready, always_enabled    *) method Action m_ALE     	( (* port="ALE"      *)  Bit #(1)         i_ALE);                           // in
										
   (* always_ready, always_enabled    *) method Action m_R_Wn		( (* port="R_Wn"     *)  Bit #(1)         i_R_Wn);                          // in
   (* always_ready, always_enabled    *) method Action m_TSIZ 		( (* port="TSIZ"     *)  Bit #(2)         i_TSIZ);                          // in
										
   (* always_ready, always_enabled    *) method Action m_FBCSn 		( (* port="FBCSn"    *)  Bit #(6)         i_FBCSn);                         // in
   (* always_ready, always_enabled    *) method Action m_BE_BWEn	( (* port="BE_BWEn"  *)  Bit #(4)         i_BE_BWEn);                       // in
   (* always_ready, always_enabled    *) method Action m_TBSTn 		( (* port="TBSTn"    *)  Bit #(1)         i_TBSTn);                         // in
   (* always_ready, always_enabled    *) method Action m_OEn		( (* port="OEn"      *)  Bit #(1)         i_OEn);                           // in
										
   (* always_ready, result="din"   *)  method Bit #(32) m_din;                                                // out
   (* always_ready, result="TAn"   *)  method Bit #(1) m_TAn;                                                      // out

endinterface: FlexBus_Slave_IFC


// ================================================================
// Connecting signal-level interfaces

instance Connectable #(FlexBus_Master_IFC ,
		       FlexBus_Slave_IFC  );

   module mkConnection #(FlexBus_Master_IFC  flexbus_m,
			 FlexBus_Slave_IFC  flexbus_s)
		       (Empty);

    (* fire_when_enabled, no_implicit_conditions *)
    rule rl_flexbus_AD_signals;
        flexbus_s.m_AD      (flexbus_m.m_AD);
    endrule


      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_flexbus_Attr_signals;
	 flexbus_s.m_ALE     (flexbus_m.m_ALE);
	 flexbus_s.m_R_Wn    (flexbus_m.m_R_Wn);
	 flexbus_s.m_TSIZ    (flexbus_m.m_TSIZ);
      endrule
      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_flexbus_signals;
	 flexbus_s.m_FBCSn   (flexbus_m.m_FBCSn);
	 flexbus_s.m_BE_BWEn (flexbus_m.m_BE_BWEn);
	 flexbus_s.m_TBSTn   (flexbus_m.m_TBSTn);
	 flexbus_s.m_OEn     (flexbus_m.m_OEn);
      endrule
      (* fire_when_enabled *)
      //(* fire_when_enabled, no_implicit_conditions *)
      rule rl_flexbus_input_signals;
	 flexbus_m.m_din (flexbus_s.m_din);
	 flexbus_m.m_TAn (flexbus_s.m_TAn);
      endrule

   endmodule
endinstance

// ****************************************************************
// ****************************************************************
// Section: Higher-level FIFO-like interfaces and transactors
// ****************************************************************
// ****************************************************************

// ================================================================
// Higher-level types for payloads (rather than just bits)

typedef enum { IDLE, FlexBus_S0_DEQ_WR_FIFOS, FlexBus_S0_DEQ_RD_FIFOS, FlexBus_S1_ADDR, FlexBus_S2_WRITE, FlexBus_S3_BURST, FlexBus_S4_HOLD } FlexBus_States deriving (Bits, Eq, FShow);
typedef enum { IDLE, FlexBus_S0_CHK_FIFOS, FlexBus_S0_DEQ_FIFOS, FlexBus_WRITE_DUMMY1, FlexBus_WRITE_DUMMY2 } FlexBus_States_wr deriving (Bits, Eq, FShow);
typedef enum { IDLE, FlexBus_S0_CHK_FIFOS, FlexBus_S0_DEQ_FIFOS} FlexBus_States_rd deriving (Bits, Eq, FShow);

//FlexBus Addr. Data Payload

typedef struct {
   Bit #(32)     s_AD;                                     // out
   } FlexBus_Payload
deriving (Bits, FShow);

typedef struct {
   Bit #(32)    din;                                    // in
   } FlexBus_din 
deriving (Bits, FShow);

//FlexBus Attributes

typedef struct {
   Bit #(1)         s_R_Wn;                                   // out
   Bit #(2)         s_TSIZ;                                   // out
   } FlexBus_Attr 
deriving (Bits, FShow);

typedef struct {
   Bit #(6)         s_FBCSn;                                  // out
   Bit #(4)         s_BEn_BWEn;                               // out
   Bit #(1)         s_TBSTn;                                  // out
   Bit #(1)         s_OEn;                                    // out
   } FlexBus_Signals #(numeric type wd_addr, numeric type wd_data)
deriving (Bits, FShow);

// FlexBus Control Signals

// Bit              s_ALE;                                    // out
// Bit              s_TAn;                                    // in

/* ----------------------------------------------------------------

   module mkFlexBusTop (Empty);
	AXI4_Slave_to_FlexBus_Master_Xactor_IFC#(56, 64,10)
                                                  flexbus_xactor_ifc <- mkAXI4_Slave_to_FlexBus_Master_Xactor;

   endmodule


// ---------------------------------------------------------------- */
// AXI4 Lite Slave to FlexBus Master transactor interface

interface AXI4_Slave_to_FlexBus_Master_Xactor_IFC #(numeric type wd_addr,
				       numeric type wd_data,
				       numeric type wd_user);
   method Action reset;

   // AXI side
   interface AXI4_Slave_IFC #(wd_addr, wd_data, wd_user) axi_side;

   // FlexBus side
   interface FlexBus_Master_IFC flexbus_side;

endinterface: AXI4_Slave_to_FlexBus_Master_Xactor_IFC

// ----------------------------------------------------------------

// AXI4 Lite Slave to FlexBus Master transactor 

module mkAXI4_Slave_to_FlexBus_Master_Xactor 
		(AXI4_Slave_to_FlexBus_Master_Xactor_IFC #(wd_addr, wd_data, wd_user))
   provisos (Add#(a__, 8, wd_addr),
        Add#(b__, 64, wd_data),
	     //Bits#(Bit#(56), wd_addr),
	     //Bits#(Bit#(64), wd_data),
	     //Bits#(Bit#(32), wd_fb_addr),
	     //Bits#(Bit#(32), wd_fb_data),
	     //Bits#(Inout#(Bit#(32)), a__),
	    // Bits#(Inout#(Bit#(32)), wd_Fb_addr),
	     //Bits#(Inout#(Bit#(32)), 48),
             Div#(wd_data, 16, 4));
   Bool unguarded = True;
   Bool guarded   = False;
   //let wD_FB_ADDR = valueOf(wd_fb_addr);
   //let wD_FB_DATA = valueOf(wd_fb_data);

   FlexBus_Register_IFC register_ifc <- mkFlexBus_Registers;

   Reg#(Bit#(32))       r_AD		<- mkReg(0); 
   Reg#(Bit#(32))       r_din		<- mkReg(0); 
   Reg#(Bit#(1))      	r_R_Wn		<- mkReg(1'b1); 
   Reg#(Bit#(2))      	r_TSIZ		<- mkReg(2'b00);  
   Reg#(Bit#(6))      	r_FBCSn		<- mkReg(6'h3F);  
   Reg#(Bit#(4))      	r_BE_BWEn	<- mkReg(4'hF);
   Reg#(Bit#(1))      	r_TBSTn		<- mkReg(1'b1);   
   Reg#(Bit#(1))      	r_OEn		<- mkReg(1'b1);      
   Reg#(Bit#(1))      	r_ALE		<- mkReg(1'b0);       
   Reg#(Bit#(1))	    r_ext_TAn	<- mkReg(1'b0);         
   Reg#(Bit#(1))	    r_int_TAn	<- mkReg(1'b1);         

   Reg#(Bit#(2))      	r_ASET		<- mkReg(2'b00);  
   Reg#(Bit#(2))      	r_PS  		<- mkReg(2'b00);  
   Reg#(Bit#(3))      	r_rpt_cnt	<- mkReg(3'b000);  
   Reg#(Bit#(2))      	r_burst_cnt	<- mkReg(2'b00);  
   Reg#(Bit#(2))      	r_hld_cnt	<- mkReg(2'b00);  
   Reg#(Bit#(6))      	r_WS_cnt	<- mkReg(6'h00);  
   Reg#(Bit#(6))      	r_SWS_cnt	<- mkReg(6'h00);  
   Reg#(Bit#(wd_addr))  r_awaddr    <- mkReg(0);
   Reg#(Bit#(2))      	r_awsize    <- mkReg(0);
   Reg#(Bit#(wd_addr))  r2_awaddr   <- mkReg(0);
   Reg#(Bit#(wd_data))  r_wdata     <- mkReg(0);
   Reg#(AXI4_Resp)      r_wrbresp	<- mkReg(AXI4_OKAY);  
   Reg#(AXI4_Resp)      r_rresp	    <- mkReg(AXI4_OKAY);  
   Reg#(Bit#(wd_data))  r_rd_data        <- mkReg(0);
   Reg#(Bit#(TDiv#(wd_data,8))) r1_wstrb <- mkReg(0);
   Reg#(Bit#(TDiv#(wd_data,8))) r2_wstrb <- mkReg(0);
   Reg#(Bit#(wd_addr))  r_araddr         <- mkReg(0);
   Reg#(Bit#(wd_addr))  r2_araddr        <- mkReg(0);
   Reg#(Bit#(2))      	r_arsize         <- mkReg(0);
   Reg#(Bit#(4))      	r_arid           <- mkReg(0);
   Reg#(Bit#(4))      	r_awid           <- mkReg(0);
   Reg#(Bit#(1))      	wr_pending       <- mkReg(0);
   Reg#(Bit#(1))      	r_chk_fifos_wr   <- mkReg(0);
   Reg#(Bit#(1))      	r_chk_fifos_rd   <- mkReg(0);
   ConfigReg#(Bit#(1))      	rd_wrb      <- mkConfigReg(1);
   Reg#(Bool)         	r_rready  	<- mkReg(False);       
   Reg#(Bool)         	r2_rready  	<- mkReg(False);       

   Reg#(Bool)         	r1_awvalid	<- mkReg(False);       
   Reg#(Bool)         	r2_awvalid	<- mkReg(False);       
   Reg#(Bool)         	r1_wvalid	<- mkReg(False);       
   Reg#(Bool)         	r2_wvalid	<- mkReg(False);       
   Reg#(Bool)      	    r1_arvalid 	<- mkReg(False);       
   Reg#(Bool)         	r2_arvalid 	<- mkReg(False);       

   Reg#(Bool)         	r1_OEn	 	<- mkReg(True);       

   Reg#(Bit#(8))  r_AD_32bit_data_byte1  <- mkReg(0);
   Reg#(Bit#(8))  r_AD_32bit_data_byte2  <- mkReg(0);
   Reg#(Bit#(8))  r_AD_32bit_data_byte3  <- mkReg(0);
   Reg#(Bit#(8))  r_AD_32bit_data_byte4  <- mkReg(0);

   Reg#(Bit#(8))  r_AD_32bit_addr_byte1  <- mkReg(0);
   Reg#(Bit#(8))  r_AD_32bit_addr_byte2  <- mkReg(0);
   Reg#(Bit#(8))  r_AD_32bit_addr_byte3  <- mkReg(0);
   Reg#(Bit#(8))  r_AD_32bit_addr_byte4  <- mkReg(0);

   Reg#(Bit#(8))  r_rd_data_32bit_byte1  <- mkReg(0);
   Reg#(Bit#(8))  r_rd_data_32bit_byte2  <- mkReg(0);
   Reg#(Bit#(8))  r_rd_data_32bit_byte3  <- mkReg(0);
   Reg#(Bit#(8))  r_rd_data_32bit_byte4  <- mkReg(0);

   Reg#(Bit#(32))  r_MBAR  <- mkReg(32'h04000000);

   Reg#(FlexBus_States) flexbus_state <- mkReg(IDLE);
   Reg#(FlexBus_States_rd) flexbus_state_rd <- mkReg(FlexBus_S0_CHK_FIFOS);
   Reg#(FlexBus_States_wr) flexbus_state_wr <- mkReg(FlexBus_S0_CHK_FIFOS);

   // These FIFOs are guarded on BSV side, unguarded on AXI side
   FIFOF #(AXI4_Wr_Addr #(wd_addr, wd_user)) f_wr_addr <- mkGFIFOF (unguarded, guarded);
   FIFOF #(AXI4_Wr_Data #(wd_data))          f_wr_data <- mkGFIFOF (unguarded, unguarded);
   FIFOF #(AXI4_Wr_Resp #(wd_user))          f_wr_resp <- mkGFIFOF (guarded, unguarded);

   FIFOF #(AXI4_Rd_Addr #(wd_addr, wd_user)) f_rd_addr <- mkGFIFOF (unguarded, guarded);
   FIFOF #(AXI4_Rd_Data #(wd_data, wd_user)) f_rd_data <- mkGFIFOF (guarded, unguarded);

   Reg#(Maybe#(Bit#(1)))  c_TAn[2] <- mkCReg(2, tagged Invalid);
   Reg#(Maybe#(Bit#(32))) c_din[2] <- mkCReg(2, tagged Invalid);

   //TriState#(Bit#(32)) tri_AD_out <- mkTriState(r1_OEn,r_AD);

   // ----------------------------------------------------------------

   rule rl_OEn;
	if (r_OEn == 1'b0)
           	r1_OEn <= False;
        else
           	r1_OEn <= True;
   endrule

   rule rl_state_S0_CHK_FIFO_RD(flexbus_state_rd == FlexBus_S0_CHK_FIFOS);
	`ifdef verbose_debug $display("STATE S0 CHK FIFOS RD FIRED"); `endif
        if (f_rd_addr.notEmpty) begin
		    register_ifc.inp_side.m_ad_bus(f_rd_addr.first.araddr[31:0]);
            flexbus_state_rd <= FlexBus_S0_DEQ_FIFOS;
            `ifdef verbose_debug_l2 $display("READ ADDR FIFO WAS READ FIRST  r_araddr=%h \n", f_rd_addr.first.araddr); `endif
        end
   endrule

  (* preempts = "rl_check_read_fifo, rl_check_write_fifo" *) 
   rule rl_check_read_fifo (r_chk_fifos_rd == 1'b1 && f_rd_addr.notEmpty); 
                rd_wrb <= 1'b1;
                r_chk_fifos_rd <= 1'b0;
                r_chk_fifos_wr <= 1'b0;
   endrule

   rule rl_check_write_fifo(r_chk_fifos_wr == 1'b1 && f_wr_addr.notEmpty && f_wr_data.notEmpty);
		    if (f_wr_addr.first.awaddr[31:16] != r_MBAR[31:16]) begin
                    rd_wrb <= 1'b0;
                    r_chk_fifos_rd <= 1'b0;
                    r_chk_fifos_wr <= 1'b0;
            end
   endrule

   rule rl_state_S0_CHK_FIFOS_WR(flexbus_state_wr == FlexBus_S0_CHK_FIFOS);
	`ifdef verbose_debug $display("STATE S0 CHK FIFOS WR FIRED"); `endif
        if (f_wr_addr.notEmpty && f_wr_data.notEmpty) begin
		    if (f_wr_addr.first.awaddr[31:16] == r_MBAR[31:16]) begin
			    f_wr_addr.deq; f_wr_data.deq;
		    end
		    else begin
                flexbus_state_wr <= FlexBus_S0_DEQ_FIFOS;
		    end
		    register_ifc.inp_side.m_ad_bus(f_wr_addr.first.awaddr[31:0]);
		    register_ifc.inp_side.m_data_bus(f_wr_data.first.wdata[31:0]);
        end
   endrule

   rule rl_state_S0_DEQ_FIFOS (flexbus_state_rd == FlexBus_S0_DEQ_FIFOS || flexbus_state_wr == FlexBus_S0_DEQ_FIFOS);
	`ifdef verbose_debug $display("STATE S0 DEQ FIFOS FIRED"); `endif
        if (rd_wrb == 1'b1) begin
                flexbus_state <= FlexBus_S0_DEQ_RD_FIFOS;
                flexbus_state_rd <= IDLE;
                flexbus_state_wr <= IDLE;
        end
        else if (rd_wrb == 1'b0) begin
                flexbus_state <= FlexBus_S0_DEQ_WR_FIFOS;
                flexbus_state_rd <= IDLE;
                flexbus_state_wr <= IDLE;
        end
        if (flexbus_state_rd == FlexBus_S0_DEQ_FIFOS && flexbus_state_wr == FlexBus_S0_DEQ_FIFOS) wr_pending <= 1'b1;
   endrule

   rule rl_state_S0_DEQ_WR_FIFOS (flexbus_state == FlexBus_S0_DEQ_WR_FIFOS);
	`ifdef verbose_debug $display("STATE S0 DEQ WR FIFOS FIRED"); `endif
	r_ASET <= register_ifc.op_side.m_ASET;
	Bit#(3) v_awsize = 3'b000;
   	if ((f_wr_addr.notEmpty) )  begin
   		r1_awvalid <= f_wr_addr.notEmpty;
		f_wr_addr.deq;
        r_chk_fifos_wr <= 1'b1;
        r_chk_fifos_rd <= 1'b1;
		AXI4_Wr_Addr#(wd_addr, wd_user) wr_addr = f_wr_addr.first;
        	r_awaddr <= f_wr_addr.first.awaddr;
        	v_awsize = f_wr_addr.first.awsize;
            r_awid <= f_wr_addr.first.awid;
                case (v_awsize) matches
			{3'b000}: r_awsize <= 2'b01;
			{3'b001}: r_awsize <= 2'b10;
			{3'b010}: r_awsize <= 2'b00;
		endcase
                `ifdef verbose_debug_l2 $display("ADDR FIFO WAS NOT EMPTY SO I DEQUEUED r_awaddr=%h \n", r_awaddr); `endif
        end
   	if ((f_wr_data.notEmpty) ) begin
   		r1_wvalid <= f_wr_data.notEmpty;
		f_wr_data.deq;
                `ifdef verbose_debug_l2 $display("DATA FIFO WAS NOT EMPTY SO I DEQUEUED\n"); `endif
   		AXI4_Wr_Data#(wd_data) wr_data = f_wr_data.first;
        	r_wdata <= f_wr_data.first.wdata;
        	r1_wstrb <= f_wr_data.first.wstrb;
		`ifdef verbose_debug_l2 $display(" dequeued first r_wdata = %h", r_wdata); `endif
        end
        if (f_wr_addr.notEmpty && f_wr_data.notEmpty) begin
                flexbus_state <= FlexBus_S1_ADDR;
        end
   endrule

   rule rl_S0_DEQ_RD_FIFOS (flexbus_state == FlexBus_S0_DEQ_RD_FIFOS);
	`ifdef verbose_debug $display("STATE S0 DEQ RD FIFOS FIRED"); `endif
	r_ASET <= register_ifc.op_side.m_ASET;
	Bit#(3) v_arsize = 3'b000;
   	if ((f_rd_addr.notEmpty) ) begin
   		r1_arvalid <= f_rd_addr.notEmpty;
		f_rd_addr.deq;
        r_chk_fifos_wr <= 1'b1;
        r_chk_fifos_rd <= 1'b1;
   		AXI4_Rd_Addr#(wd_addr, wd_user) rd_addr = f_rd_addr.first;
        	r_araddr <= f_rd_addr.first.araddr;
        	v_arsize = f_rd_addr.first.arsize;
            r_arid <= f_rd_addr.first.arid;
                case (v_arsize) matches
			{3'b000}: r_arsize <= 2'b01;
			{3'b001}: r_arsize <= 2'b10;
			{3'b010}: r_arsize <= 2'b00;
		endcase
		r_rd_data_32bit_byte1 <= 0;
		r_rd_data_32bit_byte2 <= 0;
		r_rd_data_32bit_byte3 <= 0;
		r_rd_data_32bit_byte4 <= 0;
                `ifdef verbose_debug_l2 $display("ADDR FIFO WAS NOT EMPTY SO I DEQUEUED r_araddr=%h \n", f_rd_addr.first.araddr); `endif
        end
        if (f_rd_addr.notEmpty) begin
                flexbus_state <= FlexBus_S1_ADDR;
        end
   endrule

   rule rl_enq_wr_resp;
   Bool bready = f_wr_resp.notFull; 
   if (f_wr_resp.notFull) 
  	f_wr_resp.enq (AXI4_Wr_Resp {bresp:r_wrbresp,
					  buser:0,
					  bid:r_awid});
   endrule


   rule rl_enq_rd_data;
   	Bool rready = f_rd_data.notFull; 
   	if (f_rd_data.notFull && r2_rready) begin 
   		f_rd_data.enq (AXI4_Rd_Data {rdata: r_rd_data,
						  rresp: r_rresp,
						  rlast: True,
						  ruser:0,
                          rid:r_arid});
		//AXI4_Slave_IFC.m_rready(True);
        	`ifdef verbose_debug $display("RD DATA FIFO WAS NOT FULL SO I ENQUEUED r_rd_data=%h r2_rready= %b\n", r_rd_data, r2_rready); `endif
    	end
   endrule

   rule rl_state_S1_ADDR (flexbus_state == FlexBus_S1_ADDR); //Address state
	`ifdef verbose_debug $display("STATE S1 FIRED");`endif
        r_PS   <= register_ifc.op_side.m_PS;  
	r_WS_cnt <= register_ifc.op_side.m_WS;
	r_OEn <= 1'b1;
	r_BE_BWEn <= 4'hF;
	r_FBCSn <= 6'h3F;
        r_ALE <= 1'b1;
        `ifdef verbose_debug_l2 $display(" r_ASET was ASSIGNED = %b", r_ASET); `endif
		if (r_rpt_cnt == 3'b000) begin
        		if (r1_arvalid) begin
				r_AD <= r_araddr[31:0];
            			r_R_Wn <= 1'b1;   // Read
            			r_TSIZ <= r_arsize; 
                	end
        		else if (r1_awvalid && r1_wvalid) begin
            			r_AD <= r_awaddr[31:0];
            			r_R_Wn <= 1'b0;   // WriteBar
            			r_TSIZ <= r_awsize; 
                	end
        	end
        	else begin
			if (r_R_Wn == 1'b0) r_AD <= r_awaddr[31:0];
                	else r_AD <= r_araddr[31:0];
			r_TBSTn <= 1'b1;
			r_TSIZ <= register_ifc.op_side.m_PS;
		end
        if (( r_ASET != 2'b00) ) begin 
    		r_ASET <= r_ASET - 1; 
	end
	else begin 
		flexbus_state <= FlexBus_S2_WRITE;
		if (r_rpt_cnt != 3'b000) 
                	r_rpt_cnt <= r_rpt_cnt -1;
	end
   endrule

   rule rl_assign_AD_bus_reg (flexbus_state == FlexBus_S1_ADDR) ; // Address an Attributes Phase
	`ifdef verbose_debug_l2 $display(" ASSIGN AD BUS FIRED"); `endif

        r2_awvalid <= r1_awvalid;
        r2_wvalid  <= r1_wvalid;
        r2_wstrb   <= r1_wstrb;
        r2_arvalid <= r1_arvalid;

        r2_araddr <= r_araddr;
        r2_awaddr <= r_awaddr;

	r_AD_32bit_data_byte1   <= pack(r_wdata[7:0]); 
	r_AD_32bit_data_byte2   <= pack(r_wdata[15:8]); 
	r_AD_32bit_data_byte3 <= pack(r_wdata[23:16]); 
	r_AD_32bit_data_byte4 <= pack(r_wdata[31:24]);
	r_AD_32bit_addr_byte1  <= pack(r_awaddr[31:24]); 
	r_AD_32bit_addr_byte2  <= pack(r_awaddr[23:16]); 
	r_AD_32bit_addr_byte3  <= pack(r_awaddr[15:8]); 
	r_AD_32bit_addr_byte4  <= pack(r_awaddr[7:0]); 
        `ifdef verbose_debug_l2 $display("r_wdata after ASSIGN = %h r_PS = %b r_AD_32bit_data_byte1=%h ", r_wdata, r_PS, r_AD_32bit_data_byte1);
        $display("r_awaddr after ASSIGN = %h r_PS = %b r_AD_32bit_addr_byte1=%h ", r_awaddr, r_PS, r_AD_32bit_addr_byte1); `endif
   endrule

   rule rl_assign_rd_data;
	r_rd_data[63:0] <= pack({32'h00000000, r_rd_data_32bit_byte4, r_rd_data_32bit_byte3, r_rd_data_32bit_byte2, r_rd_data_32bit_byte1});
        r2_rready <= r_rready;
        `ifdef verbose_debug_l2 $display("ASSIGN READ DATA FIRED AND r_rd_data = %h r_rready=%b r2_rready=%b", r_rd_data, r_rready, r2_rready);`endif
   endrule
  
   rule rl_read_ext_signals;
	if (isValid(c_TAn[1])) begin
    		r_ext_TAn <= fromMaybe(?,c_TAn[1]);
      		c_TAn[1]<= tagged Invalid;
	end
	if (isValid(c_din[1])) begin
    		r_din <= fromMaybe(?,c_din[1]);
      		c_din[1]<= tagged Invalid;
	end
    //r_din <= tri_AD_out._read;
   endrule

   rule rl_state_S2_WRITE (flexbus_state == FlexBus_S2_WRITE); //Write Phase
	`ifdef verbose_debug $display("STATE S2 FIRED"); `endif
     	r_ALE <= 1'b0;
        r_FBCSn <= register_ifc.op_side.m_FBCSn;
	r_SWS_cnt <= register_ifc.op_side.m_SWS;
        if (r_R_Wn == 1'b1)
		r_hld_cnt <= register_ifc.op_side.m_RDAH;
   	else 
		r_hld_cnt <= register_ifc.op_side.m_WRAH;
           	   if (r_R_Wn == 1'b1) begin
			r_OEn <= 1'b0;
  			if ((register_ifc.op_side.m_BSTR == 1'b1) && ((r_PS == 2'b01) || (r_PS == 2'b10) || (r_PS == 2'b11))) begin
   				r_TBSTn <= 1'b0; 
     		   	end
     		   end
                   else begin
			// ASSIGN WRITE DATA DEPENDING ON BURST INHIBITED OR NOT
			if ((r_rpt_cnt == 3'b000) ) begin 
                        	if (r_PS == 2'b01) begin
					r_AD <= pack({r_AD_32bit_data_byte1,r_AD_32bit_addr_byte2,r_AD_32bit_addr_byte3,r_AD_32bit_addr_byte4});
				end
                        	else if ((r_PS == 2'b10) || (r_PS == 2'b11)) begin
					r_AD <= pack({r_AD_32bit_data_byte1,r_AD_32bit_data_byte2,r_AD_32bit_addr_byte3,r_AD_32bit_addr_byte4});
				end
 				else begin
					r_AD <= pack({r_AD_32bit_data_byte1,r_AD_32bit_data_byte2,r_AD_32bit_data_byte3,r_AD_32bit_data_byte4});
				end
			end
                        else if (r_rpt_cnt == 3'b011) begin 
					r_AD <= pack({r_AD_32bit_data_byte2,r_AD_32bit_addr_byte2,r_AD_32bit_addr_byte3,r_AD_32bit_addr_byte4});
			end
			else if (r_rpt_cnt == 3'b010)
					r_AD <= pack({r_AD_32bit_data_byte3,r_AD_32bit_addr_byte2,r_AD_32bit_addr_byte3,r_AD_32bit_addr_byte4});
                        else if  (r_rpt_cnt == 3'b001) begin
				if (r_awsize == 2'b00) begin
                        		if ((r_PS == 2'b10) || (r_PS == 2'b11))
						r_AD <= pack({r_AD_32bit_data_byte3,r_AD_32bit_data_byte4,r_AD_32bit_addr_byte3,r_AD_32bit_addr_byte4});
					else if ((r_PS == 2'b01))
						r_AD <= pack({r_AD_32bit_data_byte4,r_AD_32bit_addr_byte2,r_AD_32bit_addr_byte3,r_AD_32bit_addr_byte4});
					end
				else if (r_awsize == 2'b10) begin
					if (r_PS == 2'b01) r_AD <= pack({r_AD_32bit_data_byte2,r_AD_32bit_addr_byte2,r_AD_32bit_addr_byte3,r_AD_32bit_addr_byte4});
					
				end
			end
			if (register_ifc.op_side.m_BEM == 1'b1) 
        			r_BE_BWEn <= r2_wstrb[3:0];
  			if ((register_ifc.op_side.m_BSTW == 1'b1) && ((r_PS == 2'b01) || (r_PS == 2'b10) || (r_PS == 2'b11))) begin
   				r_TBSTn <= 1'b0; 
		   	end
		   end
                if (r_WS_cnt == 6'h00) begin
		    if (r_ext_TAn == 1'b0) begin
			//r_int_TAn <= 1'b0;
			flexbus_state <= FlexBus_S3_BURST;
		    end
	            if (register_ifc.op_side.m_AA == 1'b1) begin
			r_int_TAn <= 1'b1;
                    end
		    r_WS_cnt <= register_ifc.op_side.m_WS;
           	    if (r_R_Wn == 1'b1) begin
			if (r_arsize == 2'b00) begin
  				if ((register_ifc.op_side.m_BSTR == 1'b1) && ((r_PS == 2'b01) || (r_PS == 2'b10) || (r_PS == 2'b11))) begin
					if (r_PS == 2'b01) r_burst_cnt <= 2'b11;
					if ((r_PS == 2'b10)||(r_PS == 2'b11)) r_burst_cnt <= 2'b01;
				end
  				else if ((register_ifc.op_side.m_BSTR == 1'b0) && ((r_PS == 2'b01) || (r_PS == 2'b10) || (r_PS == 2'b11))) begin
					if ((r_PS == 2'b01) && (r_rpt_cnt == 3'b000)) r_rpt_cnt <= 3'b100;
					if (((r_PS == 2'b10)||(r_PS == 2'b11)) && (r_rpt_cnt == 3'b000)) r_rpt_cnt <= 3'b010;
				end
			end
			else if (r_arsize == 2'b10) begin
				if ((register_ifc.op_side.m_BSTR == 1'b1) && (r_PS == 2'b01)) begin
					r_burst_cnt <= 2'b01;
				end 
				else if ((register_ifc.op_side.m_BSTR == 1'b0) && (r_PS == 2'b01)) begin
					if ((r_rpt_cnt == 3'b000)) r_rpt_cnt <= 3'b010;
				end
			end 
     		   end
                   else begin
			if (r_awsize == 2'b00) begin
  				if ((register_ifc.op_side.m_BSTW == 1'b1) && ((r_PS == 2'b01) || (r_PS == 2'b10) || (r_PS == 2'b11))) begin
					if (r_PS == 2'b01) r_burst_cnt <= 2'b11;
					if ((r_PS == 2'b10)||(r_PS == 2'b11)) r_burst_cnt <= 2'b01;
				end
  				else if ((register_ifc.op_side.m_BSTW == 1'b0) && ((r_PS == 2'b01) || (r_PS == 2'b10) || (r_PS == 2'b11))) begin
					if ((r_PS == 2'b01) && (r_rpt_cnt == 3'b000)) r_rpt_cnt <= 3'b100;
					if (((r_PS == 2'b10)||(r_PS == 2'b11)) && (r_rpt_cnt == 3'b000)) r_rpt_cnt <= 3'b010;
				end
			end
			else if (r_awsize == 2'b10) begin
				if ((register_ifc.op_side.m_BSTW == 1'b1) && (r_PS == 2'b01)) begin
					r_burst_cnt <= 2'b01;
				end 
				else if ((register_ifc.op_side.m_BSTW == 1'b0) && (r_PS == 2'b01)) begin
					if ((r_rpt_cnt == 3'b000)) r_rpt_cnt <= 3'b010;
				end
			end 
		   end
                end
		else begin
                   r_WS_cnt <= r_WS_cnt -1;
	     end
             `ifdef verbose_debug_l2 $display("r_AD after WRITE = %h r_ASET=%b r_R_Wn= %b r_PS = %b r_AD_32bit_data_byte1=%h ", r_AD, r_ASET, r_R_Wn, r_PS, r_AD_32bit_data_byte1); `endif
   endrule

   rule rl_state_S3_BURST (flexbus_state == FlexBus_S3_BURST); // Data Phase with/without bursting terminated prematurely externally
	`ifdef verbose_debug $display("STATE S3 FIRED"); `endif
	`ifdef verbose_debug_l2
        $display("r_rpt_cnt in BURST = %b", r_rpt_cnt);
        $display("r_burst_cnt in BURST = %b, BSTW=%b", r_burst_cnt,register_ifc.op_side.m_BSTW);
        $display (" r_AD in BURST = %h", r_AD);
        $display("r_AD after WRITE = %h r_R_Wn= %b r_PS = %b r_AD_32bit_data_byte1=%h r_AD_32bit_data_byte2=%h r_AD_32bit_data_byte3=%h", r_AD, r_R_Wn, r_PS, r_AD_32bit_data_byte1,r_AD_32bit_data_byte2,r_AD_32bit_data_byte3);
	`endif
	if (r_ext_TAn == 1'b1) begin // premature external termination SLVERR response
		flexbus_state <= FlexBus_S4_HOLD;
		if (r_R_Wn == 1'b1) begin
    		r_rresp <= AXI4_SLVERR; //SLVERR
        end else begin
    		r_wrbresp <= AXI4_SLVERR; //SLVERR
        end
	end
	else if (r_rpt_cnt == 3'b001) begin
		if (r_R_Wn == 1'b1) begin
			if (r_arsize == 2'b00) begin
                		if (r_PS == 2'b01) begin
					r_rd_data_32bit_byte4 <= r_din[7:0];
				end
                		else if ((r_PS == 2'b10) || (r_PS == 2'b11)) begin
					r_rd_data_32bit_byte3 <= r_din[7:0];
					r_rd_data_32bit_byte4 <= r_din[15:8];
				end
			end
			else if (r_arsize == 2'b10) begin
                		if (r_PS == 2'b01)
					r_rd_data_32bit_byte2 <= r_din[7:0];
			end
			r_rready <= True;
			//r_rpt_cnt <= r_rpt_cnt -1;
		end
                //else
		flexbus_state <= FlexBus_S4_HOLD;
		if (register_ifc.op_side.m_AA == 1'b1) begin // check this functionality  later for now 
			r_OEn <= 1'b1;
			r_BE_BWEn <= 4'hF;
			r_FBCSn <= 6'h3F;
		end
        end
	else if (r_rpt_cnt != 3'b000) begin
		flexbus_state <= FlexBus_S1_ADDR;
		r_ASET <= register_ifc.op_side.m_ASET;
		if (register_ifc.op_side.m_AA == 1'b1) begin
			r_OEn <= 1'b1;
			r_BE_BWEn <= 4'hF;
			r_FBCSn <= 6'h3F;
		end
		if (r_R_Wn == 1'b1) begin
                	if ((r_PS == 2'b01) && (r_rpt_cnt == 3'b100))
				r_rd_data_32bit_byte1 <= r_din[7:0];
                	else if ((r_PS == 2'b01) && (r_rpt_cnt == 3'b011))
				r_rd_data_32bit_byte2 <= r_din[7:0];
       		        else if ((r_PS == 2'b01) && (r_rpt_cnt == 3'b010)) begin
				if (r_arsize == 2'b00)
					r_rd_data_32bit_byte3 <= r_din[7:0];
				else if (r_arsize == 2'b10) 
					r_rd_data_32bit_byte1 <= r_din[7:0];
			end
        	        else if ((r_PS == 2'b10) || (r_PS == 2'b11)) begin
				r_rd_data_32bit_byte1 <= r_din[7:0];
				r_rd_data_32bit_byte2 <= r_din[15:8];
			end
		end
        end
        else if (r_burst_cnt == 2'b01) begin
		if (r_ext_TAn == 1'b1) begin
			flexbus_state <= FlexBus_S4_HOLD;
		end
		else begin
			if (r_R_Wn == 1'b0) begin
				if (r_awsize == 2'b00) begin
                       			if (r_PS == 2'b01) 
						r_AD <= pack({r_AD_32bit_data_byte4,r_AD_32bit_addr_byte2,r_AD_32bit_addr_byte3,r_AD_32bit_addr_byte4});
                       			else if ((r_PS == 2'b10) || (r_PS == 2'b11)) 
						r_AD <= pack({r_AD_32bit_data_byte3,r_AD_32bit_data_byte4,r_AD_32bit_addr_byte3,r_AD_32bit_addr_byte4});
 					//else
					//	r_AD <= pack({r_AD_32bit_data_byte1,r_AD_32bit_data_byte2,r_AD_32bit_data_byte3,r_AD_32bit_data_byte4});
				end
				else if (r_awsize == 2'b10) begin
					if (r_PS == 2'b01) r_AD <= pack({r_AD_32bit_data_byte2,r_AD_32bit_addr_byte2,r_AD_32bit_addr_byte3,r_AD_32bit_addr_byte4});
				end
			end
			else begin
				if (r_arsize == 2'b00) begin
                       			if (r_PS == 2'b01) 
						r_rd_data_32bit_byte3 <= r_din[7:0];
                       			else if ((r_PS == 2'b10) || (r_PS == 2'b11)) begin
						r_rd_data_32bit_byte1 <= r_din[7:0];
						r_rd_data_32bit_byte2 <= r_din[15:8];
					end
				end
				else if (r_arsize == 2'b10) begin
                       			if (r_PS == 2'b01) 
						r_rd_data_32bit_byte1 <= r_din[7:0];
				end
			end
			if (register_ifc.op_side.m_SWS_EN == 1'b1) begin
				if (r_SWS_cnt == 6'h00) begin 
					r_SWS_cnt <= register_ifc.op_side.m_SWS;
					if (register_ifc.op_side.m_AA == 1'b1) begin
						r_int_TAn <= 1'b1;
						r_OEn <= 1'b1;
						r_BE_BWEn <= 4'hF;
						r_FBCSn <= 6'h3F;
                                        end
					r_burst_cnt <= r_burst_cnt -1;
					//flexbus_state <= FlexBus_S4_HOLD;
				end
				else begin
					r_SWS_cnt <= r_SWS_cnt -1;
				end
                        end
                        else begin
     			  	if (r_WS_cnt == 6'h00) begin
					r_WS_cnt <= register_ifc.op_side.m_WS;
					if (register_ifc.op_side.m_AA == 1'b1) begin
						r_int_TAn <= 1'b1;
						r_OEn <= 1'b1;
						r_BE_BWEn <= 4'hF;
						r_FBCSn <= 6'h3F;
					end
					r_burst_cnt <= r_burst_cnt -1;
					//flexbus_state <= FlexBus_S4_HOLD;
			   	end
                           	else
					r_WS_cnt <= r_WS_cnt - 1;
			end
		end
	end
	else if (r_burst_cnt != 2'b00) begin
		if (r_R_Wn == 1'b0) begin
                       	if ((r_PS == 2'b01) && (r_burst_cnt == 2'b11)) 
				r_AD <= pack({r_AD_32bit_data_byte2,r_AD_32bit_addr_byte2,r_AD_32bit_addr_byte3,r_AD_32bit_addr_byte4});
                       	else if ((r_PS == 2'b01) && (r_burst_cnt == 2'b10)) begin 
				r_AD <= pack({r_AD_32bit_data_byte3,r_AD_32bit_addr_byte2,r_AD_32bit_addr_byte3,r_AD_32bit_addr_byte4});
                        end
                       	//else if ((r_PS == 2'b01) && (r_burst_cnt == 2'b01)) 
			//	r_AD <= pack({r_AD_32bit_data_byte3,r_AD_32bit_addr_byte2,r_AD_32bit_addr_byte3,r_AD_32bit_addr_byte4});
               	 	//else if ((r_PS == 2'b10) || (r_PS == 2'b11)) 
			//	r_AD <= pack({r_AD_32bit_data_byte3,r_AD_32bit_data_byte4,r_AD_32bit_addr_byte3,r_AD_32bit_addr_byte4});
		end
		else begin
                       	if ((r_PS == 2'b01) && (r_burst_cnt == 2'b11)) 
				r_rd_data_32bit_byte1 <= r_din[7:0];
                       	else if ((r_PS == 2'b01) && (r_burst_cnt == 2'b10)) begin 
				r_rd_data_32bit_byte2 <= r_din[7:0];
			end
		end
		if (register_ifc.op_side.m_SWS_EN == 1'b1) begin
			if (r_SWS_cnt == 6'h00) begin 
				r_SWS_cnt <= register_ifc.op_side.m_SWS;
				if (register_ifc.op_side.m_AA == 1'b1)
					r_int_TAn <= 1'b1;
				r_burst_cnt <= r_burst_cnt -1;
			end
			else begin
				r_SWS_cnt <= r_SWS_cnt -1;
			end
                end
     		else begin 
			if (r_WS_cnt == 6'h00) begin
				r_WS_cnt <= register_ifc.op_side.m_WS;
				if (register_ifc.op_side.m_AA == 1'b1) 
					r_int_TAn <= 1'b1;
				r_burst_cnt <= r_burst_cnt -1;
			end
                	else begin
				r_WS_cnt <= r_WS_cnt - 1;
			end
               end
	end
	else if (r_burst_cnt == 2'b00) begin 
		flexbus_state <= FlexBus_S4_HOLD;
		if (r_R_Wn == 1'b1) begin
			if (r_arsize == 2'b00) begin
                       		if (r_PS == 2'b01) begin 
					r_rd_data_32bit_byte4 <= r_din[7:0];
				end
                       		else if ((r_PS == 2'b10) || (r_PS == 2'b11)) begin
					r_rd_data_32bit_byte3 <= r_din[7:0];
					r_rd_data_32bit_byte4 <= r_din[15:8];
				end
 				else begin
					r_rd_data_32bit_byte1 <= r_din[7:0];
					r_rd_data_32bit_byte2 <= r_din[15:8];
					r_rd_data_32bit_byte3 <= r_din[23:16];
					r_rd_data_32bit_byte4 <= r_din[31:24];
				end
			end
			else if (r_arsize == 2'b10) begin
                		if (r_PS == 2'b01)
					r_rd_data_32bit_byte2 <= r_din[7:0];
                       		//if ((r_PS == 2'b10) || (r_PS == 2'b11)) begin
				else begin
					r_rd_data_32bit_byte1 <= r_din[7:0];
					r_rd_data_32bit_byte2 <= r_din[15:8];
				end
			end
			else if (r_arsize == 2'b01) begin
				r_rd_data_32bit_byte1 <= r_din[7:0];
			end
			r_rready <= True;
    		end
		if (register_ifc.op_side.m_AA == 1'b1) begin // check this functionality  later for now 
			r_OEn <= 1'b1;
			r_BE_BWEn <= 4'hF;
			r_FBCSn <= 6'h3F;
		end
	end
   endrule

   rule rl_state_S4_HOLD (flexbus_state == FlexBus_S4_HOLD); //Address Phase
	`ifdef verbose_debug $display("STATE S4 FIRED");`endif
	r_int_TAn <= 1'b1;
	r_R_Wn <= 1'b1;
	r_OEn <= 1'b1;
	r_BE_BWEn <= 4'hF;
	r_FBCSn <= 6'h3F;
	r_TBSTn <= 1'b1;
        if (r_hld_cnt ==  2'b00) begin
            if (wr_pending == 1'b1) begin
                flexbus_state <= FlexBus_S0_DEQ_WR_FIFOS;
           	    flexbus_state_wr <= IDLE;
           	    flexbus_state_rd <= IDLE;
                wr_pending <= 1'b0;
            end
            else begin
                flexbus_state <= IDLE;
           	    flexbus_state_wr <= FlexBus_S0_CHK_FIFOS;
           	    flexbus_state_rd <= FlexBus_S0_CHK_FIFOS;
            end
                r1_arvalid  <= False;
                r1_awvalid  <= False;
                r1_wvalid   <= False;

		        r_rready	<= False;
                r_wrbresp	<= AXI4_OKAY;  
                r_rresp	    <= AXI4_OKAY;  
                r_ASET		<= 2'b00;  
                r_rpt_cnt	<= 3'b000;  
                r_burst_cnt	<= 2'b00;  
                r_hld_cnt	<= 2'b00;  
                r_WS_cnt	<= 6'h00;  
                r_SWS_cnt	<= 6'h00;  
                r_awaddr        <= 0;
                r_wdata         <= 0;
                //r_rd_data     <= 0;
                r1_wstrb 	<= 0;
                //r2_wstrb 	<= 0;
                r_araddr        <= 0;
        end
        else 
		r_hld_cnt <= r_hld_cnt -1;
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset;
      `ifdef verbose_debug_l2 $display (" I RESET \n"); `endif
      f_wr_addr.clear;
      f_wr_data.clear;
      f_wr_resp.clear;
      f_rd_addr.clear;
      f_rd_data.clear;
      
      c_TAn[0]<= tagged Invalid;
      c_din[0]<= tagged Invalid;
   endmethod

   // AXI side
   interface axi_side = interface AXI4_Slave_IFC;

			   // Wr Addr channel
			   method Action m_awvalid (Bool           awvalid,
						    Bit #(wd_addr) awaddr,
								Bit#(3) awsize,
						    Bit #(wd_user) awuser,
								Bit#(8) awlen,
								Bit#(2) awburst,
                                Bit#(4) awid
								);
                  if (awvalid && f_wr_addr.notFull) begin
				        f_wr_addr.enq (AXI4_Wr_Addr {awaddr: awaddr,
								   awuser: awuser,
									 awlen:awlen,
									 awsize:awsize,
									 awburst:awburst,
                                     awid:awid});
                    end
			   endmethod

			   method Bool m_awready;
			      return f_wr_addr.notFull;
			   endmethod

			   // Wr Data channel
			   method Action m_wvalid (Bool                      wvalid,
						   Bit #(wd_data)            wdata,
						   Bit #(TDiv #(wd_data, 8)) wstrb,
							 Bool wlast,
							 Bit#(4) wid);
                             if (wvalid && f_wr_data.notFull) begin 
				 f_wr_data.enq (AXI4_Wr_Data {wdata: wdata, wstrb: wstrb, wlast:wlast, wid: wid});
                            end
			   endmethod

			   method Bool m_wready;
			      return f_wr_data.notFull;
			   endmethod

			   // Wr Response channel
			   method Bool           m_bvalid = f_wr_resp.notEmpty;
			   method Bit #(2)       m_bresp  = pack (f_wr_resp.first.bresp);
			   method Bit #(wd_user) m_buser  = f_wr_resp.first.buser;
			   method Bit #(4)       m_bid  = f_wr_resp.first.bid;
			   method Action m_bready (Bool bready);
			      if (bready && f_wr_resp.notEmpty)
				 f_wr_resp.deq;
			   endmethod

			   // Rd Addr channel
			   method Action m_arvalid (Bool           arvalid,
						    Bit #(wd_addr) araddr,
								Bit#(3)				 arsize,
						    Bit #(wd_user) aruser,
								Bit#(8) 			 arlen,
								Bit#(2)				 arburst,
                                Bit#(4) arid);
                                if (arvalid && f_rd_addr.notFull) begin
				 f_rd_addr.enq (AXI4_Rd_Addr {araddr: araddr,
								   aruser: aruser,
									 arlen : arlen,
									 arsize: arsize,
									 arburst:arburst,
                                     arid:arid});
                        end
			   endmethod

			   method Bool m_arready;
			      return f_rd_addr.notFull;
			   endmethod

			   // Rd Data channel
			   method Bool           m_rvalid = f_rd_data.notEmpty;
			   method Bit #(2)       m_rresp  = pack (f_rd_data.first.rresp);
			   method Bit #(wd_data) m_rdata  = f_rd_data.first.rdata;
			   method Bool m_rlast  = f_rd_data.first.rlast;
			   method Bit #(wd_user) m_ruser  = f_rd_data.first.ruser;
			   method Bit#(4) m_rid=f_rd_data.first.rid;

			   method Action m_rready (Bool rready);
			      if (rready && f_rd_data.notEmpty)
				 f_rd_data.deq;
			   endmethod
			endinterface;

interface flexbus_side = interface FlexBus_Master_IFC;
	    //interface io_AD_master = tri_AD_out.io;
        
                         method Action m_TAn (Bit #(1) tAn) if(c_TAn[0] matches tagged Invalid);
                            	c_TAn[0] <= tagged Valid tAn;
                         endmethod
  			   method Action m_din ( Bit#(32) din )if(c_din[0] matches tagged Invalid); 
                            	c_din[0] <= tagged Valid din;
               endmethod
  			 method Bit #(32)   m_AD;            
			  	return r_AD;
             endmethod


  			 method Bit #(1)       m_R_Wn;                                   // out
				return r_R_Wn;
                         endmethod
			 method Bit #(2)       m_TSIZ;                                   // out
				return r_TSIZ;
                         endmethod



  			 method Bit #(6)       m_FBCSn;                                  // out
				return r_FBCSn;
                         endmethod
  			 method Bit #(4)       m_BE_BWEn;                                // out
				return r_BE_BWEn;
                         endmethod
  			 method Bit #(1)       m_TBSTn;                                  // out
				return r_TBSTn;
                         endmethod
  			 method Bit #(1)       m_OEn;                                    // out
				return r_OEn;
                         endmethod

  			 method Bit #(1)       m_ALE;                                    // out
				return r_ALE;
                         endmethod
                //endinterface;

			endinterface;

endmodule: mkAXI4_Slave_to_FlexBus_Master_Xactor

module mkFlexBus_Registers (FlexBus_Register_IFC);

// Vectors of Chip Select AR, MR and Control Registers
        Vector#(6, Reg#(Bit#(32)) ) vec_addr_regs <- replicateM (mkReg(0));
        Vector#(6, Reg#(Bit#(32)) ) vec_mask_regs <- replicateM (mkReg(0));
        Vector#(6, Reg#(Bit#(32)) ) vec_cntr_regs <- replicateM (mkReg(0));

// Control Register Fields

	Reg#(Bit#(6)) r_FBCSn	<- mkReg(6'h3F); 
	Reg#(Bit#(6)) r_SWS	<- mkReg(6'h00); 
	Reg#(Bit#(1)) r_SWS_EN	<- mkReg(1'b0); 
	Reg#(Bit#(2)) r_ASET	<- mkReg(2'b00); 
	Reg#(Bit#(2)) r_RDAH	<- mkReg(2'b00); 
	Reg#(Bit#(2)) r_WRAH	<- mkReg(2'b00); 
	Reg#(Bit#(6)) r_WS	<- mkReg(6'h00); 
	Reg#(Bit#(1)) r_AA	<- mkReg(1'b0); 
	Reg#(Bit#(2)) r_PS	<- mkReg(2'b00); 
	Reg#(Bit#(1)) r_BEM	<- mkReg(1'b0); 
	Reg#(Bit#(1)) r_BSTR	<- mkReg(1'b0); 
	Reg#(Bit#(1)) r_BSTW	<- mkReg(1'b0);

	Reg#(Bit#(32)) r_rom_cntr_reg_0 <- mkReg(0);
	Reg#(Bit#(32)) r_ad_bus         <- mkReg(32'hFFFFFFFF);
	Reg#(Bit#(32)) r_data_bus       <- mkReg(32'h00000000);
	Reg#(Bit#(32)) r_MBAR           <- mkReg(32'h04000000);
//------------------------------------------------------------------------

	rule rl_write_config_regs;
		Bit#(32) v_MBAR = r_MBAR + 'h0500;
	   	for (int i=0; i<6; i=i+1) begin
			if ( v_MBAR == r_ad_bus) begin 
				vec_addr_regs[i][31:16] <= r_data_bus[31:16];
                        end
			v_MBAR = v_MBAR + 'h04;
			if ( v_MBAR == r_ad_bus) begin
				vec_mask_regs[i] <= r_data_bus;
                        end
			v_MBAR = v_MBAR + 'h04;
			if ( v_MBAR == r_ad_bus) begin
				vec_cntr_regs[i] <= r_data_bus;
                        end
			v_MBAR = v_MBAR + 'h04;
		end
	endrule

	rule rl_generate_individual_chip_sels;

	   Bit#(6) chp_sel_vec = 6'h3F;
	   Bit#(32) r_cntr_reg_sel = 32'h00000000;
	   for (int i=0; i<6; i=i+1) begin
		if ((~vec_mask_regs[i] & vec_addr_regs[i]) == (~vec_mask_regs[i] & pack({r_ad_bus[31:16],16'h0000}))) begin
			chp_sel_vec[i] = 1'b0;
		end
           end
	   r_FBCSn <= pack({chp_sel_vec[5],chp_sel_vec[4],chp_sel_vec[3],chp_sel_vec[2],chp_sel_vec[1],chp_sel_vec[0]});

		case (pack({chp_sel_vec[5],chp_sel_vec[4],chp_sel_vec[3],chp_sel_vec[2],chp_sel_vec[1],chp_sel_vec[0]})) matches
                    {6'b111110}: r_cntr_reg_sel   = vec_cntr_regs[0];
                    {6'b111101}: r_cntr_reg_sel   = vec_cntr_regs[1];
                    {6'b111011}: r_cntr_reg_sel   = vec_cntr_regs[2];
                    {6'b110111}: r_cntr_reg_sel   = vec_cntr_regs[3];
                    {6'b101111}: r_cntr_reg_sel   = vec_cntr_regs[4];
                    {6'b011111}: r_cntr_reg_sel   = vec_cntr_regs[5];
		endcase

		r_SWS 		<= r_cntr_reg_sel[31:26];
		r_SWS_EN 	<= r_cntr_reg_sel[23];
		r_ASET		<= r_cntr_reg_sel[21:20];
		r_RDAH		<= r_cntr_reg_sel[19:18];
		r_WRAH		<= r_cntr_reg_sel[17:16];
		//r_WS  		<= r_cntr_reg_sel[15:10];
        r_WS        <= 6'h06;
		r_AA     	<= r_cntr_reg_sel[8];
		r_PS  		<= r_cntr_reg_sel[7:6];
		r_BEM    	<= r_cntr_reg_sel[5];
		r_BSTR   	<= r_cntr_reg_sel[4];
		r_BSTW   	<= r_cntr_reg_sel[3];
	endrule
//-------------------------------------------------------------------------
// FlexBus Register Input Interface
interface inp_side = interface FlexBus_Register_Input_IFC;
        method Action reset (Bit #(32) ad_bus);
		for (int i=0; i<6; i=i+1)
                    vec_addr_regs[i] <= 32'h00000000;
		for (int i=0; i<6; i=i+1)
                    vec_mask_regs[i] <= 32'h00000000;
		for (int i=0; i<6; i=i+1)
                    vec_cntr_regs[i] <= 32'h00000000;
                r_rom_cntr_reg_0[8] <= ad_bus[2];
                r_rom_cntr_reg_0[7:6] <= ad_bus[1:0];
                r_rom_cntr_reg_0[5] <= ad_bus[3];
                r_rom_cntr_reg_0[15:10] <= 6'h3F;
                r_rom_cntr_reg_0[21:16] <= 6'h3F;
		vec_cntr_regs[0] <= r_rom_cntr_reg_0;
        endmethod
        method Action m_ad_bus (Bit #(32) ad_bus);
		r_ad_bus <= ad_bus;
        endmethod
        method Action m_data_bus (Bit #(32) data_bus);
		r_data_bus <= data_bus;
        endmethod
	endinterface;

// FlexBus Register Output Interface
interface op_side = interface FlexBus_Register_Output_IFC;
	method Bit#(6) m_FBCSn ();
               return r_FBCSn;
        endmethod
	method Bit#(6) m_SWS ();
               return r_SWS;
        endmethod
        method Bit#(1) m_SWS_EN (); 
               return r_SWS_EN;
        endmethod
	method Bit#(2) m_ASET ();
               return r_ASET;
        endmethod
	method Bit#(2) m_RDAH ();
               return r_RDAH;
        endmethod
	method Bit#(2) m_WRAH ();
               return r_WRAH;
        endmethod
	method Bit#(6) m_WS ();
               return r_WS;
        endmethod
	method Bit#(1) m_AA ();
               return r_AA;
        endmethod
	method Bit#(2) m_PS ();
               return r_PS;
        endmethod
	method Bit#(1) m_BEM ();
               return r_BEM;
        endmethod
	method Bit#(1) m_BSTR ();
               return r_BSTR;
        endmethod
	method Bit#(1) m_BSTW ();
               return r_BSTW;
        endmethod
	endinterface;

endmodule: mkFlexBus_Registers


endpackage
