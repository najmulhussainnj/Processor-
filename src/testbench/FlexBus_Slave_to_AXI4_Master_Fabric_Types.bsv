
package FlexBus_Slave_to_AXI4_Master_Fabric_Types;

// ================================================================
// Exports

export

FlexBus_Slave_to_AXI4_Master_Fabric_IFC (..),

// Transactors from RTL-level interfacecs to FIFO-like interfaces.
mkFlexBus_Slave_to_AXI4_Master_Fabric;

// ================================================================
// BSV library imports

import ConfigReg ::*;
import FIFOF       :: *;
import SpecialFIFOs::*;
import Connectable :: *;
import TriState ::*;
`include "defined_parameters.bsv"

// ----------------
// BSV additional libs

import Semi_FIFOF :: *;

import FlexBus_Types :: *; 
import AXI4_Types   :: *;

function Bit#(wd_addr) address_increment(Bit#(8) arlen, Bit#(3) arsize, Bit#(2) arburst, Bit#(wd_addr) address)
provisos(
                Add#(a__, 4, wd_addr),
                Add#(b__, 3, wd_addr),
                Add#(c__, 2, wd_addr));
                // bit_width_size= (2^arsize)*8
                // arburst = 0(FIXED), 1(INCR) and 2(WRAP)
                       if(arburst==0) // FIXED
                             return address;
                       else if(arburst==1)begin // INCR
                             return address+ (('b1)<<arsize);
                       end
                       else begin // WRAP
                            let new_addr=address;
                            case (arlen)
                                1: new_addr[arsize]=~address[arsize];
                                3: begin
                                     if(arsize==0)
                                           new_addr[1:0]=new_addr[1:0]+1;
                                     else if(arsize==1)
                                           new_addr[2:1]=new_addr[2:1]+1;
                                     else if(arsize==2)
                                           new_addr[3:2]=new_addr[3:2]+1;
                                     else if(arsize==3)
                                           new_addr[4:3]=new_addr[4:3]+1;
                                   end
                                7: begin
                                      if(arsize==0)
                                           new_addr[2:0]=new_addr[2:0]+1;
                                      else if(arsize==1)
                                           new_addr[3:1]=new_addr[3:1]+1;
                                      else if(arsize==2)
                                           new_addr[4:2]=new_addr[4:2]+1;
                                      else if(arsize==3)
                                           new_addr[5:3]=new_addr[5:3]+1;
                                   end
                                15:begin //Bit#(4) x = address[arsize+3:arsize]+1;new_addr[arsize+3:arsize]=x;end
                                      if(arsize==0)
                                            new_addr[3:0]=new_addr[3:0]+1;
                                      else if(arsize==1)
                                            new_addr[4:1]=new_addr[4:1]+1;
                                      else if(arsize==2)
                                            new_addr[5:2]=new_addr[5:2]+1;
                                      else if(arsize==3)
                                            new_addr[6:3]=new_addr[6:3]+1;
                                 end
                           endcase
                           return new_addr;
                    end
               endfunction
// ================================================================
/*
module mkVerfn_Top (Empty);

	FlexBus_Slave_to_AXI4_Master_Fabric_IFC#(32,32,4) verfn_ifc <- mkFlexBus_Slave_to_AXI4_Master_Fabric;

        AXI4_Slave_to_FlexBus_Master_Xactor_IFC#(32, 32, 4)
                                                  flexbus_xactor_ifc <- mkAXI4_Slave_to_FlexBus_Master_Xactor;

        mkConnection(flexbus_xactor_ifc.flexbus_side,verfn_ifc.flexbus_side);

endmodule 
*/
// ================================================================
// Master Fabric interface

interface FlexBus_Slave_to_AXI4_Master_Fabric_IFC #(numeric type wd_addr,
					numeric type wd_data,
					numeric type wd_user);
   method Action reset;

   // FlexBus side
   interface FlexBus_Slave_IFC flexbus_side;

   // AXI side
   interface AXI4_Master_IFC #(wd_addr, wd_data, wd_user) axi_side;

endinterface: FlexBus_Slave_to_AXI4_Master_Fabric_IFC

// ----------------------------------------------------------------
// Master transactor

module mkFlexBus_Slave_to_AXI4_Master_Fabric (FlexBus_Slave_to_AXI4_Master_Fabric_IFC #(wd_addr, wd_data, wd_user))
provisos(
                         Add#(a__, 4, wd_addr),
                         Add#(b__, 3, wd_addr),
                         Add#(c__, 2, wd_addr),
             Bits#(Bit#(32), wd_addr),
             Bits#(Bit#(64), wd_data));

  ConfigReg#(FlexBus_States) flexbus_state_verfn <- mkConfigReg(FlexBus_S1_ADDR);

  Reg#(Bit#(32)) r_AD 	<- mkReg(32'h00000000);
  Reg#(Bit#(1)) r_ALE 		<- mkReg(1'b0);
  Reg#(Bit#(1)) r_R_Wn 		<- mkReg(1'b0);
  Reg#(Bit#(2)) r_TSIZ 		<- mkReg(2'b00);
  Reg#(Bit#(6)) r_FBCSn 	<- mkReg(6'h00);
  Reg#(Bit#(4)) r_BE_BWEn 	<- mkReg(4'h0);
  Reg#(Bit#(1)) r_TBSTn 	<- mkReg(1'b0);
  Reg#(Bit#(1)) r_OEn 		<- mkReg(1'b0);

  Reg#(Bool)    r1_OEn      <- mkReg(True);

  Reg#(Bit#(32)) r_din 	<- mkReg(0);
  Reg#(Bit#(1)) r_TAn 		<- mkReg(1'b0);

  Reg#(Bit#(6)) r_WS_val 	<- mkReg(6'h02);
  Reg#(Bit#(6)) r_WS	 	<- mkReg(6'h00);

   Bool unguarded = True;
   Bool guarded   = False;

  Reg#(Maybe#(AXI4_Wr_Addr #(wd_addr, wd_user))) f_wr_addr[3] <-mkCReg(3,tagged Invalid);
  Reg#(Maybe#(AXI4_Wr_Data #(wd_data)))          f_wr_data[3] <-mkCReg(3,tagged Invalid);
  Reg#(Maybe#(AXI4_Wr_Resp #(wd_user)))          f_wr_resp[3] <-mkCReg(3,tagged Invalid);


   Reg#(Maybe#(AXI4_Rd_Addr #(wd_addr, wd_user)))  f_rd_addr[3] <- mkCReg(3,tagged Invalid);
   Reg#(Maybe#(AXI4_Rd_Data #(wd_data, wd_user))) f_rd_data[3] <- mkCReg(3, tagged Invalid);
   Reg#(Maybe#(AXI4_Rd_Addr #(wd_addr, wd_user)))  rd_req_reg[2] <- mkCReg(2,tagged Invalid);
	 Reg#(Bit#(8)) rg_read_burst_cycle <-mkReg(0);

   //  TriState#(Bit#(32)) tri_AD_in <- mkTriState(!r1_OEn,r_din);

        rule rl_OEn;
            if (r_OEn == 1'b0)
                r1_OEn <= False;
            else
                r1_OEn <= True;
        endrule

     //   rule rl_read_AD_bus;
       //     r_AD <= tri_AD_in._read;
     //   endrule

		rule generate_read_request(rd_req_reg[1] matches tagged Valid .ar &&& f_rd_addr[0] matches tagged Invalid);
			`ifdef verbose_debug_ver $display("generate_read_request FIRED");`endif
			`ifdef verbose $display($time,"\tAXI4MasterRead: Generating Read Request for Address: %h BurstSize: %d BurstLength: %d BurstMode: :%d",ar.araddr,ar.arsize,ar.arlen,ar.arburst); `endif
			f_rd_addr[0]<=tagged Valid ar;
			let info=ar;
			if(ar.arlen==rg_read_burst_cycle) begin// end of burst
				rd_req_reg[1]<= tagged Invalid;
				rg_read_burst_cycle<=0;
			end
			else begin
				info.araddr=address_increment(ar.arlen,ar.arsize,ar.arburst,ar.araddr);
				rg_read_burst_cycle<=rg_read_burst_cycle+1;
				rd_req_reg[1]<=tagged Valid info;
			end
		endrule

   Reg#(Maybe#(AXI4_Wr_Addr #(wd_addr, wd_user)))  wr_req_reg[2] <- mkCReg(2,tagged Invalid);
   Reg#(Maybe#(AXI4_Wr_Data #(wd_data)))  wr_data_reg[2] <- mkCReg(2,tagged Invalid);
	 Reg#(Bit#(8)) rg_write_burst_cycle <-mkReg(0);
	 rule generate_write_request(wr_req_reg[1] matches tagged Valid .ar &&& wr_data_reg[1] matches tagged Valid .wd &&& f_wr_addr[0] matches tagged Invalid &&& f_wr_data[0] matches tagged Invalid);
			`ifdef verbose_debug_ver $display("generate_write_request FIRED"); `endif
			`ifdef verbose $display($time,"\tAXI4MasterWrite: Generating Write Request for Address: %h Data: %h BurstSize: %d BurstLength: %d BurstMode: :%d",ar.awaddr,wd.wdata,ar.awsize,ar.awlen,ar.awburst); `endif
			f_wr_addr[0]<=tagged Valid ar;
			f_wr_data[0]<=tagged Valid wd;
			let info=ar;
			if(ar.awlen==rg_write_burst_cycle) begin// end of burst
				wr_req_reg[1]<= tagged Invalid;
				wr_data_reg[1]<= tagged Invalid;
				rg_write_burst_cycle<=0;
			end
			else begin
				info.awaddr=address_increment(ar.awlen,ar.awsize,ar.awburst,ar.awaddr);
				rg_write_burst_cycle<=rg_write_burst_cycle+1;
				wr_req_reg[1]<=tagged Valid info;
			end
		endrule

		rule rl_generate_addr (r_ALE== 1 && flexbus_state_verfn == FlexBus_S1_ADDR );
			`ifdef verbose_debug_ver $display("STATE S1 ADDR VERFN fired "); `endif
			r_WS <= r_WS_val;
			if (r_R_Wn == 1'b1) begin
				if(rd_req_reg[0] matches tagged Invalid) begin
					rd_req_reg[0]<=tagged Valid (AXI4_Rd_Addr {araddr : pack({r_AD}),
									  aruser : 0,
									  arsize : 3'h2,
									  arlen  : 8'h00,
									  arburst: 2'b00,
									  arid : 0
									});
				end
			end
 			else begin
				if(wr_req_reg[0] matches tagged Invalid) begin
					wr_req_reg[0]<=tagged Valid (AXI4_Wr_Addr {awaddr :pack({r_AD}),
									  awuser : 0,
									  awsize : 3'h2,
									  awlen  : 8'h00,
									  awburst: 2'b00,
									  awid : 0
									});
				end
			end
           		flexbus_state_verfn <= FlexBus_S2_WRITE; 
		endrule
		rule rl_state_S2_WRITE (flexbus_state_verfn == FlexBus_S2_WRITE); //Write Phase
			`ifdef verbose_debug_ver $display("STATE S2 WRITE VERFN FIRED"); `endif
			if (r_R_Wn == 1'b0) begin
				if(wr_data_reg[0] matches tagged Invalid) begin
	 				wr_data_reg[0]<=tagged Valid (AXI4_Wr_Data{wdata: pack({32'h00000000,r_AD[7:0],r_AD[15:8],r_AD[23:16],r_AD[31:24]}),
									 wstrb	: 8'h0F,
									 wid 	: 0,
									 wlast 	: True
									});
				end
			end
			if (r_WS == 0) begin
           			flexbus_state_verfn <= FlexBus_S3_BURST; 
				r_WS <= r_WS_val;
			end
			else
				r_WS <= r_WS -1;
		endrule
		rule rl_state_S3_BURST (flexbus_state_verfn == FlexBus_S3_BURST); //Burst Phase
			`ifdef verbose_debug_ver $display("STATE S3 BURST VERFN FIRED"); `endif
			if (r_R_Wn == 1'b1)  begin
				if(f_rd_data[1] matches tagged Valid .ar) begin	
  					r_din <= ar.rdata[31:0]; 
					`ifdef verbose_debug_ver $display("r_din = %h %h", r_din, ar.rdata); `endif
					f_rd_data[1]<=tagged Invalid;
				end
			end
           		flexbus_state_verfn <= FlexBus_S1_ADDR; 
		endrule

/*
   // FIFOF side
	 method Action i_wr_addr(AXI4_Wr_Addr#(wd_addr,wd_user) write_address)if(wr_req_reg[0] matches tagged Invalid);
	 	wr_req_reg[0]<=tagged Valid write_address;
	endmethod
	method Action i_wr_data(AXI4_Wr_Data#(wd_data) write_data);
	 	wr_data_reg[0]<=tagged Valid write_data;
	endmethod
	 method ActionValue#(AXI4_Wr_Resp#(wd_user)) o_wr_resp if(f_wr_resp[1] matches tagged Valid .aresp);
	 	f_wr_resp[1]<=tagged Invalid;
	 	return aresp;
	 endmethod
	 method Action i_rd_addr(AXI4_Rd_Addr#(wd_addr,wd_user) read_address)if(rd_req_reg[0] matches tagged Invalid);
	 	rd_req_reg[0]<=tagged Valid read_address;
	endmethod
	 method ActionValue#(AXI4_Rd_Data #(wd_data, wd_user)) o_rd_data if(f_rd_data[1] matches tagged Valid .ar);
	 	f_rd_data[1]<=tagged Invalid;
		return ar;
	 endmethod
*/
   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset;
      f_wr_addr[2]<=tagged Invalid;
      f_wr_data[2]<=tagged Invalid;
      f_wr_resp[2]<=tagged Invalid;
      f_rd_addr[2]<=tagged Invalid;
      f_rd_data[2]<=tagged Invalid;
   endmethod

   // AXI side
   interface axi_side = interface AXI4_Master_IFC;
			   // Wr Addr channel
			   method Bool           m_awvalid = isValid(f_wr_addr[1]);
			   method Bit #(wd_addr) m_awaddr  = fromMaybe(?,f_wr_addr[1]).awaddr;
			   method Bit #(wd_user) m_awuser  = fromMaybe(?,f_wr_addr[1]).awuser;
				 method Bit #(3)			 m_awsize	 = fromMaybe(?,f_wr_addr[1]).awsize;
				 method Bit #(8) 			 m_awlen   = fromMaybe(?,f_wr_addr[1]).awlen;
				 method Bit #(2)			 m_awburst = fromMaybe(?,f_wr_addr[1]).awburst;
				 method Bit #(4)			 m_awid			=fromMaybe(?,f_wr_addr[1]).awid;
			   method Action m_awready (Bool awready);
			      if (isValid(f_wr_addr[1]) && awready) f_wr_addr[1]<=tagged Invalid;
			   endmethod

			   // Wr Data channel
			   method Bool                       m_wvalid = isValid(f_wr_data[1]);
			   method Bit #(wd_data)             m_wdata  = fromMaybe(?,f_wr_data[1]).wdata;
			   method Bit #(TDiv #(wd_data, 8))  m_wstrb  = fromMaybe(?,f_wr_data[1]).wstrb;
			   method Bool                       m_wlast =  fromMaybe(?,f_wr_data[1]).wlast;
			   method Bit#(4)										 m_wid =		fromMaybe(?,f_wr_data[1]).wid;
			   method Action m_wready (Bool wready);
			      if (isValid(f_wr_data[1]) && wready) f_wr_data[1]<=tagged Invalid;
			   endmethod

			   // Wr Response channel
			   method Action m_bvalid (Bool bvalid, Bit #(2) bresp, Bit #(wd_user) buser, Bit#(4) bid);
			      if (bvalid && !isValid(f_wr_resp[0]))
							 f_wr_resp[0]<=tagged Valid (AXI4_Wr_Resp {bresp: unpack (bresp), buser: buser, bid: bid});
			   endmethod

			   method Bool m_bready;
			      return !isValid(f_wr_resp[0]);
			   endmethod

			   // Rd Addr channel
			   method Bool           m_arvalid = isValid(f_rd_addr[1]);
			   method Bit #(wd_addr) m_araddr  = fromMaybe(?,f_rd_addr[1]).araddr;
			   method Bit #(wd_user) m_aruser  = fromMaybe(?,f_rd_addr[1]).aruser;
				 method Bit #(3)			 m_arsize	 = fromMaybe(?,f_rd_addr[1]).arsize;
				 method Bit #(8) 			 m_arlen   = fromMaybe(?,f_rd_addr[1]).arlen;
				 method Bit #(2)			 m_arburst = fromMaybe(?,f_rd_addr[1]).arburst;
				 method Bit #(4)			 m_arid			=fromMaybe(?,f_rd_addr[1]).arid;
			   method Action m_arready (Bool arready);
			      if (isValid(f_rd_addr[1]) && arready) 
							f_rd_addr[1]<=tagged Invalid;
			   endmethod

			   // Rd Data channel
			   method Action m_rvalid (Bool           rvalid,
						   Bit #(2)       rresp,
						   Bit #(wd_data) rdata,
						   Bool rlast,
						   Bit #(wd_user) ruser,
							 Bit#(4) rid);
			      if (rvalid && !isValid(f_rd_data[0]))
				 f_rd_data[0]<=tagged Valid (AXI4_Rd_Data {rresp: unpack (rresp),
								   rdata: rdata,
									 rlast: rlast,
								   ruser: ruser,
									 rid: rid});
			   endmethod

			   method Bool m_rready;
			      return !isValid(f_rd_data[0]);
			   endmethod

			endinterface;

   interface flexbus_side = interface FlexBus_Slave_IFC;
    //   interface io_AD_slave = tri_AD_in.io;
	method Action m_AD             (  Bit #(32)   i_AD);                            // in
		r_AD <= i_AD;
	endmethod
    //interface i_not_AD_s = interface Not_AD_s;
	method Action m_ALE            (  Bit #(1)         i_ALE);                           // in
		r_ALE <= i_ALE;
	endmethod

	method Action m_R_Wn           (  Bit #(1)         i_R_Wn);                          // in
		r_R_Wn <= i_R_Wn;
	endmethod
	method Action m_TSIZ           (  Bit #(2)         i_TSIZ);                          // in
		r_TSIZ <= i_TSIZ;
	endmethod

	method Action m_FBCSn          (  Bit #(6)         i_FBCSn);                         // in
		r_FBCSn <= i_FBCSn;
	endmethod
	method Action m_BE_BWEn        (  Bit #(4)         i_BE_BWEn);                       // in
		r_BE_BWEn <= i_BE_BWEn;
	endmethod
	method Action m_TBSTn          (  Bit #(1)         i_TBSTn);                         // in
		r_TBSTn <= i_TBSTn;
	endmethod
	method Action m_OEn            (  Bit #(1)         i_OEn);                           // in
		r_OEn <= i_OEn;
	endmethod

	method Bit #(32) m_din = r_din;				//out
	method Bit #(1) m_TAn = r_TAn;					//out
    //endinterface;
			    endinterface;

endmodule: mkFlexBus_Slave_to_AXI4_Master_Fabric

// ================================================================

endpackage
