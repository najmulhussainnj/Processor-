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
// Copyright (c) 2017 Bluespec, Inc.  All Rights Reserved

package AXI4_Types;

// ================================================================
// Facilities for ARM AXI4-Lite, consisting of 5 independent channels:
//   Write Address, Write Data, Write Response, Read Address and Read Data

// Ref: ARM document:
//    AMBA AXI and ACE Protocol Specification
//    AXI3, AXI4, and AXI4-Lite
//    ACE and ACE-Lite
//    ARM IHI 0022E (ID022613)
//    Issue E, 22 Feb 2013

// See export list below

// ================================================================
// Exports

export

// RTL-level interfaces (signals/buses)
AXI4_Master_IFC (..),
AXI4_Slave_IFC (..),


// Higher-level enums and structs for the 5 AXI4 channel payloads
AXI4_Resp (..),

AXI4_Wr_Addr (..),
AXI4_Wr_Data (..),
AXI4_Wr_Resp (..),
AXI4_Rd_Addr (..),
AXI4_Rd_Data (..),

// Higher-level FIFO-like interfaces for the 5 AXI4 channels,
AXI4_Master_Xactor_IFC (..),
AXI4_Master_Fabric_IFC (..),
AXI4_Slave_Xactor_IFC (..),
AXI4_Slave_Fabric_IFC (..),

// Transactors from RTL-level interfacecs to FIFO-like interfaces.
mkAXI4_Master_Xactor,
mkAXI4_Master_Fabric,
mkAXI4_Slave_Xactor,
mkAXI4_Slave_Fabric,
address_increment;

// ================================================================
// BSV library imports

import FIFOF       :: *;
import SpecialFIFOs::*;
import Connectable :: *;
`include "defined_parameters.bsv"

// ----------------
// BSV additional libs

import Semi_FIFOF :: *;

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

// ****************************************************************
// ****************************************************************
// Section: RTL-level interfaces
// ****************************************************************
// ****************************************************************

// ================================================================
// These are the signal-level interfaces for an AXI4-Lite master.
// The (*..*) attributes ensure that when bsc compiles this to Verilog,
// we get exactly the signals specified in the ARM spec.

interface AXI4_Master_IFC #(numeric type wd_addr,
				 numeric type wd_data,
				 numeric type wd_user);
   // Wr Addr channel
   (* always_ready, result="awvalid" *) method Bool           m_awvalid;                                // out
   (* always_ready, result="awaddr" *)  method Bit #(wd_addr) m_awaddr;                                 // out
   (* always_ready, result="awprot" *)  method Bit #(3)       m_awprot;                                 // out
   (* always_ready, result="awuser" *)  method Bit #(wd_user) m_awuser;                                 // out
   (* always_ready, result="awlen" *)   method Bit #(8)  m_awlen;	    			                            // out
   (* always_ready, result="awsize" *)  method Bit #(3)  m_awsize;			                                // out
   (* always_ready, result="awburst" *) method Bit #(2)  m_awburst;			                                // out
   (* always_ready, result="awlock" *) method Bit #(1)		m_awlock;			                                // out
   (* always_ready, result="awcache" *) method Bit #(4)		m_awcache;			                                // out
   (* always_ready, result="awqos" *) method Bit #(4)			m_awqos;			                                // out
   (* always_ready, result="awregion" *) method Bit #(4)  m_awregion;			                                // out
   (* always_ready, result="awid" *) method Bit #(4)			m_awid;			                                // out
   (* always_ready, always_enabled *)   method Action			m_awready ((* port="awready" *) Bool awready);    // in

   // Wr Data channel
   (* always_ready, result="wvalid" *)  method Bool                      m_wvalid;                      // out
   (* always_ready, result="wdata" *)   method Bit #(wd_data)            m_wdata;                       // out
   (* always_ready, result="wstrb" *)   method Bit #(TDiv #(wd_data, 8)) m_wstrb;                       // out
   (* always_ready, result="wlast" *)   method Bool                      m_wlast;                       // out
   (* always_ready, result="wid" *) method Bit #(4)			m_wid;			                                // out
   (* always_ready, always_enabled *)   method Action m_wready ((* port="wready" *)  Bool wready);      // in

   // Wr Response 0hannel
   (* always_ready, always_enabled *)
   method Action m_bvalid ((* port="bvalid" *)  Bool           bvalid,    // in
			   (* port="bresp"  *)  Bit #(2)       bresp,     // in
			   (* port="buser"  *)  Bit #(wd_user) buser,
				 (* port="bid"*) Bit#(4) bid);    // in
   (* always_ready, result="bready" *)  method Bool m_bready;                                            // out

   // Rd Addr channel
   (* always_ready, result="arvalid" *) method Bool            m_arvalid;                               // out
   (* always_ready, result="araddr" *)  method Bit #(wd_addr)  m_araddr;                                // out
   (* always_ready, result="arprot" *)  method Bit #(3)        m_arprot;                                // out
   (* always_ready, result="aruser" *)  method Bit #(wd_user)  m_aruser;                                // out
   (* always_ready, result="arlen" *)   method Bit #(8)  m_arlen;	    			                            // out
   (* always_ready, result="arsize" *)  method Bit #(3)  m_arsize;			                                // out
   (* always_ready, result="arburst" *) method Bit #(2)  m_arburst;			                                // out
   (* always_ready, result="arlock" *) method Bit #(1)		m_arlock;			                                // out
   (* always_ready, result="arcache" *) method Bit #(4)		m_arcache;			                                // out
   (* always_ready, result="arqos" *) method Bit #(4)			m_arqos;			                                // out
   (* always_ready, result="arregion" *) method Bit #(4)  m_arregion;			                                // out
   (* always_ready, result="arid" *) method Bit #(4)			m_arid;			                                // out
   (* always_ready, always_enabled  *)  method Action m_arready ((* port="arready" *) Bool arready);    // in

   // Rd Data channel
   (* always_ready, always_enabled  *)
   method Action m_rvalid ((* port="rvalid" *) Bool           rvalid,    // in
			   (* port="rresp" *)  Bit #(2)       rresp,     // in
			   (* port="rdata" *)  Bit #(wd_data) rdata,     // in
			   (* port="rlast" *)  Bool rlast,     // in
			   (* port="ruser" *)  Bit #(wd_user) ruser,
				 (* port="rid" *)  Bit #(4) rid);    // in
   (* always_ready, result="rready" *)  method Bool m_rready;                                                 // out
endinterface: AXI4_Master_IFC

// ================================================================
// These are the signal-level interfaces for an AXI4-Lite slave.
// The (*..*) attributes ensure that when bsc compiles this to Verilog,
// we get exactly the signals specified in the ARM spec.

interface AXI4_Slave_IFC #(numeric type wd_addr,
				numeric type wd_data,
				numeric type wd_user);
   // Wr Addr channel
//   (* always_ready, always_enabled *)
   method Action m_awvalid ((* port="awvalid" *) Bool           awvalid,    // in
			    (* port="awaddr" *)  Bit #(wd_addr) awaddr,     // in
			    (* port="awprot" *)  Bit #(3)       awprot,     // in
			    (* port="awuser" *)  Bit #(wd_user) awuser,
			    (* port="awlen" *)   Bit #(8) awlen,						// in
			    (* port="awsize" *)   Bit #(3) awsize,						// in
			    (* port="awburst" *)  Bit #(2) awburst,						// in
					(* port="awlock" *) Bit#(1) awlock,
					(* port="awcache" *) Bit#(4) awcache,
					(* port="awqos" *) Bit#(4) awqos,
					(* port="awregion" *) Bit#(4) awregion,
					(* port="awid" *) Bit#(4) awid
					);    // in
   (* always_ready, result="awready" *)
   method Bool m_awready;                                                   // out

   // Wr Data channel
//   (* always_ready, always_enabled *)
   method Action m_wvalid ((* port="wvalid" *) Bool                     wvalid,    // in
			   (* port="wdata" *)  Bit #(wd_data)           wdata,     // in
			   (* port="wstrb" *)  Bit #(TDiv #(wd_data,8)) wstrb,
			   (* port="wlast" *)  Bool  wlast,
				 (* port="wid" *) Bit#(4) wid
				 );    // in
   (* always_ready, result="wready" *)
   method Bool m_wready;                                                           // out

   // Wr Response channel
   (* always_ready, result="bvalid" *)  method Bool           m_bvalid;                                 // out
   (* always_ready, result="bresp" *)   method Bit #(2)       m_bresp;                                  // out
   (* always_ready, result="buser" *)   method Bit #(wd_user) m_buser;                                  // out
   (* always_ready, result="bid" *)   method Bit #(4)       m_bid;                                  // out
   (* always_ready, always_enabled *)   method Action m_bready  ((* port="bready" *)   Bool bready);    // in

   // Rd Addr channel
   (* always_ready, always_enabled *)
   method Action m_arvalid ((* port="arvalid" *) Bool           arvalid,    // in
			    (* port="araddr" *)  Bit #(wd_addr) araddr,     // in
			    (* port="arprot" *)  Bit #(3)       arprot,     // in
			    (* port="aruser" *)  Bit #(wd_user) aruser,
					(* port="arlen" *) 	 Bit #(8)  arlen,	 				// in
        	(* port="arsize" *)	 Bit #(3)  arsize,        // in
        	(* port="arburst" *) Bit #(2)  arburst,       // in
					(* port="arlock" *) Bit#(1) arlock,
					(* port="arcache" *) Bit#(4) arcache,
					(* port="arqos" *) Bit#(4) arqos,
					(* port="arregion" *) Bit#(4) arregion,
					(* port="arid" *) Bit#(4) arid
					);    // in
   (* always_ready, result="arready" *)
   method Bool m_arready;                                                   // out

   // Rd Data channel
   (* always_ready, result="rvalid" *)  method Bool           m_rvalid;                                 // out
   (* always_ready, result="rresp" *)   method Bit #(2)       m_rresp;                                  // out
   (* always_ready, result="rdata" *)   method Bit #(wd_data) m_rdata;                                  // out
   (* always_ready, result="rlast" *)   method Bool m_rlast;                                  // out
   (* always_ready, result="ruser" *)   method Bit #(wd_user) m_ruser;                                  // out
   (* always_ready, result="rid" *)   method Bit #(4)       m_rid;                                  // out
   (* always_ready, always_enabled *)   method Action m_rready  ((* port="rready" *)   Bool rready);    // in
endinterface: AXI4_Slave_IFC

// ================================================================
// Connecting signal-level interfaces

instance Connectable #(AXI4_Master_IFC #(wd_addr, wd_data, wd_user),
		       AXI4_Slave_IFC  #(wd_addr, wd_data, wd_user));

   module mkConnection #(AXI4_Master_IFC #(wd_addr, wd_data, wd_user) axim,
			 AXI4_Slave_IFC  #(wd_addr, wd_data, wd_user) axis)
		       (Empty);

//      (* fire_when_enabled, no_implicit_conditions *)
      (* fire_when_enabled *)
      rule rl_wr_addr_channel;
	 axis.m_awvalid (axim.m_awvalid, axim.m_awaddr, axim.m_awprot, axim.m_awuser, axim.m_awlen, axim.m_awsize, axim.m_awburst,axim.m_awlock,axim.m_awcache,axim.m_awqos,axim.m_awregion,axim.m_awid);
	 axim.m_awready (axis.m_awready);
      endrule

//      (* fire_when_enabled, no_implicit_conditions *)
      (* fire_when_enabled *)
      rule rl_wr_data_channel;
	 axis.m_wvalid (axim.m_wvalid, axim.m_wdata, axim.m_wstrb, axim.m_wlast,axim.m_wid);
	 axim.m_wready (axis.m_wready);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_wr_response_channel;
	 axim.m_bvalid (axis.m_bvalid, axis.m_bresp, axis.m_buser,axis.m_bid);
	 axis.m_bready (axim.m_bready);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_rd_addr_channel;
	 axis.m_arvalid (axim.m_arvalid, axim.m_araddr, axim.m_arprot, axim.m_aruser, axim.m_arlen, axim.m_arsize, axim.m_arburst,axim.m_arlock,axim.m_arcache,axim.m_arqos,axim.m_arregion,axim.m_arid);
	 axim.m_arready (axis.m_arready);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_rd_data_channel;
	 axim.m_rvalid (axis.m_rvalid, axis.m_rresp, axis.m_rdata, axis.m_rlast, axis.m_ruser,axis.m_rid);
	 axis.m_rready (axim.m_rready);
      endrule
   endmodule
endinstance

// ================================================================
// AXI4-Lite dummy slave: never accepts requests, never produces responses

// ****************************************************************
// ****************************************************************
// Section: Higher-level FIFO-like interfaces and transactors
// ****************************************************************
// ****************************************************************

// ================================================================
// Higher-level types for payloads (rather than just bits)

typedef enum { AXI4_OKAY, AXI4_EXOKAY, AXI4_SLVERR, AXI4_DECERR } AXI4_Resp
deriving (Bits, Eq, FShow);

// Write Address channel

typedef struct {
   Bit #(wd_addr)  awaddr;
   Bit #(3)        awprot;
   Bit #(wd_user)  awuser;
	 Bit#(8) 				awlen;
	 Bit#(3) 				awsize;
	 Bit#(2)				awburst;
	 Bit#(1)				awlock;
	 Bit#(4)				awcache;
	 Bit#(4)				awqos;
	 Bit#(4)				awregion;
	 Bit#(4)				awid;
   } AXI4_Wr_Addr #(numeric type wd_addr, numeric type wd_user)
deriving (Bits, FShow);

// Write Data channel

typedef struct {
   Bit #(wd_data)             wdata;
   Bit #(TDiv #(wd_data, 8))  wstrb;
	 Bit#(4) wid;
	 Bool wlast;
   } AXI4_Wr_Data #(numeric type wd_data)
deriving (Bits, FShow);

// Write Response channel

typedef struct {
   AXI4_Resp  bresp;
   Bit #(wd_user)  buser;
	 Bit#(4) bid;
   } AXI4_Wr_Resp #(numeric type wd_user)
deriving (Bits, FShow);

// Read Address channel

typedef struct {
   Bit #(wd_addr)  araddr;
   Bit #(3)        arprot;
   Bit #(wd_user)  aruser;
	 Bit#(8) 				 arlen;
	 Bit#(3) 				 arsize;
	 Bit#(2) 				 arburst;
	 Bit#(1)				arlock;
	 Bit#(4)				arcache;
	 Bit#(4)				arqos;
	 Bit#(4)				arregion;
	 Bit#(4)				arid;
   } AXI4_Rd_Addr #(numeric type wd_addr, numeric type wd_user)
deriving (Bits, FShow);

// Read Data channel

typedef struct {
   AXI4_Resp  rresp;
   Bit #(wd_data)  rdata;
	 Bool 					 rlast;
   Bit #(wd_user)  ruser;
	 Bit#(4)					rid;
   } AXI4_Rd_Data #(numeric type wd_data, numeric type wd_user)
deriving (Bits, FShow);

// ================================================================
// Master transactor interface

interface AXI4_Master_Xactor_IFC #(numeric type wd_addr,
					numeric type wd_data,
					numeric type wd_user);

   // AXI side
   interface AXI4_Master_IFC #(wd_addr, wd_data, wd_user) axi_side;

   // FIFOF side
   interface FIFOF_I #(AXI4_Wr_Addr #(wd_addr, wd_user)) i_wr_addr;
   interface FIFOF_I #(AXI4_Wr_Data #(wd_data))          i_wr_data;
   interface FIFOF_O #(AXI4_Wr_Resp #(wd_user))          o_wr_resp;

   interface FIFOF_I #(AXI4_Rd_Addr #(wd_addr, wd_user)) i_rd_addr;
	 //method Action i_rd_addr(AXI4_Rd_Addr#(wd_addr,wd_user) read_req);
   interface FIFOF_O #(AXI4_Rd_Data #(wd_data, wd_user)) o_rd_data;
endinterface: AXI4_Master_Xactor_IFC

// ----------------------------------------------------------------
// Master transactor

module mkAXI4_Master_Xactor (AXI4_Master_Xactor_IFC #(wd_addr, wd_data, wd_user))
provisos(
			 Add#(a__, 4, wd_addr),
			 Add#(b__, 3, wd_addr),
			 Add#(c__, 2, wd_addr));

   Bool unguarded = True;
   Bool guarded   = False;

   // These FIFOs are guarded on BSV side, unguarded on AXI side
//   FIFOF #(AXI4_Wr_Addr #(wd_addr, wd_user)) f_wr_addr <- mkGLFIFOF (guarded, unguarded);
//   FIFOF #(AXI4_Wr_Data #(wd_data))          f_wr_data <- mkGLFIFOF (guarded, unguarded);
//   FIFOF #(AXI4_Wr_Resp #(wd_user))          f_wr_resp <- mkGLFIFOF (unguarded, guarded);
//
//   FIFOF #(AXI4_Rd_Addr #(wd_addr, wd_user))  f_rd_addr <- mkGLFIFOF (guarded, unguarded);
//   FIFOF #(AXI4_Rd_Data #(wd_data, wd_user)) f_rd_data <- mkGLFIFOF  (unguarded, guarded);
   FIFOF #(AXI4_Wr_Addr #(wd_addr, wd_user)) f_wr_addr <- mkGSizedFIFOF(guarded, unguarded,2);
   FIFOF #(AXI4_Wr_Data #(wd_data))          f_wr_data <- mkGSizedFIFOF(guarded, unguarded,2) ;
   FIFOF #(AXI4_Wr_Resp #(wd_user))          f_wr_resp <- mkGSizedFIFOF(unguarded, guarded,2) ;

   FIFOF #(AXI4_Rd_Addr #(wd_addr, wd_user))  f_rd_addr <- mkGSizedFIFOF(guarded, unguarded,2);
   FIFOF #(AXI4_Rd_Data #(wd_data, wd_user)) f_rd_data <-  mkGSizedFIFOF(unguarded, guarded,2);
   // ----------------------------------------------------------------
   // INTERFACE

   // AXI side
   interface axi_side = interface AXI4_Master_IFC;
			   // Wr Addr channel
			   method Bool						m_awvalid = f_wr_addr.notEmpty;
			   method Bit #(wd_addr)	m_awaddr  = f_wr_addr.first.awaddr;
			   method Bit #(3)				m_awprot  = f_wr_addr.first.awprot;
			   method Bit #(wd_user)	m_awuser  = f_wr_addr.first.awuser;
				 method Bit #(8)				m_awlen   = f_wr_addr.first.awlen;
				 method Bit #(3)				m_awsize	= f_wr_addr.first.awsize;
				 method Bit #(2)				m_awburst = f_wr_addr.first.awburst;
				 method Bit #(1)				m_awlock = f_wr_addr.first.awlock;		
				 method Bit #(4)				m_awcache = f_wr_addr.first.awcache;
				 method Bit #(4)				m_awqos = f_wr_addr.first.awqos;
				 method Bit #(4)				m_awregion  = f_wr_addr.first.awregion;
				 method Bit #(4)				m_awid  = f_wr_addr.first.awid;
			   method Action m_awready (Bool awready);
			      if (f_wr_addr.notEmpty && awready) f_wr_addr.deq;
			   endmethod

			   // Wr Data channel
			   method Bool                       m_wvalid = f_wr_data.notEmpty;
			   method Bit #(wd_data)             m_wdata  = f_wr_data.first.wdata;
			   method Bit #(TDiv #(wd_data, 8))  m_wstrb  = f_wr_data.first.wstrb;
			   method Bool                       m_wlast =  f_wr_data.first.wlast;
				 method Bit#(4)										 m_wid   =	f_wr_data.first.wid;
			   method Action m_wready (Bool wready);
			      if (f_wr_data.notEmpty && wready) f_wr_data.deq;
			   endmethod

			   // Wr Response channel
			   method Action m_bvalid (Bool bvalid, Bit #(2) bresp, Bit #(wd_user) buser, Bit#(4) bid);
			      if (bvalid && f_wr_resp.notFull)
				 f_wr_resp.enq (AXI4_Wr_Resp {bresp: unpack (bresp), buser: buser, bid: bid});
			   endmethod

			   method Bool m_bready;
			      return f_wr_resp.notFull;
			   endmethod

			   // Rd Addr channel
			   method Bool           m_arvalid = f_rd_addr.notEmpty;
			   method Bit #(wd_addr) m_araddr  = f_rd_addr.first.araddr;
			   method Bit #(3)       m_arprot  = f_rd_addr.first.arprot;
			   method Bit #(wd_user) m_aruser  = f_rd_addr.first.aruser;
				 method Bit #(8) 			 m_arlen   = f_rd_addr.first.arlen;
				 method Bit #(3)			 m_arsize	 = f_rd_addr.first.arsize;
				 method Bit #(2)			 m_arburst = f_rd_addr.first.arburst;
				 method Bit #(1)				m_arlock = f_rd_addr.first.arlock;		
				 method Bit #(4)				m_arcache = f_rd_addr.first.arcache;
				 method Bit #(4)				m_arqos = f_rd_addr.first.arqos;
				 method Bit #(4)				m_arregion  = f_rd_addr.first.arregion;
				 method Bit #(4)				m_arid  = f_rd_addr.first.arid;
			   method Action m_arready (Bool arready);
			      if (f_rd_addr.notEmpty && arready) f_rd_addr.deq;
			   endmethod

			   // Rd Data channel
			   method Action m_rvalid (Bool           rvalid,
						   Bit #(2)       rresp,
						   Bit #(wd_data) rdata,
						   Bool rlast,
						   Bit #(wd_user) ruser,
							 Bit#(4) rid);
			      if (rvalid && f_rd_data.notFull)begin
				 f_rd_data.enq (AXI4_Rd_Data {rresp: unpack (rresp),
								   rdata: rdata,
									 rlast:	rlast,
								   ruser: ruser,
									 rid: rid});
							end
			   endmethod

			   method Bool m_rready;
			      return f_rd_data.notFull;
			   endmethod

			endinterface;

   // FIFOF side
   interface i_wr_addr = to_FIFOF_I (f_wr_addr);
   interface i_wr_data = to_FIFOF_I (f_wr_data);
   interface o_wr_resp = to_FIFOF_O (f_wr_resp);

   interface i_rd_addr = to_FIFOF_I (f_rd_addr);
   interface o_rd_data = to_FIFOF_O (f_rd_data);
endmodule: mkAXI4_Master_Xactor
// ================================================================
// Master Fabric interface

interface AXI4_Master_Fabric_IFC #(numeric type wd_addr,
					numeric type wd_data,
					numeric type wd_user);

   // AXI side
   interface AXI4_Master_IFC #(wd_addr, wd_data, wd_user) axi_side;

   // FIFOF side
	 method Action i_wr_addr(AXI4_Wr_Addr#(wd_addr,wd_user) read_address);
	 method Action i_wr_data(AXI4_Wr_Data#(wd_data) read_data);
	 method ActionValue#(AXI4_Wr_Resp#(wd_user)) o_wr_resp ;

	 method Action i_rd_addr(AXI4_Rd_Addr#(wd_addr,wd_user) read_address);
	 method ActionValue#(AXI4_Rd_Data #(wd_data, wd_user)) o_rd_data;
endinterface: AXI4_Master_Fabric_IFC

// ----------------------------------------------------------------
// Master transactor

module mkAXI4_Master_Fabric (AXI4_Master_Fabric_IFC #(wd_addr, wd_data, wd_user))
provisos(
			 Add#(a__, 4, wd_addr),
			 Add#(b__, 3, wd_addr),
			 Add#(c__, 2, wd_addr));

   Bool unguarded = True;
   Bool guarded   = False;

  Reg#(Maybe#(AXI4_Wr_Addr #(wd_addr, wd_user))) f_wr_addr[3] <-mkCReg(3,tagged Invalid);
  Reg#(Maybe#(AXI4_Wr_Data #(wd_data)))          f_wr_data[3] <-mkCReg(3,tagged Invalid);
  Reg#(Maybe#(AXI4_Wr_Resp #(wd_user)))          f_wr_resp[3] <-mkCReg(3,tagged Invalid);


   Reg#(Maybe#(AXI4_Rd_Addr #(wd_addr, wd_user)))  f_rd_addr[3] <- mkCReg(3,tagged Invalid);
   Reg#(Maybe#(AXI4_Rd_Data #(wd_data, wd_user))) f_rd_data[3] <- mkCReg(3, tagged Invalid);
   Reg#(Maybe#(AXI4_Rd_Addr #(wd_addr, wd_user)))  rd_req_reg[2] <- mkCReg(2,tagged Invalid);
	 Reg#(Bit#(8)) rg_read_burst_cycle <-mkReg(0);

		rule generate_read_request(rd_req_reg[1] matches tagged Valid .ar &&& f_rd_addr[0] matches tagged Invalid);
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
			`ifdef verbose $display($time,"\tAXI4MasterWrite: Generating Write Request for Address: %h Data: %h BurstSize: %d BurstLength: %d BurstMode: :%d",ar.awaddr,wd.wdata,ar.awsize,ar.awlen,ar.awburst); `endif
            `ifdef verbose $display("",fshow(ar),fshow(wd),fshow(f_wr_addr[0]),fshow(f_wr_data[0])); `endif
            f_wr_addr[0]<=tagged Valid ar;
			f_wr_data[0]<=tagged Valid wd;
			let info=ar;
			if(ar.awlen==rg_write_burst_cycle) begin// end of burst
				wr_req_reg[1]<= tagged Invalid;
				rg_write_burst_cycle<=0;
			end
			else begin
				info.awaddr=address_increment(ar.awlen,ar.awsize,ar.awburst,ar.awaddr);
				rg_write_burst_cycle<=rg_write_burst_cycle+1;
				wr_req_reg[1]<=tagged Valid info;
			end
			wr_data_reg[1]<= tagged Invalid;
        endrule


   // ----------------------------------------------------------------
   // INTERFACE

   // AXI side
   interface axi_side = interface AXI4_Master_IFC;
			   // Wr Addr channel
			   method Bool           m_awvalid = isValid(f_wr_addr[1]);
			   method Bit #(wd_addr) m_awaddr  = fromMaybe(?,f_wr_addr[1]).awaddr;
			   method Bit #(3)       m_awprot  = fromMaybe(?,f_wr_addr[1]).awprot;
			   method Bit #(wd_user) m_awuser  = fromMaybe(?,f_wr_addr[1]).awuser;
				 method Bit #(8) 			 m_awlen   = fromMaybe(?,f_wr_addr[1]).awlen;
				 method Bit #(3)			 m_awsize	 = fromMaybe(?,f_wr_addr[1]).awsize;
				 method Bit #(2)			 m_awburst = fromMaybe(?,f_wr_addr[1]).awburst;
				 method Bit #(1)			 m_awlock		=fromMaybe(?,f_wr_addr[1]).awlock;		
				 method Bit #(4)			 m_awcache	=fromMaybe(?,f_wr_addr[1]).awcache;
				 method Bit #(4)			 m_awqos		=fromMaybe(?,f_wr_addr[1]).awqos;
				 method Bit #(4)			 m_awregion =fromMaybe(?,f_wr_addr[1]).awregion;
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
			   method Bit #(3)       m_arprot  = fromMaybe(?,f_rd_addr[1]).arprot;
			   method Bit #(wd_user) m_aruser  = fromMaybe(?,f_rd_addr[1]).aruser;
				 method Bit #(8) 			 m_arlen   = fromMaybe(?,f_rd_addr[1]).arlen;
				 method Bit #(3)			 m_arsize	 = fromMaybe(?,f_rd_addr[1]).arsize;
				 method Bit #(2)			 m_arburst = fromMaybe(?,f_rd_addr[1]).arburst;
				 method Bit #(1)			 m_arlock		=fromMaybe(?,f_rd_addr[1]).arlock;		
				 method Bit #(4)			 m_arcache	=fromMaybe(?,f_rd_addr[1]).arcache;
				 method Bit #(4)			 m_arqos		=fromMaybe(?,f_rd_addr[1]).arqos;
				 method Bit #(4)			 m_arregion =fromMaybe(?,f_rd_addr[1]).arregion;
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

   // FIFOF side
	 method Action i_wr_addr(AXI4_Wr_Addr#(wd_addr,wd_user) write_address)if(wr_req_reg[0] matches tagged Invalid);
	 	wr_req_reg[0]<=tagged Valid write_address;
	endmethod
	method Action i_wr_data(AXI4_Wr_Data#(wd_data) write_data) if (wr_data_reg[0] matches tagged Invalid);
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
endmodule: mkAXI4_Master_Fabric

// ================================================================

// ================================================================
// Slave transactor interface

interface AXI4_Slave_Xactor_IFC #(numeric type wd_addr,
				       numeric type wd_data,
				       numeric type wd_user);

   // AXI side
   interface AXI4_Slave_IFC #(wd_addr, wd_data, wd_user) axi_side;

   // FIFOF side
   interface FIFOF_O #(AXI4_Wr_Addr #(wd_addr, wd_user)) o_wr_addr;
   interface FIFOF_O #(AXI4_Wr_Data #(wd_data))          o_wr_data;
   interface FIFOF_I #(AXI4_Wr_Resp #(wd_user))          i_wr_resp;

   interface FIFOF_O #(AXI4_Rd_Addr #(wd_addr, wd_user)) o_rd_addr;
   interface FIFOF_I #(AXI4_Rd_Data #(wd_data, wd_user)) i_rd_data;
endinterface: AXI4_Slave_Xactor_IFC

// ----------------------------------------------------------------
// Slave transactor

module mkAXI4_Slave_Xactor (AXI4_Slave_Xactor_IFC #(wd_addr, wd_data, wd_user));

   Bool unguarded = True;
   Bool guarded   = False;

   // These FIFOs are guarded on BSV side, unguarded on AXI side
   FIFOF #(AXI4_Wr_Addr #(wd_addr, wd_user)) f_wr_addr <- mkGSizedFIFOF (unguarded, guarded,2);
   FIFOF #(AXI4_Wr_Data #(wd_data))          f_wr_data <- mkGSizedFIFOF (unguarded, guarded,2);
   FIFOF #(AXI4_Wr_Resp #(wd_user))          f_wr_resp <- mkGSizedFIFOF (guarded, unguarded,2);

   FIFOF #(AXI4_Rd_Addr #(wd_addr, wd_user)) f_rd_addr <- mkGSizedFIFOF (unguarded, guarded,2);
   FIFOF #(AXI4_Rd_Data #(wd_data, wd_user)) f_rd_data <- mkGSizedFIFOF (guarded, unguarded,2);

   // ----------------------------------------------------------------
   // INTERFACE

   // AXI side
   interface axi_side = interface AXI4_Slave_IFC;
			   // Wr Addr channel
			   method Action m_awvalid (Bool           awvalid,
						    Bit #(wd_addr) awaddr,
						    Bit #(3)       awprot,
						    Bit #(wd_user) awuser,
								Bit#(8) awlen,
								Bit#(3) awsize,
								Bit#(2) awburst,
								Bit#(1) awlock,
								Bit#(4) awcache,
								Bit#(4) awqos,
								Bit#(4) awregion,
								Bit#(4) awid
								)if (f_wr_addr.notFull);
			      if(awvalid) 
				 f_wr_addr.enq (AXI4_Wr_Addr {awaddr: awaddr,
								   awprot: awprot,
								   awuser: awuser,
									 awlen:awlen,
									 awsize:awsize,
									 awburst:awburst,
									 awlock:awlock,
									 awcache:awcache,
									 awqos:awqos,
									 awregion:awregion,
									 awid:awid});
			   endmethod

			   method Bool m_awready;
			      return f_wr_addr.notFull;
			   endmethod

			   // Wr Data channel
			   method Action m_wvalid (Bool                      wvalid,
						   Bit #(wd_data)            wdata,
						   Bit #(TDiv #(wd_data, 8)) wstrb,
							 Bool wlast,
							 Bit#(4) wid) if(f_wr_data.notFull);
			      if (wvalid)
				 f_wr_data.enq (AXI4_Wr_Data {wdata: wdata, wstrb: wstrb, wlast:wlast, wid: wid});
//                 $display($stime,"\t******* Write Data channel Enqueueing ******\n");
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
						    Bit #(3)       arprot,
						    Bit #(wd_user) aruser,
								Bit#(8) 			 arlen,
								Bit#(3)				 arsize,
								Bit#(2)				 arburst,
								Bit#(1) arlock,
								Bit#(4) arcache,
								Bit#(4) arqos,
								Bit#(4) arregion,
								Bit#(4) arid);
			      if (arvalid && f_rd_addr.notFull)
				 f_rd_addr.enq (AXI4_Rd_Addr {araddr: araddr,
								   arprot: arprot,
								   aruser: aruser,
									 arlen : arlen,
									 arsize: arsize,
									 arburst:arburst,
									 arlock:arlock,
									 arcache:arcache,
									 arqos:arqos,
									 arregion:arregion,
									 arid:arid});
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

   // FIFOF side
   interface o_wr_addr = to_FIFOF_O (f_wr_addr);
   interface o_wr_data = to_FIFOF_O (f_wr_data);
   interface i_wr_resp = to_FIFOF_I (f_wr_resp);

   interface o_rd_addr = to_FIFOF_O (f_rd_addr);
   interface i_rd_data = to_FIFOF_I (f_rd_data);
endmodule: mkAXI4_Slave_Xactor

// ================================================================
interface AXI4_Slave_Fabric_IFC #(numeric type wd_addr,
				       numeric type wd_data,
				       numeric type wd_user);

   // AXI side
   interface AXI4_Slave_IFC #(wd_addr, wd_data, wd_user) axi_side;

   // FIFOF side
	 method ActionValue#(AXI4_Wr_Addr#(wd_addr,wd_user)) o_wr_addr;
	 method AXI4_Wr_Addr#(wd_addr,wd_user) o_wr_addr1;
	 method ActionValue#(AXI4_Wr_Data#(wd_data)) o_wr_data;
	 method Action i_wr_resp(AXI4_Wr_Resp#(wd_user) resp);

	 method ActionValue#(AXI4_Rd_Addr#(wd_addr, wd_user)) o_rd_addr;
	 method AXI4_Rd_Addr#(wd_addr, wd_user) o_rd_addr1;
	 method Action i_rd_data(AXI4_Rd_Data#(wd_data,wd_user) read_data);
endinterface: AXI4_Slave_Fabric_IFC


module mkAXI4_Slave_Fabric (AXI4_Slave_Fabric_IFC #(wd_addr, wd_data, wd_user));

   Bool unguarded = True;
   Bool guarded   = False;

   // These FIFOs are guarded on BSV side, unguarded on AXI side
   Reg#(Maybe#(AXI4_Wr_Addr #(wd_addr, wd_user)))  f_wr_addr[3] <-mkCReg(3, tagged Invalid);
   Reg#(Maybe#(AXI4_Wr_Data #(wd_data)))          f_wr_data[3] <-mkCReg(3, tagged Invalid);
   Reg#(Maybe#(AXI4_Wr_Resp #(wd_user)))          f_wr_resp[3] <-mkCReg(3, tagged Invalid);

	 Reg#(Maybe#(AXI4_Rd_Addr #(wd_addr, wd_user))) f_rd_addr[3] <- mkCReg(3,tagged Invalid);
	 Reg#(Maybe#(AXI4_Rd_Data #(wd_data, wd_user))) f_rd_data[3] <- mkCReg(3, tagged Invalid);

   // ----------------------------------------------------------------
   // INTERFACE

   // AXI side
   interface axi_side = interface AXI4_Slave_IFC;
			   // Wr Addr channel
			   method Action m_awvalid (Bool           awvalid,
						    Bit #(wd_addr) awaddr,
						    Bit #(3)       awprot,
						    Bit #(wd_user) awuser,
								Bit#(8) awlen,
								Bit#(3) awsize,
								Bit#(2) awburst,
								Bit#(1) awlock,
								Bit#(4) awcache,
								Bit#(4) awqos,
								Bit#(4) awregion,
								Bit#(4) awid);
			      if (awvalid && !isValid(f_wr_addr[0]))
				 f_wr_addr[0]<=tagged Valid (AXI4_Wr_Addr {awaddr: awaddr,
								   awprot: awprot,
								   awuser: awuser,
									 awlen:awlen,
									 awsize:awsize,
									 awburst:awburst,
									 awlock:awlock,
									 awcache:awcache,
									 awqos:awqos,
									 awregion:awregion,
									 awid:awid});
			   endmethod

			   method Bool m_awready;
			      return !isValid(f_wr_addr[0]);
			   endmethod

			   // Wr Data channel
			   method Action m_wvalid (Bool                      wvalid,
						   Bit #(wd_data)            wdata,
						   Bit #(TDiv #(wd_data, 8)) wstrb,
							 Bool wlast,
							 Bit#(4) wid);
			      if (wvalid && !isValid(f_wr_data[0]))
							 f_wr_data[0]<=tagged Valid (AXI4_Wr_Data {wdata: wdata, wstrb: wstrb, wlast:wlast,wid: wid});
			   endmethod

			   method Bool m_wready;
			      return !isValid(f_wr_data[0]);
			   endmethod

			   // Wr Response channel
			   method Bool           m_bvalid = isValid(f_wr_resp[1]);
			   method Bit #(2)       m_bresp  = pack (fromMaybe(?,f_wr_resp[1]).bresp);
			   method Bit #(wd_user) m_buser  = fromMaybe(?,f_wr_resp[1]).buser;
			   method Action m_bready (Bool bready);
			      if (bready && isValid(f_wr_resp[1]))
						 f_wr_resp[1]<=tagged Invalid;
			   endmethod
			   method Bit #(4)       m_bid  = fromMaybe(?,f_wr_resp[1]).bid;

			   // Rd Addr channel
			   method Action m_arvalid (Bool           arvalid,
						    Bit #(wd_addr) araddr,
						    Bit #(3)       arprot,
						    Bit #(wd_user) aruser,
								Bit#(8) 			 arlen,
								Bit#(3)				 arsize,
								Bit#(2)				 arburst,
								Bit#(1) arlock,
								Bit#(4) arcache,
								Bit#(4) arqos,
								Bit#(4) arregion,
								Bit#(4) arid);
			      if (arvalid && !isValid(f_rd_addr[0]))
				 f_rd_addr[0]<=tagged Valid (AXI4_Rd_Addr {araddr: araddr,
								   arprot: arprot,
								   aruser: aruser,
									 arlen:arlen,
									 arsize:arsize,
									 arburst:arburst,
									 arlock:arlock,
									 arcache:arcache,
									 arqos:arqos,
									 arregion:arregion,
									 arid:arid});
			   endmethod

			   method Bool m_arready;
			      return !isValid(f_rd_addr[0]);
			   endmethod

			   // Rd Data channel
			   method Bool           m_rvalid = isValid(f_rd_data[1]);
			   method Bit #(2)       m_rresp  = pack (fromMaybe(?,f_rd_data[1]).rresp);
			   method Bit #(wd_data) m_rdata  = fromMaybe(?,f_rd_data[1]).rdata;
			   method Bool m_rlast  = fromMaybe(?,f_rd_data[1]).rlast;
			   method Bit #(wd_user) m_ruser  = fromMaybe(?,f_rd_data[1]).ruser;
			   method Action m_rready (Bool rready);
			      if (rready && isValid(f_rd_data[1]))
						 f_rd_data[1]<=tagged Invalid;
			   endmethod
				 method Bit#(4) m_rid=fromMaybe(?,f_rd_data[1]).rid;
			endinterface;

   // FIFOF side
//   interface o_wr_addr = to_FIFOF_O (f_wr_addr);
	 method ActionValue#(AXI4_Wr_Addr#(wd_addr,wd_user)) o_wr_addr if(f_wr_addr[1] matches tagged Valid .write_addr);
	 	f_wr_addr[1]<=tagged Invalid;
		return write_addr;
	 endmethod
	 method AXI4_Wr_Addr#(wd_addr,wd_user) o_wr_addr1 if(f_wr_addr[1] matches tagged Valid .write_addr);
		return write_addr;
	 endmethod
//   interface o_wr_data = to_FIFOF_O (f_wr_data);
	 method ActionValue#(AXI4_Wr_Data#(wd_data)) o_wr_data if(f_wr_data[1] matches tagged Valid .write_data);
	 	f_wr_data[1]<=tagged Invalid;
		return write_data;
	 endmethod
//   interface i_wr_resp = to_FIFOF_I (f_wr_resp);
	 method Action i_wr_resp(AXI4_Wr_Resp#(wd_user) resp)if(f_wr_resp[0] matches tagged Invalid);
	 	f_wr_resp[0]<=tagged Valid resp;
 	 endmethod

	 method ActionValue#(AXI4_Rd_Addr#(wd_addr, wd_user)) o_rd_addr if(f_rd_addr[1] matches tagged Valid .ar);
	 	f_rd_addr[1]<=tagged Invalid;
	 	return ar;
 	 endmethod
	 method AXI4_Rd_Addr#(wd_addr, wd_user) o_rd_addr1;
	 	return fromMaybe(?,f_rd_addr[1]);
 	 endmethod
	 method Action i_rd_data(AXI4_Rd_Data#(wd_data,wd_user) read_data)if(f_rd_data[0] matches tagged Invalid);
	 	f_rd_data[0]<=tagged Valid read_data;
	 endmethod
endmodule: mkAXI4_Slave_Fabric


endpackage
