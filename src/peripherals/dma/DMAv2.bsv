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
package DMAv2;

// ================================================================
// Copyright (c) Bluespec, Inc., 2007-2011 All Rights Reserved

import FIFO :: * ;
import FIFOF :: * ;
import Vector :: * ;
import FShow::*;
import GetPut::*;
import DefaultValue ::*;
import AXI4_Types   :: *;
import AXI4_Fabric  :: *;
import Semi_FIFOF        :: *;

`include "ARM.defines"

// ================================================================
// DMA requests and responses parameters

`define  Req_Info_sz 10
`define  Req_Addr_sz 32
`define  Req_Data_sz 32

typedef Bit#(`Req_Info_sz) Req_Info; 
typedef Bit#(`Req_Addr_sz) Req_Addr; 
typedef Bit#(`Req_Data_sz) Req_Data; 
//The Req and Resp are of the same width in the current design
//typedef Bit#(10)	RespInfo;
//typedef Bit#(1)	RespAddr;
//typedef Bit#(32)	RespData;

// ----------------------------------------------------------------
// At times it is best to consider registers as completely homogeneous,
// so that they can be accessed as a bit pattern with no internal
// structure.  These functions convert reg interfaces based on a
// structured type to reg interfaces based on a bit pattern of at
// least the same width.

function Reg#(Bit#(n)) regAToRegBitN( Reg#(a_type) rin )
	provisos ( Bits#( a_type, asize),
				 Add#(asize,xxx,n) ) ;

	return
	interface Reg
		method Bit#(n) _read ();
			a_type tmp =  rin._read()  ;
			return zeroExtend (pack( tmp )) ;
		endmethod
		method Action _write( Bit#(n) din );
			rin._write( unpack( truncate(din) )) ;
		endmethod
	endinterface ;
endfunction

// ================================================================
// The DMA interface has two sub-interfaces
//	  A AXI4 Slave interface for config
//	  A AXI4 Master interface for data transfers

interface DmaC #(numeric type numChannels);
	interface AXI4_Master_IFC#(`Req_Addr_sz,`Req_Data_sz,`Req_Info_sz) mmu;
	interface AXI4_Slave_IFC#(`Req_Addr_sz,`Req_Data_sz,`Req_Info_sz) cfg;
endinterface


typedef UInt#(16)  DMACounts ;
// The number of channels is a parameter.
// typedef 2 NumChannels;

// For the module, we add additional configuration registers to control
// which port the transfer reads from and writes to.  
// The majority of the design remains the same, additional fifos, and
// interface must be added, as well as adding new rules to control
// which port is read or written.

// Several configuration registers are included, and connected to the
// config socket/fifo.


module mkDMA( DmaC #(numChannels) );	 

	// The DMA contains one master interface, and one slave interface. The processor sends
	// request through the slave interface to set the config registers.
	// The DMA's master initiates a request to one of the peripherals through one of the
	// channels. The response is taken (through response sub-interface of DMA's Master interface
	// and returned to the processor (through response sub-interface of the DMA's Slave interface 
	AXI4_Slave_Xactor_IFC #(`Req_Addr_sz,`Req_Data_sz,`Req_Info_sz) s_xactor <- mkAXI4_Slave_Xactor;
	AXI4_Master_Xactor_IFC #(`Req_Addr_sz,`Req_Data_sz,`Req_Info_sz) m_xactor <- mkAXI4_Master_Xactor;


	////////////////////////////////////////////////////////////////	
	// We will need some registers to control the DMA transfer
	// A Bit to signal if the transfer is enabled	
	Vector#(numChannels, Reg#(Bool))  dmaEnabledRs	<- replicateM(mkReg(False));

	//  The read address and other stuff needed to generate a read
	Vector#(numChannels, Reg#(Req_Addr))	readAddrRs	<- replicateM(mkReg(0));
	Vector#(numChannels, Reg#(DMACounts)) readCntrRs  <- replicateM(mkReg(0));
	Vector#(numChannels, Reg#(DMACounts)) currentReadRs  <- replicateM(mkReg(0));
	Vector#(numChannels, Reg#(DMACounts)) currentWriteRs <- replicateM(mkReg(0));

	// To distinguish the ports for reads and writes, we need 2 bits
	Vector#(numChannels,Reg#(Bit#(2))) 	portSrcDestRs <- replicateM( mkReg(0)) ;

	// The destination address
	Vector#(numChannels, Reg#(Req_Addr))  destAddrRs	<- replicateM(mkReg(0));

	// Use a FIFO to pass the read response to the write "side",
	//  thus allowing pending transations and concurrency.
	// FIFOs can be replicated as well.
	Vector#(numChannels,FIFO#(Req_Data))  
		  responseDataFs <- replicateM( mkSizedFIFO(2)) ;  

	// We also want to pass the destination address for each read over
	// to the write "side"
	// The depth of this fifo limits the number of outstanding reads
	// which may be pending before the write.  The maximum outstanding
	// reads depends on the overall latency of the read requests.
	Vector#(numChannels,FIFO#(Req_Addr)) 
		destAddrFs <- replicateM( mkSizedFIFO( 4 )) ;			  

	///  DMA rules //////////////////////////////////////////////////
	// We define a function inside the module so it can access some
	// of the registers without passing too many arguments.  
	// The function takes as arguments the conditions and fifos
	// (interfaces)
	// And returns a set a rules.
	// The rule are identical to the set used in the one mmu port case.
	function Rules generatePortDMARules (AXI4_Master_Xactor_IFC#(`Req_Addr_sz,`Req_Data_sz,`Req_Info_sz) xactor,
													 Integer chanNum
													 );
		return
		rules

		// To start a read, when the dma is enabled and there are data to
		// move, and we are in the right state 
		rule startRead (dmaEnabledRs[chanNum]._read && 
						 readCntrRs[chanNum] > currentReadRs[chanNum] );
	  // Create a read request, and enqueue it
	  // Since there can be multiple pending requests, either read or
	  // writes, we use the `Req_Info field to mark these.		
			
			let read_request = AXI4_Rd_Addr {araddr: readAddrRs[chanNum]._read, arprot: 0,
			aruser: fromInteger(0 + 2*chanNum),	arlen: 0, arsize: 2, arburst: 'b0}; // arburst: 00-FIXED 01-INCR 10-WRAP
			xactor.i_rd_addr.enq(read_request);

			// Enqueue the Write destination address
			destAddrFs[chanNum].enq( destAddrRs[chanNum] ) ;

			// Some house keeping -- increment the read address,
			// decrement the counter.
			readAddrRs[chanNum] <= readAddrRs[chanNum] + 4 ;
			currentReadRs[chanNum] <=  currentReadRs[chanNum] + 1  ;
			destAddrRs[chanNum] <=  destAddrRs[chanNum] + 4 ;
			$display ("DMA[%0d] startRead", chanNum);
		endrule


		// We finish the read when we see the correct respInfo on the mmu response fifo
		rule finishRead ( xactor.o_rd_data.first.ruser == fromInteger(0 + 2*chanNum) );
			// grab the data from the mmu reponse fifo
			let resp <- pop_o(xactor.o_rd_data) ;	  

			// Need to consider what to do if the response is an error or
			// fail but we will keep it simple for now

			// Pass the read data to the write "side" of the dma
			responseDataFs[chanNum].enq( resp.rdata ) ;
	 		$display ("DMA[%0d]: finishRead", chanNum);
		endrule


		// This rule start the write process
		// Note that this rule conflicts with rule startRead, so we make
		// this rule be more urgent. I.e., finish writing before you start
		// reading more.			
		rule startWrite;
			// Generate a Write 
      		let write_data = AXI4_Wr_Data {wdata: responseDataFs[chanNum].first, wstrb: 0, wlast:True};
      		let write_addr = AXI4_Wr_Addr {awaddr: destAddrFs[chanNum].first, awprot:0,
								awuser:fromInteger(1 + 2*chanNum), awlen: 0, awsize: 2, awburst: 0}; 

			// enqueue the request.
			xactor.i_wr_data.enq(write_data);
			xactor.i_wr_addr.enq(write_addr);

			// Some other house keeping - removing the data from the fifos
			destAddrFs[chanNum].deq;
			responseDataFs[chanNum].deq;
			$display ("DMA[%0d] startWrite", chanNum);
		endrule

		// This rule waits for the write to finish
		rule finishWrite((xactor.o_wr_resp.first.buser == fromInteger(1 + 2*chanNum)) );
			xactor.o_wr_resp.deq ;			 // take the response data and finish
			currentWriteRs[chanNum]._write (currentWriteRs[chanNum] + 1);
			$display ("DMA[%0d]: finishWrite", chanNum);
		endrule
			
	 endrules;
	endfunction

	function Rules generateTransferDoneRules( Integer chanNum );
		return
		rules
			// Conditions to mark when transfer is done
			rule markTransferDone (dmaEnabledRs[chanNum]._read &&
										 (currentWriteRs[chanNum]._read == readCntrRs[chanNum]._read) &&
										 (currentReadRs[chanNum]._read  == readCntrRs[chanNum]._read) ) ;
				dmaEnabledRs[chanNum]._write (False) ; 
				currentWriteRs[chanNum] <= 0 ;
				currentReadRs[chanNum]  <= 0 ;
		 $display ("DMA[%0d]: transfer done", chanNum);
			endrule
		endrules ;
	endfunction
	
	// Generate the rules, place them in priority order
	//
	Rules ruleset = emptyRules;

	for (Integer ch = 0; ch < valueof (numChannels); ch = ch + 1)
		ruleset = rJoinDescendingUrgency (ruleset,
					generatePortDMARules( m_xactor, ch));

	for (Integer ch = 0; ch < valueof (numChannels); ch = ch + 1)
		ruleset = rJoinDescendingUrgency (ruleset,
					generateTransferDoneRules(ch));

	// Add the rules to this module
	//
	addRules (ruleset);


	///  Rules and other code to interface config port /////////////

	// Add a zero-size register as a default for invalid addresses
	Reg#(Bit#(0)) nullReg <- mkReg( ? ) ;

	// For ease of development we want all registers to look like 32
	// bit resister-- the data size of the config socket.
	// Create function to map from address to specific registers
	// For the multi channel DMA, split the 12 bit address into 2
	// fields, 4 bits to select the channel, 8 for the register.
	
	function Reg#(Req_Data) selectReg( Req_Addr addr ) ;
		Bit#(8) taddr = truncate( addr ) ;
		Bit#(4) channelSel = truncate ( addr >> 8 ) ;
		return
		case ( taddr )
			8'h00 :  return regAToRegBitN( readAddrRs[channelSel] ) ;
			8'h04 :  return regAToRegBitN( readCntrRs[channelSel] ) ;
			8'h08 :  return regAToRegBitN( destAddrRs[channelSel] ) ;
			8'h0C :  return regAToRegBitN( dmaEnabledRs[channelSel] ) ;  
			8'h10 :  return regAToRegBitN( portSrcDestRs[channelSel] ) ;		 
			default:  return regAToRegBitN( nullReg ) ;
		endcase ;
	endfunction


	// A rule for writing into configuration registers
	rule writeConfig;
		let write_addr <- pop_o(s_xactor.o_wr_addr);
		let write_data <- pop_o(s_xactor.o_wr_data);

		$display ("DMA[%0d] writeConfig: ", (write_addr.awaddr[11:8]), fshow (write_addr));

		// Select and write the register 
		let thisReg = selectReg(write_addr.awaddr);
		thisReg <= write_data.wdata;

		// Now generate the response and enqueue
		let resp = AXI4_Wr_Resp { bresp: AXI4_OKAY, buser: write_addr.awuser };
		s_xactor.i_wr_resp.enq(resp);
	endrule

	// A rule for reading a configuration register 
	rule readConfig;
		let read_addr <- pop_o(s_xactor.o_rd_addr);
		// Select the register 
		let thisReg = selectReg(read_addr.araddr) ;
		// Now generate the response and enqueue
		let resp = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: thisReg,
										rlast:True, ruser: read_addr.aruser};
		s_xactor.i_rd_data.enq(resp);
	endrule

	//Note that unknownConfig rule is not needed here because all the FIFOs will
	//be empty and hence neither writeConfig nor readConfig will fire.

	////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////
	//
	// Create the interfaces by connecting the axi side interfaces
	// of the transactors to it.

	interface cfg= s_xactor.axi_side;
	interface mmu= m_xactor.axi_side;
endmodule

endpackage


			
