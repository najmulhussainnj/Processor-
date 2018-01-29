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
package DMA ;

// ================================================================
// Copyright (c) Bluespec, Inc., 2007-2011 All Rights Reserved

import FIFO :: * ;
import FIFOF :: * ;
import Vector :: * ;
import GDefines::*;
import FShow::*;
import GetPut::*;
import DefaultValue ::*;

`include "ARM.defines"

// ================================================================
// DMA requests and responses

// ----------------
// Requests

typedef enum { NOP, WR, RD } ReqOp
		  deriving (Bits, Eq);

instance FShow #(ReqOp);
	function Fmt fshow (ReqOp op);
		case (op)
	 NOP : return fshow ("NOP");
	 WR  : return fshow ("WR");
	 RD  : return fshow ("RD");
		endcase
	endfunction
endinstance

typedef Bit#(10)  ReqInfo;
typedef Bit#(32)  ReqAddr;
typedef Bit#(32)  ReqData;

typedef struct {
  ReqOp						reqOp;
  ReqInfo					reqInfo;
  ReqAddr					reqAddr;
  ReqData					reqData;
} Socket_Req
  deriving (Bits);

instance FShow #(Socket_Req);
	function Fmt fshow (Socket_Req req);
		return (fshow ("Socket_Req{")
			+
			fshow (req.reqOp)
			+
			(  (req.reqOp() != NOP)
			 ? $format (", %h, %h, %h}", req.reqInfo(), req.reqAddr(), req.reqData())
			 : fshow (""))
			+
			fshow ("}"));
	endfunction
endinstance

// ================================================================
// Responses

typedef enum { NOP, OK } RespOp
		  deriving (Bits, Eq);

instance FShow #(RespOp);
	function Fmt fshow (RespOp op);
		case (op)
	 NOP  : return fshow ("NOP");
	 OK	: return fshow ("OK");
		endcase
	endfunction
endinstance

typedef Bit#(10)	RespInfo;
typedef Bit#(1)	 RespAddr;
typedef Bit#(32)	RespData;

typedef struct {
  ReqOp						reqOp;
  RespOp					  respOp;
  RespInfo					respInfo;
  RespAddr					respAddr;
  RespData					respData;
} Socket_Resp
  deriving (Eq, Bits);

instance FShow #(Socket_Resp);
	function Fmt fshow (Socket_Resp resp);
		return (fshow ("Socket_Resp{")
			+
			fshow (resp.respOp())
			+
			(  (resp.respOp() != NOP)
			 ? $format (", %h, %h, %h", resp.respInfo(), resp.respAddr(), resp.respData())
			 : fshow (""))
			+
			fshow ("}"));
	endfunction
endinstance

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
//	  A TLM Receive interface (slave) for config
//	  A TLM Send	 interface (master) for data transfers

interface DmaC #(numeric type numChannels);
	interface AXI4_Lite_Master_IFC#(`PADDR, `Reg_width, 0) cfg;
	interface TLMRecvIFC#(`ARM_RR) cfg;
	interface TLMSendIFC#(`ARM_RR) mmu;
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

	// For each socket, we will need 2 fifos, one for request, and 1
	// for response.  These fifos provide the interface for the
	// interface sockets
	////////////////////////////////////////////////////////////////
	// The fifos for the config port -- these are 1 element pipeline fifos.
	(* doc = "a fifo to hold incoming configuration requests" *)
	FIFOF#(Socket_Req)	cnfReqF  <- mkFIFOF ;
	(* doc = "a fifo to hold outgoing configuration responses" *)
	FIFOF#(Socket_Resp)  cnfRespF <- mkFIFOF;


	////////////////////////////////////////////////////////////////
	// The fifos for the MMU port
	(* doc = "a fifo to hold outgoing requests towards the memory" *)
	FIFOF#(Socket_Req)  mmuReqF  <- mkFIFOF;
	(* doc = "a fifo to hold incoming responses from the memory" *)
	FIFOF#(Socket_Resp) mmuRespF <- mkFIFOF;


	////////////////////////////////////////////////////////////////	
	// The fifos for the config port -- these are 1 element pipeline fifos.
//	 FIFOF#(Socket_Req)	cnfReqF  <- mkGSizedFIFOF(True, False, 2) ;
//	 FIFOF#(Socket_Resp)  cnfRespF <- mkGSizedFIFOF(False, True, 2)  ;

//	 // The fifos for the MMU
//	 FIFOF#(Socket_Req)  mmuReqF  <- mkGSizedFIFOF(False, True, 2);
//	 FIFOF#(Socket_Resp) mmuRespF <- mkGSizedFIFOF(True, False, 2);

//	 // The fifos for the MMU2
//	 FIFOF#(Socket_Req)  mmu2ReqF  <- mkGSizedFIFOF(False, True, 2);
//	 FIFOF#(Socket_Resp) mmu2RespF <- mkGSizedFIFOF(True, False, 2);

	////////////////////////////////////////////////////////////////	
	// We will need some registers to control the DMA transfer
	// A Bit to signal if the transfer is enabled	
	Vector#(numChannels, Reg#(Bool))  dmaEnabledRs	<- replicateM(mkReg(False));

	//  The read address and other stuff needed to generate a read
	Vector#(numChannels, Reg#(ReqAddr))	readAddrRs	<- replicateM(mkReg(0));
	Vector#(numChannels, Reg#(DMACounts)) readCntrRs  <- replicateM(mkReg(0));
	Vector#(numChannels, Reg#(DMACounts)) currentReadRs  <- replicateM(mkReg(0));
	Vector#(numChannels, Reg#(DMACounts)) currentWriteRs <- replicateM(mkReg(0));

	// To distinguish the ports for reads and writes, we need 2 bits
	Vector#(numChannels,Reg#(Bit#(2))) 	portSrcDestRs <- replicateM( mkReg(0)) ;

	// The destination address
	Vector#(numChannels, Reg#(ReqAddr))  destAddrRs	<- replicateM(mkReg(0));

	// Use a FIFO to pass the read response to the write "side",
	//  thus allowing pending transations and concurrency.
	// FIFOs can be replicated as well.
	Vector#(numChannels,FIFO#(ReqData))  
		  responseDataFs <- replicateM( mkSizedFIFO(2)) ;  

	// We also want to pass the destination address for each read over
	// to the write "side"
	// The depth of this fifo limits the number of outstanding reads
	// which may be pending before the write.  The maximum outstanding
	// reads depends on the overall latency of the read requests.
	Vector#(numChannels,FIFO#(ReqAddr)) 
		destAddrFs <- replicateM( mkSizedFIFO( 4 )) ;			  

	///  DMA rules //////////////////////////////////////////////////
	// We define a function inside the module so it can access some
	// of the registers without passing too many arguments.  
	// The function takes as arguments the conditions and fifos
	// (interfaces)
	// And returns a set a rules.
	// The rule are identical to the set used in the one mmu port case.
	function Rules generatePortDMARules (FIFOF#(Socket_Req)  requestF,
													 FIFOF#(Socket_Resp) responseF,
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
	  // writes, we use the reqInfo field to mark these.		
			
		//let wa= AXI4_Lite_Wr_Addr {awaddr: reqs.first.reqAddr, awprot:0, awuser:0, awlen: 0, awsize: 3, awburst: 'b01};
			let req = Socket_Req {reqAddr : readAddrRs[chanNum]._read,
									 reqData : 0,
									 reqOp	: RD,
									 reqInfo : fromInteger(0 + 2*chanNum)};
			requestF.enq( req ) ;

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
			rule finishRead ( responseF.first.respInfo == fromInteger(0 + 2*chanNum) );
				// grab the data from the mmu reponse fifo
				Socket_Resp resp = responseF.first ;	  
				responseF.deq ;

				// Need to consider what to do if the response is an error or
				// fail but we will keep it simple for now

				// Pass the read data to the write "side" of the dma
				responseDataFs[chanNum].enq( resp.respData ) ;
		 $display ("DMA[%0d]: finishRead", chanNum);
			endrule

		// This rule start the write process
		// Note that this rule conflicts with rule startRead, so we make
		// this rule be more urgent. I.e., finish writing before you start
		// reading more.			
			
		rule startWrite  ;
			// Generate a Write 
			let wreq = Socket_Req {reqAddr : destAddrFs[chanNum].first,
									  reqData : responseDataFs[chanNum].first,
									  reqOp	: WR,
									  reqInfo : fromInteger(1 + 2*chanNum) } ;
										  
			// enqueue the request.
			requestF.enq( wreq ) ;

			// Some other house keeping - removing the data from the fifos
			destAddrFs[chanNum].deq;
			responseDataFs[chanNum].deq;
			$display ("DMA[%0d] startWrite", chanNum);
		endrule

		// This rule waits for the write to finish
		rule finishWrite((responseF.first.respInfo == fromInteger(1 + 2*chanNum)) );
			responseF.deq ;			 // take the response data and finish
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
					generatePortDMARules( mmuReqF, mmuRespF, ch));

	for (Integer ch = 0; ch < valueof (numChannels); ch = ch + 1)
		ruleset = rJoinDescendingUrgency (ruleset,
					generateTransferDoneRules(ch));

	// Add the rules to this module
	//
	addRules (ruleset);



	
	
//	 // Now we can generate the rules which can be manipulated further.
//	 Rules r01 = generatePortDMARules( portSrcDestRs[0]._read[0] == 0,
//												  portSrcDestRs[0]._read[1] == 0,
//												 mmu1ReqF, mmu1RespF, 0 ) ;
//	 Rules r02 = generatePortDMARules( portSrcDestRs[0]._read[0] == 1,
//												  portSrcDestRs[0]._read[1] == 1,
//												 mmu2ReqF, mmu2RespF, 0 ) ;
//	 Rules r11 = generatePortDMARules( portSrcDestRs[1]._read[0] == 0,
//												  portSrcDestRs[1]._read[1] == 0,
//												 mmu1ReqF, mmu1RespF, 1 ) ;
//	 Rules r12 = generatePortDMARules( portSrcDestRs[1]._read[0] == 1,
//												  portSrcDestRs[1]._read[1] == 1,
//												 mmu2ReqF, mmu2RespF, 1 ) ;
//	 Rules td0 = generateTransferDoneRules(0) ;
//	 Rules td1 = generateTransferDoneRules(1) ;
	
//	 // The set of rules above create a conflict because 2 startRead
//	 // conflict.  To specify priority of the DMA channels, we relate
//	 // the urgency of the rules by joining the rules with following
//	 // rule functions.
//	 Rules r1 = rJoinDescendingUrgency( r01, r11) ;
//	 Rules r2 = rJoinDescendingUrgency( r02, r12) ;
	
//	 r2 = rJoinDescendingUrgency( r1, r2 ) ;
//	 r2 = rJoinDescendingUrgency( r2, td0 ) ;
//	 r2 = rJoinDescendingUrgency( r2, td1 ) ;
 
//	 addRules( r2 ) ;  
	
	
	///  Rules and other code to interface config port /////////////

	// Add a zero-size register as a default for invalid addresses
	Reg#(Bit#(0)) nullReg <- mkReg( ? ) ;

	// For ease of development we want all registers to look like 32
	// bit resister-- the data size of the config socket.
	// Create function to map from address to specific registers
	// For the multi channel DMA, split the 12 bit address into 2
	// fields, 4 bits to select the channel, 8 for the register.
	
	function Reg#(ReqData) selectReg( ReqAddr addr ) ;
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


	// A rule for writing to a registers
	rule writeConfig ( cnfReqF.first.reqOp == WR ) ;
		let req =  cnfReqF.first ;
		cnfReqF.deq ;

		$display ("DMA[%0d] writeConfig: ", (req.reqAddr[11:8]), fshow (req));

		// Select and write the register 
		let thisReg = selectReg( req.reqAddr ) ;
		thisReg <= req.reqData ;

		// Now generate the response and enqueue
		let resp = Socket_Resp {respOp	: OK,
										reqOp	 : WR,
										respAddr : 0,
										respInfo : req.reqInfo,
										respData : req.reqData } ;
		cnfRespF.enq( resp ) ;
	endrule

	// A rule for reading a configuration register 
	rule readConfig ( cnfReqF.first.reqOp == RD ) ;
		let req =  cnfReqF.first ;
		cnfReqF.deq ;

		// Select the register 
		let thisReg = selectReg( req.reqAddr ) ;

		// Now generate the response and enqueue
		let resp = Socket_Resp {respOp	: OK,
										reqOp	 : RD,
										respAddr : 0,
										respInfo : req.reqInfo,
										respData : thisReg } ;
		cnfRespF.enq( resp ) ;
	endrule

	(* descending_urgency = 
	 "writeConfig, readConfig, unknownConfig"  *)
	rule unknownConfig ( True ) ;
		let req =  cnfReqF.first ;
		cnfReqF.deq ;

		// Select the register 
		let thisReg = selectReg( req.reqAddr ) ;

		// Now generate the response and enqueue
		let resp = Socket_Resp {respOp	: NOP,
										reqOp	 : NOP,
										respAddr : 0,
										respInfo : req.reqInfo,
										respData : thisReg } ;
		cnfRespF.enq( resp ) ;
	endrule

	function AXI4_Lite_Master_Xactor_IFC#(wd_addr,wd_dara,wd_user)
		fifos_to_AXI4_Lite_ifc (FIFOF#(Socket_Req) reqs,
                               FIFOF#(Socket_Resp) resps);
		return
		(interface AXI4_Lite_Master_Xactor_IFC#(wd_addr,wd_data,wd_user)
			//interface AXI4_Lite_Master_IFC #(wd_addr, wd_data, wd_user) axi_side;
			interface FIFOF_I #(AXI4_Lite_Wr_Addr #(wd_addr, wd_user))
				i_wr_addr= fifos_to_AXI4_Lite_Wr_Addr(reqs);
			interface FIFOF_I #(AXI4_Lite_Wr_Data #(wd_data))
				i_wr_data= fifos_to_AXI4_Lite_Wr_Data(reqs);
			interface FIFOF_O #(AXI4_Lite_Wr_Resp #(wd_user))
				o_wr_resp= fifos_to_AXI4_Lite_Wr_Resp(resps);

			interface FIFOF_I #(AXI4_Lite_Rd_Addr #(wd_addr, wd_user));
				i_rd_addr= fifos_to_AXI4_Lite_Rd_Addr(reqs);
			interface FIFOF_O #(AXI4_Lite_Rd_Data #(wd_data, wd_user))
				o_rd_data= fifos_to_AXI4_Lite_Rd_Data(resps);
		endinterface);
	endfunction

	function FIFO_I#(AXI4_Lite_Wr_Addr #(wd_addr, wd_user))
		fifos_to_AXI4_Lite_Wr_Addr#(FIFOF#(Socket_Req) reqs);
		return interface FIFO_I
			method Action  enq(din);
				let x= Socket_Req {reqOp: defaultValue,
                                   reqInfo: defaultValue,
                                   reqAddr: din.awaddr,
                                   reqData: defaultValue};

				reqs.enq (x);
			endmethod
			method Bool notFull()= reqs.notFull;
		endinterface
	endfunction


	////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////
	//
	// Create the interfaces by connecting the fifo interfaces to the
	//  the socket ports.

	interface TLMRecvIFC cfg;
		interface Get tx;
			method ActionValue#(BusResponse) get;
				cnfRespF.deq();
				return socketRespToBusResp(cnfRespF.first());
			endmethod
		endinterface
		interface Put rx;
			method Action put(x) = cnfReqF.enq(busReqToSocketReq(x));
		endinterface
	endinterface




	AXI4_Lite_Master_Xactor_IFC #(`PADDR,`Reg_width,0) m_xactor <- mkAXI4_Lite_Master_Xactor;
	interface AXI4_Lite_Master_IFC#(`PADDR, `Reg_width, 0) cfg;
		


	interface TLMSendIFC mmu;
		interface Get tx;
			method ActionValue#(BusRequest) get;
				mmuReqF.deq();
				return socketReqToBusReq(mmuReqF.first());
			endmethod
		endinterface
		interface Put rx;
			method Action put(x) = mmuRespF.enq(busRespToSocketResp(x));
		endinterface
	endinterface

endmodule

// ================================================================

function BusResponse socketRespToBusResp(Socket_Resp tmp);
	BusResponse r = defaultValue;
	r.error = !(tmp.respOp==OK);
	r.data = tmp.respData;
	r.write = (tmp.reqOp==WR);
	r.id = unpack(truncate(tmp.respInfo));
	return r;
endfunction

function BusRequest socketReqToBusReq(Socket_Req tmp);
	BusRequest r = defaultValue;
	r.byteen = '1;
	r.address = tmp.reqAddr;
	r.data = tmp.reqData;
	r.write = (tmp.reqOp == WR);
	r.id = unpack(truncate(tmp.reqInfo));
	return r;
endfunction

function Socket_Req busReqToSocketReq(BusRequest x);
	 Socket_Req r = unpack(0);
	 r.reqOp = x.write ? WR : RD;
	 r.reqAddr = x.address;
	 r.reqData = x.data;
	 r.reqInfo = pack(extend(x.id));

	 return r;
 endfunction

 function Socket_Resp busRespToSocketResp(BusResponse x);
	 Socket_Resp r = unpack(0);
	 r.respOp = x.error ? NOP : OK;
	 r.respData = x.data;
	 r.reqOp = x.write ? WR : RD;
	 r.respInfo = pack(extend(x.id));
	 return r;
 endfunction

endpackage


			
