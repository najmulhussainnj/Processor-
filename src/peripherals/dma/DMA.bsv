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
//TODO 
//1. change CPAR, CMAR and CNDTR registers to conditionalWrite registers so that a write to these registers is possible only when the DMA channel is disabled.
//   DMA ISR register cannot be written by the software in any case. It can be reset by the software by writing into to DMA_IFCR.
//--DONE-- 2. Implement functionality of DMA_IFCR
//--DONE-- 3. Optimization. Remove the 1+ from 1 + (2*chanNum) for id because we are anyways checking for read and write responses in different FIFOs
//--DONE-- 4. What to do if the DMA channel needs to be disabled in between? How to clear the FIFOs, and what to do about the pending on going transaction?
//5. Opt. Try to remove currentReadRs and currentWriteRs and use !destAddrFs.notEmpty instead to detect transfer complete and generate exception. Is generateTransferDoneRules even needed then?
//6. Parameterize the number of channels and peripherals completely.
//7. Write an assertion if currentReadRs[chan]==currentWriteRs[chan] then destAddrFs and responseDataFs are both empty. Prove this formally?
//8. Implement burst mode readConfig and writeConfig registers
//9. While joining generateTransferDoneRules, try using rJoinConflictFree instead of rJoinDescendingUrgency and check if they are formally equivalent. The conflict is between finishWrite and this rule. But the condition currentReadRs==currentWriteRs will not be true untill finishWrite fires. So, these two rules will never actually fire together.

package DMA;

import FIFO :: * ;
import FIFOF :: * ;
import Vector :: * ;
import FShow::*;
import GetPut::*;
import DefaultValue ::*;
import AXI4_Types   :: *;
import AXI4_Fabric  :: *;
import Semi_FIFOF        :: *;
import ConcatReg :: *;
import ConfigReg :: *;

`define Burst_length_bits 8
`include "defined_parameters.bsv"
`define verbose
// ================================================================
// DMA requests and responses parameters


function Bit#(1) fn_aligned_addr(Bit#(3) addr, Bit#(3) awsize);
   bit out = 0;
    case(awsize)
        'd3: begin
            if(addr == 0) out = 1;
        end
        'd2: begin
            if(addr[1:0] == 0) out = 1;
        end
        'd1: begin
            if(addr[0] == 0) out = 1;
        end
        'd0: begin
            out = 1;
        end
        default: out = 0;
    endcase
    return out;
endfunction




typedef Bit#(`USERSPACE) Req_Info; 
typedef Bit#(`PADDR) Req_Addr; 
typedef Bit#(`Reg_width) Req_Data; 
//The Req and Resp are of the same width in the current design
//typedef Bit#(10) RespInfo;
//typedef Bit#(1) RespAddr;
//typedef Bit#(32) RespData;

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

// This function converts a Vector of 7 Registers to a single Register
function Reg#(Bit#(TMul#(7,q))) vector7ToRegN(Vector#(7,Reg#(Bit#(q))) inpV);
	return concatReg7(inpV[6], inpV[5], inpV[4], inpV[3], inpV[2], inpV[1], inpV[0]);
	//return asReg(zeroExtend(pack(inpV)));
endfunction

// ================================================================
// The DMA interface has two sub-interfaces
//  A AXI4 Slave interface for config
//  A AXI4 Master interface for data transfers

interface DmaC #(numeric type numChannels, numeric type numPeripherals);
	interface AXI4_Master_IFC#(`PADDR,`Reg_width,`USERSPACE) mmu;
	interface AXI4_Slave_IFC#(`PADDR,`Reg_width,`USERSPACE) cfg;
	method Action interrupt_from_peripherals(Bit#(numPeripherals) pint);
	method Bit#(numChannels) interrupt_to_processor();
endinterface


typedef UInt#(16)  DMACounts ;
// The number of channels is a parameter.
// typedef 2 NumChannels;


// Several configuration registers are included, and connected to the
// config socket/fifo.

// Along with the destination address, if we are writing into a peripheral, we need to pass the peripheral id 
// of the peripheral to check if the corresponding interrupt line is still high.
// Also, we need to send the destination transfer size for all the transactions.
typedef struct{
	Req_Addr addr;
	Bool is_dest_periph;
	Bit#(TLog#(numPeriphs)) periph_id;
} DestAddrFs_type#(numeric type numPeriphs) deriving (Bits,Eq);

typedef struct{
	Bit#(TLog#(numChannels)) chanNum;
	Bit#(4) id;
} Disable_channel_type#(numeric type numChannels) deriving (Bits, Eq);

instance DefaultValue#(Disable_channel_type#(numChannels));
	defaultValue= Disable_channel_type { chanNum: 'd-1,
										 id: 'd-1 };
endinstance

(* descending_urgency = "writeConfig, handle_interrupts" *)
(* descending_urgency = "writeConfig, rl_finishRead" *)
(* descending_urgency = "writeConfig, rl_startWrite" *)
module mkDMA( DmaC #(numChannels, numPeripherals) )
	provisos ( Add#(numChannels, 0, 7),
			   Add#(a__, TLog#(numPeripherals), 4)); 

	// The DMA contains one master interface, and one slave interface. The processor sends
	// request through the slave interface to set the config registers.
	// The DMA's master initiates a request to one of the peripherals through one of the
	// channels. The response is taken (through response sub-interface of DMA's Master interface
	// and returned to the processor (through response sub-interface of the DMA's Slave interface 
	AXI4_Slave_Xactor_IFC #(`PADDR,`Reg_width,`USERSPACE) s_xactor <- mkAXI4_Slave_Xactor;
	AXI4_Master_Xactor_IFC #(`PADDR,`Reg_width,`USERSPACE) m_xactor <- mkAXI4_Master_Xactor;


	////////////////////////////////////////////////////////////////
	//////////////////////// DMA Registers /////////////////////////
	////////////////////////////////////////////////////////////////
	Vector#(numChannels,Reg#(Bit#(4))) dma_isr <- replicateM(mkReg(0));		//Interrupt Status Register
	Vector#(numChannels,Reg#(Bit#(4))) dma_ifcr <- replicateM(mkReg(0));	//Interrupt Flag Clear Register
	Vector#(numChannels,Reg#(Bit#(32))) dma_ccr <- replicateM(mkConfigReg(0));	//Channel Configuration Register
	Vector#(numChannels,Reg#(Bit#(16))) dma_cndtr <- replicateM(mkConfigReg(0));	//Channel Number of Data Transfer Register
	Vector#(numChannels,Reg#(Req_Addr)) dma_cpar <- replicateM(mkReg(0));	//Channel Peripheral Address Register
	Vector#(numChannels,Reg#(Req_Addr)) dma_cmar <- replicateM(mkReg(0));	//Channel Memory Address Register
	Vector#(numChannels,Reg#(Bit#(4))) dma1_cselr <- replicateM(mkReg(0));	//Channel SELection Register
	//We do not have dma2_cselr because there is only one DMA, and not 2 in this architecture

	//Registers to keep track if all the data read is wrtten
	//Vector#(numChannels, Array#(Reg#(DMACounts))) currentReadRs[2]  <- replicateM(mkCReg(2,0));
	//Vector#(numChannels, Array#(Reg#(DMACounts))) currentWriteRs[2] <- replicateM(mkCReg(2,0));
	Reg#(DMACounts) currentReadRs[valueOf(numChannels)][2];
	Reg#(DMACounts) currentWriteRs[valueOf(numChannels)][2];
	Reg#(Bool)		rg_is_cndtr_zero[valueOf(numChannels)][2];
    Reg#(Bit#(8)) rg_write_strobe <- mkReg(0);
    Reg#(Bit#(2)) rg_tsize <- mkReg(0);
	for(Integer i=0 ; i<valueOf(numChannels) ; i=i+1) begin
		currentReadRs[i] <- mkCReg(2,0);
		currentWriteRs[i] <- mkCReg(2,0);
		rg_is_cndtr_zero[i] <- mkCReg(2,True);
	end

	// Use a FIFO to pass the read response to the write "side",
	//  thus allowing pending transations and concurrency.
	// FIFOs can be replicated as well.
	Vector#(numChannels,FIFOF#(Req_Data))  
		  responseDataFs <- replicateM(mkSizedFIFOF(2)) ;  

	//Wire to pass the interrupt from peripheral to DMA
	Wire#(Vector#(numPeripherals,Bit#(1))) wr_peripheral_interrupt <- mkDWire(replicate(0));

	//Wire to set the TEIF
	Wire#(Maybe#(Bit#(4))) wr_bus_err <- mkDWire(tagged Invalid);

	// We also want to pass the destination address for each read over
	// to the write "side", along with some other metadata.
	// The depth of this fifo limits the number of outstanding reads
	// which may be pending before the write.  The maximum outstanding
	// reads depends on the overall latency of the read requests.
	Vector#(numChannels,FIFOF#(DestAddrFs_type#(numPeripherals))) 
		destAddrFs <- replicateM( mkSizedFIFOF(2)) ;
	
	// This register stores the initial value of the CNDTR. It is used to restore the value back
	// when operating in circular mode.
	Vector#(numChannels,Reg#(Bit#(16))) rg_cndtr <- replicateM(mkReg(0));

	// The spec specifies that the CPAR and CMAR values when read in middle of a transaction should
	// still hold the original programmed value, and not the address of the current transaction.
	// Therefore, we have a copy of these registers which indicate the address of the current
	// ongoing transaction on that channel.
	Vector#(numChannels,Reg#(Req_Addr)) rg_cpa <- replicateM(mkConfigReg(0));	// Local Channel Peripheral Address Register
	Vector#(numChannels,Reg#(Req_Addr)) rg_cma <- replicateM(mkConfigReg(0));	// Local Channel Memory Address Register

	Reg#(Bit#(`Burst_length_bits)) rg_burst_count <- mkReg(0);
	Reg#(Bit#(TLog#(numChannels))) rg_current_trans_chan_id <- mkReg(0);
	Reg#(Tuple3#(Bool, Bit#(TLog#(numChannels)), Bit#(4))) rg_disable_channel <- mkReg(tuple3(False, ?, 'd-1));
	Reg#(Tuple2#(Bit#(TLog#(numChannels)), Bit#(32))) rg_writeConfig_ccr <- mkReg(tuple2(0,0));
	Reg#(Bool) rg_finish_write[valueOf(numChannels)][2];
	Reg#(Bool) rg_finish_read[valueOf(numChannels)][2];
	for(Integer i=0 ; i<valueOf(numChannels) ; i=i+1) begin
		rg_finish_write[i]<- mkCReg(2,True);
		rg_finish_read[i]<- mkCReg(2,True);
	end

	// This function returns the id of the peripheral for which this channel is configured
	function Bit#(TLog#(numPeripherals)) fn_map_channel_to_periph(Integer chanNum);
		return truncate(dma1_cselr[chanNum]);
	endfunction

	// This function tells the maximum priority number amongst all the active peripherals
	// which want to initiate a transaction. If one of the peripherals want to initiate a transaction
	// i.e. it's interrupt line in high, we check if any other peripheral, whose interrupt line is high,
	// has a higher priority (bits 13:12 in dma_ccrX). If so, we set the max_priority_num to
	// the value of the highest interrupt.
	// Note that even after this there might be multiple peripheral which have the same priority level as
	// max_priority_num. Amongst them, the one with the lower channel number is given priority.
	function Tuple2#(Bit#(TLog#(numChannels)),Bit#(TLog#(numPeripherals))) fn_dma_priority_encoder();
		Bit#(2) max_priority_num= 0;
		//stores id of the periph whose channel has been granted to generate req in this cycle.
		Bit#(TLog#(numChannels)) grant_chan_id= 'd-1;
		Bit#(TLog#(numPeripherals)) grant_periph_id= 'd-1;
		for(Integer i=valueOf(numChannels)-1; i>=0; i=i-1) begin			//for every channel
			let periph_connected_to_channel_i=fn_map_channel_to_periph(i);	//identify the peripheral connected to this channel
			if(dma_ccr[i][0]==1 && dma_isr[i][1]==0 &&                                          //if the DMA channel is enabled
			(wr_peripheral_interrupt[periph_connected_to_channel_i]==1 	//if the peripheral has raised an interrupt
			|| dma_ccr[i][14]==1											//if M2M transfer, need not check for interrupt
			//|| (dma_ccr[4]==1 && !rg_burst)	//if not burst mode, the interrupt of periph needn't be high when reading from memory
				  								//if in burst mode, its better if the peripheral is ready, and then we fetch the word from memory? TODO
			)) begin 
				//check if its priority is greater than any of the other peripherals' priority
				//here we check for equality also as the channels with lower number have higher priority.
				//therefore if two requests have same "high" software priority, the channel whose channel number
				//is lower will be chosen
				if(dma_ccr[i][13:12]>=max_priority_num) begin
					max_priority_num= dma_ccr[i][13:12];	//if so, then set the current priority level to be that of peripheral[i]
					grant_chan_id= fromInteger(i);
					grant_periph_id= periph_connected_to_channel_i;
				end
			end
		end
		return tuple2(grant_chan_id,grant_periph_id);
	endfunction

	function Bit#(16) fn_decr_cndtr(Bit#(16) cndtr, Bit#(2) tsize, Bit#(`Burst_length_bits) bsize);
		Bit#(17) lv_to_sub= (zeroExtend(bsize)+1) << tsize;
		Bit#(17) lv_result= {1'b0,cndtr}-lv_to_sub;
		if(lv_result[16]==1)    //underflow. Can happen in burst mode when the bytes to be transferred is not an exact multiple of the burst length x burst size.
			return 0;
        else
		    return lv_result[15:0];
    endfunction

	// This function increments the source or destination address depending on
	// the size of the transfer.
	// Note that though STM's DMA defines tsize=2'b11 as reserved, we use it to
	// perform a 64-bit data transfer.
	function Req_Addr fn_incr_address(Req_Addr addr, Bit#(2) tsize, Bit#(`Burst_length_bits) bsize) provisos(Bits#(Req_Addr,sz_Req_Addr), Add#(sz_Req_Addr,1, a));
		Bit#(a) lv_to_add= (zeroExtend(bsize)+1) << tsize;
		Bit#(a) lv_result= {1'b0,addr}+lv_to_add;
		return truncate(lv_result);
	endfunction

	//This function performs the data alignment for 64 bit data
	function Bit#(64) fn_data_alignment64 (Bit#(64) data, Bit#(2) source_sz, Bit#(2) dest_sz);
		Bit#(64) outp;
		case (source_sz) matches
			2'd0: begin 
					outp= zeroExtend(data[7:0]);
				  end
			2'd1: begin
					if(dest_sz==2'd0)
						outp= zeroExtend(data[7:0]);
					else
						outp= zeroExtend(data[15:0]);
				  end
			2'd2: begin
					if(dest_sz==2'd0)
						outp= zeroExtend(data[7:0]);
					else if(dest_sz==2'd1)
						outp= zeroExtend(data[15:0]);
					else 
						outp= zeroExtend(data[31:0]);
				  end
			2'd3: begin
					if(dest_sz==2'd0)
						outp= zeroExtend(data[7:0]);
					else if(dest_sz==2'd1)
						outp= zeroExtend(data[15:0]);
					else if(dest_sz== 2'd2)
						outp= zeroExtend(data[31:0]);
					else
						outp= data;
				  end
		endcase
		return outp;
	endfunction

	// DMA rules //////////////////////////////////////////////////
	// We define a function inside the module so it can access some
	// of the registers without passing too many arguments.  
	// The function takes as arguments the conditions and fifos
	// (interfaces)
	// And returns a set a rules.
	// The rule are identical to the set used in the one mmu port case.
	function Rules generatePortDMARules (AXI4_Master_Xactor_IFC#(`PADDR,`Reg_width,`USERSPACE) xactor, Integer chanNum);
		return
		rules

	    /*rule display_stat(dma_ccr[chanNum][0]==1 || tpl_1(rg_disable_channel));
			Bit#(2) max_priority_num= 0;
			//stores id of the periph whose channel has been granted to generate req in this cycle.
			Bit#(TLog#(numChannels)) grant_chan_id= 'd-1;
			Bit#(TLog#(numPeripherals)) grant_periph_id= 'd-1;
			for(Integer i=valueOf(numChannels)-1; i>=0; i=i-1) begin			//for every channel
				let periph_connected_to_channel_i=fn_map_channel_to_periph(i);	//identify the peripheral connected to this channel
				//$display("\nChan %d interrupt: %b cndtr: 'h%h prio: %d", i, dma_isr[i][1], wr_peripheral_interrupt[periph_connected_to_channel_i], dma_cndtr[chanNum],tpl_1(fn_dma_priority_encoder()));
				if(dma_ccr[i][0]==1 && dma_isr[i][1]==0 &&                                          //if the DMA channel is enabled
				  (wr_peripheral_interrupt[periph_connected_to_channel_i]==1 	//if the peripheral has raised an interrupt
				  || dma_ccr[i][14]==1											//if M2M transfer, need not check for interrupt
				  //|| (dma_ccr[4]==1 && !rg_burst)
				  )) begin //if not burst mode, the interrupt of periph needn't be high when reading from memory
					//check if its priority is greater than any of the other peripherals' priority
					//here we check for equality also as the channels with lower number have higher priority.
					//therefore if two requests have same "high" software priority, the channel whose channel number
					//is lower will be chosen
					if(dma_ccr[i][13:12]>=max_priority_num) begin
						max_priority_num= dma_ccr[i][13:12];	//if so, then set the current priority level to be that of peripheral[i]
						grant_chan_id= fromInteger(i);
						grant_periph_id= periph_connected_to_channel_i;
						//$display("########## grant chan: %d grant periph_id: %d", grant_chan_id, grant_periph_id);
					end
				//$display("dma_ccr[%d][13:12]: %b max_prio: %b",i,dma_ccr[i][13:12],max_priority_num);
				end
			end
			if(pack(wr_peripheral_interrupt)!=0 || dma_ccr[chanNum][14]==1) begin
				Reg#(Bit#(64)) lv_dma_ifcr= regAToRegBitN( vector7ToRegN( dma_ifcr ));
				$display($time,"Chan%d: cndtr: %h dma_isr: %h dma_ifcr: %h", chanNum, dma_cndtr[chanNum], dma_isr[chanNum], lv_dma_ifcr );
			end
			//$display($time,"grant chan_id: %d periph_id: %d",grant_chan_id,grant_periph_id);
			//$display("Channel no. %d user_id: %d", chanNum, xactor.o_rd_data.first.ruser); 
        endrule*/

		// To start a read, following conditions need to be met
		// 1. There is data to be transferred
		// 2. The dma is enabled (dma_ccr[chanNum][0]=1)
		// 3. The channel has the highest priority
		// 4. If multiple channels have same high priority, choose the one whose channel number is lowest
		// 5. A channel is disabled before the complete transfer is finished, do not initiate any more requests
		// The 2nd, 3rd and 4th conditions are handled by fn_dma_priority_encoder
		(* descending_urgency =  "rl_startWrite, rl_startRead" *)
		//(* preempts = "rl_finishRead, rl_can_change_channel" *)
		//(* preempts = "rl_startWrite, rl_can_change_channel" *)
		//(* preempts = "rl_send_burst_write_data, rl_can_change_channel" *)
		rule rl_startRead	( !rg_is_cndtr_zero[chanNum][0] &&	//no of bytes remaining to transfer is not 0
						  	  fromInteger(chanNum) == tpl_1(fn_dma_priority_encoder()) &&  //if the this channel has the highest priority
							  (!tpl_1(rg_disable_channel) ||  tpl_2(rg_disable_channel)!=fromInteger(chanNum))
						  		&& rg_finish_read[chanNum][1]);	//if the channel is not being disabled by the processor
			Req_Addr lv_araddr;
			Bit#(2) lv_arsize;
			bit lv_burst_type;
			let lv_dma_ccr= dma_ccr[chanNum];
			Bit#(`Burst_length_bits) lv_burst= lv_dma_ccr[`Burst_length_bits+15:16]; //Upper bits of CCR supports configurable bursts !! Added, not part of STMicro
			let lv_periph_id= tpl_2(fn_dma_priority_encoder());

			if(lv_dma_ccr[6]==1)	//peripheral increment mode is on
				rg_cpa[chanNum]<= fn_incr_address(rg_cpa[chanNum], lv_dma_ccr[9:8], dma_ccr[chanNum][`Burst_length_bits+15:16]);
			if(lv_dma_ccr[7]==1)	//memory increment mode is on
				rg_cma[chanNum]<= fn_incr_address(rg_cma[chanNum], lv_dma_ccr[11:10], dma_ccr[chanNum][`Burst_length_bits+15:16]);

			if(lv_dma_ccr[4]==0) begin		//read from peripheral
				lv_araddr= rg_cpa[chanNum];	//set the address to read from
				lv_arsize= lv_dma_ccr[9:8];	//set the transfer size
				lv_burst_type= lv_dma_ccr[6];	//0: Fixed, 1: INCR which is consistent with that of AXI4
				`ifdef verbose $display($time,"\tDMA: chan[%0d] starting read from peripheral address %h",chanNum, lv_araddr); `endif
				// Since the destination is memory, the write request needn't wait for any interrupt line to be high
				// Therefore, we send the first argument as Invalid
				destAddrFs[chanNum].enq( DestAddrFs_type {	addr: rg_cma[chanNum], // Enqueue the Write destination address
															is_dest_periph: False,
															periph_id: lv_periph_id});
			end
			else begin							//read from memory
				lv_araddr= rg_cma[chanNum];		//set the address to read from
				lv_arsize= lv_dma_ccr[11:10];	//set the transfer size
				lv_burst_type= lv_dma_ccr[7];	//0: Fixed, 1: INCR which is consistent with that of AXI4
				`ifdef verbose $display($time,"\tDMA: chan[%0d] starting read from memory address %h",chanNum, lv_araddr); `endif
				// Since the destination address is that of a peripheral, the write request can be issued only when
				// the corresponding peripheral's interrupt line is high. Therefore, we send the periph_id too.
				Bool lv_is_dest_periph;
				if(lv_dma_ccr[14]==0)
					lv_is_dest_periph= True;
				else
					lv_is_dest_periph= False;
                    `ifdef verbose $display("dest_is_periph: %h",lv_is_dest_periph); `endif
				destAddrFs[chanNum].enq( DestAddrFs_type { 	addr: rg_cpa[chanNum], // Enqueue the Write destination address
															is_dest_periph: lv_is_dest_periph,
															periph_id: lv_periph_id});

			end

			// Create a read request, and enqueue it
			// Since there can be multiple pending requests, either read or
			// writes, we use the `Req_Info field to mark these.
			let read_request = AXI4_Rd_Addr {araddr: lv_araddr, arprot: 0,
											 arid: {1'b1,fromInteger(chanNum)}, arlen: lv_burst,
											 arsize: zeroExtend(lv_arsize), arburst: zeroExtend(lv_burst_type), //arburst: 00-FIXED 01-INCR 10-WRAP
											 arlock: 0, arcache: 0, arqos: 0,
											 arregion: 0, aruser: 0 };
				
			xactor.i_rd_addr.enq(read_request);
            `ifdef verbose $display("Sending a read request with araddr: %h arid: %h arlen: %h arsize: %h arburst: %h",lv_araddr,fromInteger(chanNum),lv_burst,lv_arsize,lv_burst_type); `endif

			//housekeeping. To be done when the transaction is complete.
			currentReadRs[chanNum][0]<= currentReadRs[chanNum][0] + 1;
            dma_cndtr[chanNum]<= fn_decr_cndtr(dma_cndtr[chanNum], lv_arsize, dma_ccr[chanNum][`Burst_length_bits+15:16]);
            rg_current_trans_chan_id<= fromInteger(chanNum);
			rg_finish_read[chanNum][1]<= False;
		endrule

		// We finish the read when we see the correct respInfo on the mmu response fifo
		rule rl_finishRead (xactor.o_rd_data.first.rid == {1'b1,fromInteger(chanNum)} && xactor.o_rd_data.first.rresp==AXI4_OKAY);
			// update cndtr register to keep track of remaining transactions
			let lv_dma_ccr= dma_ccr[chanNum];
			Bit#(2) lv_tsize;
			Bit#(2) lv_source_size;

			if(dma_ccr[chanNum][4]==0) begin		//if the source is peripheral
				lv_tsize= dma_ccr[chanNum][11:10];	//destination's tsize will be that of memory
				lv_source_size= dma_ccr[chanNum][9:8];
			end
			else begin
				lv_tsize= dma_ccr[chanNum][9:8];	//destination's tsize will be that of peripheral
				lv_source_size= dma_ccr[chanNum][11:10];
			end

			// grab the data from the mmu reponse fifo
			let resp <- pop_o(xactor.o_rd_data);
			`ifdef verbose $display("DMA: chan[%d] finish read. Got data: %h",chanNum, resp.rdata); `endif

			// Pass the read data to the write "side" of the dma
			responseDataFs[chanNum].enq( resp.rdata );
			rg_finish_read[chanNum][0]<= True;
		endrule

		//rule to handle circ mode
		rule rl_handle_circ_mode(dma_ccr[chanNum][5]==1 && rg_is_cndtr_zero[chanNum][1]); //if circular mode is enabled
			dma_cndtr[chanNum]<= rg_cndtr[chanNum];
		endrule

		// This rule start the write process
		// Note that this rule conflicts with rule startRead, so we make
		// this rule be more urgent. i.e., finish writing before you start
		// reading more.
		/*rule rl_startWrite111;
			$display("helloo.. intrpt: %b burst_count: %h finish_write: %b", wr_peripheral_interrupt[ destAddrFs[chanNum].first.periph_id ], rg_burst_count, rg_finish_write[chanNum][1]);
		endrule*/

		rule rl_startWrite( (destAddrFs[chanNum].first.is_dest_periph==False		//if the dest is memory
		|| wr_peripheral_interrupt[ destAddrFs[chanNum].first.periph_id ]==1)	//if dest is not memory, then check if the peripheral's interrupt line is active
		&& rg_burst_count==0 && rg_finish_write[chanNum][1]==True);
			let lv_data= destAddrFs[chanNum].first;

			Bit#(2) lv_tsize;
			bit lv_burst_type;
			let lv_dma_ccr= dma_ccr[chanNum];
			if(lv_dma_ccr[4]==0) begin			//if the source is peripheral
				lv_tsize= lv_dma_ccr[11:10];	//destination's tsize will be that of memory
				lv_burst_type= lv_dma_ccr[7];	//destination's burst type will be that of memory
			end
			else begin
				lv_tsize= lv_dma_ccr[9:8];		//destination's tsize will be that of peripheral
				lv_burst_type= lv_dma_ccr[6];	//destination's burst type will be that of peripheral
			end

			Bit#(`Reg_width) actual_data= responseDataFs[chanNum].first;
			Bit#(`Burst_length_bits) lv_burst_len= lv_dma_ccr[`Burst_length_bits+15:16];
		//	Bit#(6) x = {3'b0,lv_data.addr[2:0]}<<3;
			Bit#(8) write_strobe=lv_tsize==0?8'b1:lv_tsize==1?8'b11:lv_tsize==2?8'hf:8'hff;
			if(lv_tsize!=3)begin			// 8-bit write;
				//actual_data=actual_data<<(x);
				write_strobe=write_strobe<<(lv_data.addr[`byte_offset:0]);
			end
			//lv_data.addr[2:0]=0; // also make the address 64-bit aligned
            `ifdef verbose $display("Start Write"); `endif

			
			Bool lv_last= True;
			if(lv_burst_len>0) begin // only enable the next rule when doing a write in burst mode.
				rg_burst_count<=rg_burst_count+1;
				lv_last= False;
				`ifdef verbose $display("Starting burst mode write...."); `endif
			end
			`ifdef verbose
			else begin
				$display("Performing a single write...");
			end 
			`endif

            rg_write_strobe <= write_strobe; //Write strobe needs to be rotated so that burst writes are sent correctly, storing write_strobe in a register. ~Vinod
            rg_tsize <= lv_tsize; //Storing rg_tsize in a register. ~Vinod
			// Generate a Write 
			let write_data = AXI4_Wr_Data { wdata: actual_data , wstrb: write_strobe, wlast: lv_last, wid: {1'b1,fromInteger(chanNum)}};
			let write_addr = AXI4_Wr_Addr {	awaddr: lv_data.addr, awprot:0, awuser: 0,
											awlen: lv_burst_len, awsize: zeroExtend(lv_tsize), awburst: zeroExtend(lv_burst_type),
											awlock: 0, awcache: 0, awqos: 0, awregion: 0, awid: {1'b1,fromInteger(chanNum)} };

			// enqueue the request.
			xactor.i_wr_data.enq(write_data);
			xactor.i_wr_addr.enq(write_addr);
			rg_finish_write[chanNum][1]<= False;

			// Some other house keeping - removing the data from the fifos
			responseDataFs[chanNum].deq;
			destAddrFs[chanNum].deq;	//dequeing this FIFO will cause startRead to fire.
			`ifdef verbose $display ($time,"\tDMA[%0d] startWrite addr: %h data: %h", chanNum,lv_data.addr,responseDataFs[chanNum].first); `endif
		endrule

		//This rule is used to send burst write data. The explicit condition ensures that we
		//send burst length number of data i.e. rg_burst_count>1. When rg_burst_count = the burst
		//length specified in dma_ccr, rg_burst_count becomes 0. Since rg_burst_count!=0 infers
		//lesser hardware compared to rg_burst_count>1, we write that as the explicit condition.
		rule rl_send_burst_write_data(rg_burst_count!=0);// && dma_ccr[chanNum][`Burst_length_bits+15:16]!='d0);
			Bool lv_last= rg_burst_count==dma_ccr[chanNum][`Burst_length_bits+15:16];
			/*==  Since this is going to always be a line write request in burst mode No need of shifting data and address=== */
            let write_strobe = rotateBitsBy(rg_write_strobe,1<<rg_tsize); //~Vinod Rotating write_strobe by awsize
			let w  = AXI4_Wr_Data {wdata:  responseDataFs[chanNum].first, wstrb: write_strobe , wlast: lv_last, wid: {1'b1, fromInteger(chanNum)} };
      		xactor.i_wr_data.enq(w);
			`ifdef verbose $display ($time,"\tDMA[%0d] startWrite Burst data: %h rg_burst_count: %d dma_ccr[23:16]: %d", chanNum,responseDataFs[chanNum].first,  rg_burst_count, dma_ccr[chanNum][23:16]); `endif
			if(lv_last)begin
				`ifdef verbose $display("Last data received..."); `endif
				rg_burst_count<=0;
			end
			else begin
				rg_burst_count<=rg_burst_count+1;
			end
			responseDataFs[chanNum].deq;
            rg_write_strobe <= write_strobe;
		endrule


		// This rule waits for the write to finish
		rule rl_finishWrite( (xactor.o_wr_resp.first.bid == {1'b1, fromInteger(chanNum)}) &&
		(xactor.o_wr_resp.first.bresp==AXI4_OKAY) );
			let x<- pop_o(xactor.o_wr_resp) ;			 // take the response data and finish
			currentWriteRs[chanNum][0]<= currentWriteRs[chanNum][0] + 1;
			`ifdef verbose $display ("DMA[%0d]: finishWrite", chanNum); `endif
			rg_finish_write[chanNum][0]<= True;
		endrule
		
		rule rl_cndtr_is_zero;
			rg_is_cndtr_zero[chanNum][0]<= (dma_cndtr[chanNum]==0);
		endrule

	endrules;
	endfunction

	function Rules generateTransferDoneRules( Integer chanNum );
		return
		rules
			// Conditions to mark when transfer is done.
			// This rule will not fire for when circular mode because dma_cndtr[chanNum] is updated 
			// in the next cycle, which is before currentWriteRs can possibly get updated.
			rule markTransferDone (	dma_ccr[chanNum][0]==1 &&	//if the channel is enabled
									rg_is_cndtr_zero[chanNum][0] &&	//if the remaining data to transfer is 0
									currentWriteRs[chanNum][0]== currentReadRs[chanNum][0]) ;	//the final write has finished
				//dmaEnabledRs[chanNum]._write (False) ; 
				currentWriteRs[chanNum][0] <= 0 ;
				currentReadRs[chanNum][0]  <= 0 ;
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

	//
	for (Integer ch = 0; ch < valueof (numChannels); ch = ch + 1)
		ruleset = rJoinDescendingUrgency (ruleset,
					generateTransferDoneRules(ch));



	(* descending_urgency = "rl_write_response_error, rl_read_response_error" *)
	rule rl_write_response_error(m_xactor.o_wr_resp.first.bresp == AXI4_SLVERR || m_xactor.o_wr_resp.first.bresp == AXI4_DECERR);
		wr_bus_err<= tagged Valid m_xactor.o_wr_resp.first.bid;
	endrule

	rule rl_read_response_error(m_xactor.o_rd_data.first.rresp == AXI4_SLVERR || m_xactor.o_rd_data.first.rresp == AXI4_DECERR);
		wr_bus_err<= tagged Valid m_xactor.o_rd_data.first.rid;
	endrule

	rule handle_interrupts;	//interrupts will be raised only when channel is enabled
		for (Integer chanNum = 0; chanNum < valueOf (numChannels); chanNum = chanNum + 1) begin
			Bit#(4) chan_isr= 'd0;	//TEIF, HTIF, TCIF, GIF
			Bit#(32) lv_dma_ccr= dma_ccr[chanNum];
			//$display("*** chan: %d en: %d dma_cndtr: %h rg_cndtr: %h",chanNum, dma_ccr[chanNum][0], dma_cndtr[chanNum], rg_cndtr[chanNum]);
			if(wr_bus_err matches tagged Valid .chan_num &&& fromInteger(chanNum)=={1'b1, chan_num[2:0]}) begin
				chan_isr[3]=1;
				$display("Bus error on channel %d",chanNum);
			end
			if(lv_dma_ccr[0]==1) begin
				if(currentWriteRs[chanNum][1]==currentReadRs[chanNum][1]) begin	//once the read and write transactions are over
					if(rg_is_cndtr_zero[chanNum][0]) //TODO check what happens if you read from port 1 here
						chan_isr[1]= 1;
				end
				//The Half Transfer Complete will be set when half transfer is complete.
				//Since dma_cndtr is modified by startRead rule, though it would've become half,
				//the transaction wouldn't be complete till the write is over. Also, by the time write is over, 
				//another read request could've been issued. Therefore, we have the below condition.
				if(dma_cndtr[chanNum]<=(rg_cndtr[chanNum]>>1) && rg_finish_write[chanNum][1]) begin
					chan_isr[2]= 1;
				end
			end
			chan_isr[0]= chan_isr[3] | chan_isr[2] | chan_isr[1];	//Setting the GIF
			chan_isr= dma_isr[chanNum][3:0] | chan_isr; //Sticky nature of interrupts. Should be cleared by software using ifcr.
			
			//The bits in IFCR represent the interrupts that need to be cleared
			chan_isr= chan_isr & ~(dma_ifcr[chanNum]);
			dma_isr[chanNum]<= chan_isr;
//            $display("chan_isr %b dma_cndtr[%d] :%h",chan_isr,chanNum,dma_cndtr[chanNum]);
		end
	endrule


	// Rules and other code to interface config port /////////////

	// Add a zero-size register as a default for invalid addresses
	Reg#(Bit#(0)) nullReg <- mkReg( ? ) ;

	// For ease of development we want all registers to look like 64
	// bit resister-- the data size of the config socket.
	// Create function to map from address to specific registers
	function Tuple2#(Reg#(Req_Data), Bool) selectReg( Req_Addr addr );
        Bit#(8) taddr = truncate( addr ) ;
        return
        case ( taddr )
            8'h00 : return tuple2(regAToRegBitN( vector7ToRegN( dma_isr )), True);
            8'h04 : return tuple2(regAToRegBitN( vector7ToRegN( dma_ifcr )), True);
            8'hB0 : return tuple2(regAToRegBitN( vector7ToRegN( dma1_cselr )), True);
 
            8'h08 : return tuple2(regAToRegBitN( dma_ccr[0] ), True);  //32-bit
            8'h0c : return tuple2(regAToRegBitN( dma_cndtr[0] ), True); //16-bit -- 32-bit Addr 
            8'h10 : return tuple2(regAToRegBitN( dma_cpar[0] ), True); //64-bit
            8'h18 : return tuple2(regAToRegBitN( dma_cmar[0] ), True); //64-bit
 
            8'h20 : return tuple2(regAToRegBitN( dma_ccr[1] ), True);
            8'h24 : return tuple2(regAToRegBitN( dma_cndtr[1] ), True);
            8'h28 : return tuple2(regAToRegBitN( dma_cpar[1] ), True);
            8'h30 : return tuple2(regAToRegBitN( dma_cmar[1] ), True);
 
            8'h38 : return tuple2(regAToRegBitN( dma_ccr[2] ), True);
            8'h3C : return tuple2(regAToRegBitN( dma_cndtr[2] ), True);
            8'h40 : return tuple2(regAToRegBitN( dma_cpar[2] ), True);
            8'h48 : return tuple2(regAToRegBitN( dma_cmar[2] ), True);
 
            8'h50 : return tuple2(regAToRegBitN( dma_ccr[3] ), True);
            8'h54 : return tuple2(regAToRegBitN( dma_cndtr[3] ), True);
            8'h58 : return tuple2(regAToRegBitN( dma_cpar[3] ), True);
            8'h60 : return tuple2(regAToRegBitN( dma_cmar[3] ), True);
 
            8'h68 : return tuple2(regAToRegBitN( dma_ccr[4] ), True);
            8'h6C : return tuple2(regAToRegBitN( dma_cndtr[4] ), True);
            8'h70 : return tuple2(regAToRegBitN( dma_cpar[4] ), True);
            8'h78 : return tuple2(regAToRegBitN( dma_cmar[4] ), True);
 
            8'h80 : return tuple2(regAToRegBitN( dma_ccr[5] ), True);
            8'h84 : return tuple2(regAToRegBitN( dma_cndtr[5] ), True);
            8'h88 : return tuple2(regAToRegBitN( dma_cpar[5] ), True);
            8'h90 : return tuple2(regAToRegBitN( dma_cmar[5] ), True);
 
            8'h98 : return tuple2(regAToRegBitN( dma_ccr[6] ), True);
            8'h9C : return tuple2(regAToRegBitN( dma_cndtr[6] ), True);
            8'hA0 : return tuple2(regAToRegBitN( dma_cpar[6] ), True);
            8'hA8 : return tuple2(regAToRegBitN( dma_cmar[6] ), True);
 
            default: return tuple2(regAToRegBitN( nullReg ), False);
        endcase ;
    endfunction

	function Bit#(3) ccr_channel_number (Req_Addr addr);
        Bit#(8) taddr= truncate(addr);
        return
        case ( taddr )
            8'h08 : return 0;
            8'h20 : return 1;
            8'h38 : return 2;
            8'h50 : return 3;
            8'h68 : return 4;
            8'h80 : return 5;
            8'h98 : return 6;
            default: 'd7;
        endcase;
    endfunction
	
	Rules writeConfig = (rules
		rule writeConfig;
			let write_addr <- pop_o(s_xactor.o_wr_addr);
			let write_data <- pop_o(s_xactor.o_wr_data);
			Req_Data lv_data= 0;
			Req_Addr lv_addr= 0;
			AXI4_Resp lv_bresp= AXI4_OKAY;
			Bool lv_send_response= True;

			if(write_data.wstrb=='hF0) begin
				lv_data= zeroExtend(write_data.wdata[63:32]);
				lv_addr= {truncateLSB(write_addr.awaddr),3'b100};
			end
			else if(write_data.wstrb=='h0F) begin
				lv_data= zeroExtend(write_data.wdata[31:0]);
				lv_addr= {truncateLSB(write_addr.awaddr),3'b000};
			end
			else if(write_data.wstrb=='hFF) begin
				lv_data= write_data.wdata;
				lv_addr= write_addr.awaddr;
			end
			else begin	//The write request is not 64-bits, and therefore return a bus error
				lv_bresp= AXI4_SLVERR;
				$display($time,"\tDMA: KAT GAYA");
			end

				`ifdef verbose $display ($time,"\tDMA writeConfig addr: %0h data: %0h", lv_addr, lv_data); `endif
			// Select and write the register
			let lv1= selectReg(lv_addr);
			let thisReg = tpl_1(lv1);
			if(!tpl_2(lv1)) begin	//if no register mapping exists for the given address
				lv_bresp= AXI4_SLVERR;
				$display($time,"\tDMA: Wapas KAT GAYA");
			end
			//else is not needed as the selectReg function handles it

			let lv_ccr_channel_number= ccr_channel_number(lv_addr);
            `ifdef verbose $display("ccr_channel_number %h lv_addr %h",lv_ccr_channel_number, lv_addr); `endif
			if( lv_ccr_channel_number!='d-1 && tpl_2(lv1)) begin 	//if the current write is happening to one of the channel's CCR.
				if(lv_data[0]==1 ) begin			//if the channel is being enabled
					rg_cpa[lv_ccr_channel_number] <= dma_cpar[lv_ccr_channel_number];	//peripheral address is copied
					rg_cma[lv_ccr_channel_number] <= dma_cmar[lv_ccr_channel_number];	//memory address is copied
					rg_cndtr[lv_ccr_channel_number]<= dma_cndtr[lv_ccr_channel_number];	//the cndtr value is saved
					rg_disable_channel<= tuple3(False,?,?);
					$display("----------------------- ENABLING DMA CHANNEL %d", lv_ccr_channel_number," -----------------------");
                    
                    Bit#(3) cmar_align = dma_cmar[lv_ccr_channel_number][2:0]; //Vinod
                    Bit#(3) cpar_align = dma_cpar[lv_ccr_channel_number][2:0]; //Vinod
                    
                    //lv_data[9:8] and lv_data[11:10] gives transfer size supposedly. Using K-Maps --Possibility of a bug?
                bit cmar_is_aligned =  fn_aligned_addr(write_addr.awaddr[2:0], write_addr.awsize);
                bit cpar_is_aligned =  fn_aligned_addr(write_addr.awaddr[2:0], write_addr.awsize); 

                if((cmar_is_aligned&cpar_is_aligned)==0) begin
                        lv_bresp = AXI4_DECERR; //DECERR for Unaligned addresses
                        $display("\tAXI4_DECERR\n");
                end

                    $display("cmar_is_aligned: %b cpar_is_aligned: %b",cmar_is_aligned,cpar_is_aligned);
					
                    
                    if(lv_data[4]==0) begin
						$display("SOURCE: Peripheral  Addr: 'h%0h Transfer size: 'b%b Incr: %b",dma_cpar[lv_ccr_channel_number], lv_data[9:8], lv_data[6]);
						$display("DEST  : Memory      Addr: 'h%0h Transfer size: 'b%b Incr: %b",dma_cmar[lv_ccr_channel_number], lv_data[11:10], lv_data[7]);
					end
					else if(lv_data[14]==0) begin
						$display("SOURCE: Memory      Addr: 'h%0h Transfer size: 'b%b Incr: %b",dma_cmar[lv_ccr_channel_number], lv_data[11:10], lv_data[7]);
						$display("DEST  : Peripheral  Addr: 'h%0h Transfer size: 'b%b Incr: %b",dma_cpar[lv_ccr_channel_number], lv_data[9:8], lv_data[6]);
					end
					else begin
						$display("SOURCE: Memory  Addr: 'h%0h Transfer size: 'b%b Incr: %b",dma_cmar[lv_ccr_channel_number], lv_data[11:10], lv_data[7]);
						$display("DEST  : Memory  Addr: 'h%0h Transfer size: 'b%b Incr: %b",dma_cpar[lv_ccr_channel_number], lv_data[9:8], lv_data[6]);
					end
					$display("Priority level: 'b%b Circular mode: %b CNDTR: 'h%h", lv_data[13:12], lv_data[5], dma_cndtr[lv_ccr_channel_number]);
				end
				else begin //the channel is being disabled
					//TODO since it is a CReg, what if we check in port [1]?
					if(currentReadRs[lv_ccr_channel_number][0]!=currentWriteRs[lv_ccr_channel_number][0]) begin	//there is an on going transaction
						rg_disable_channel<= tuple3(True, lv_ccr_channel_number, write_addr.awid);
						lv_send_response= False;
						rg_writeConfig_ccr<= tuple2(lv_ccr_channel_number, truncate(lv_data));
					end
					else begin	// no pending transaction
						//clear the local registers
						rg_is_cndtr_zero[lv_ccr_channel_number][0]<= True;
					end
				end
			end

			// Now generate the response and enqueue
			if(lv_send_response) begin
				thisReg <= lv_data;
				let resp = AXI4_Wr_Resp { bresp: lv_bresp, buser: 0, bid: write_addr.awid };
				s_xactor.i_wr_resp.enq(resp);
			end
		endrule
	endrules);

	//Rule to send response to processor that the dma channel has been disabled after the on going transactions are over
	Rules rl_send_chan_disabled_to_proc = (rules
		rule rl_send_chan_disabled_to_proc(tpl_1(rg_disable_channel) && currentReadRs[tpl_2(rg_disable_channel)][1]==currentWriteRs[tpl_2(rg_disable_channel)][1]);
			rg_disable_channel<= tuple2(False,?);
			dma_ccr[tpl_1(rg_writeConfig_ccr)]<= tpl_2(rg_writeConfig_ccr);
			let resp = AXI4_Wr_Resp { bresp: AXI4_OKAY, buser: 0, bid: tpl_3(rg_disable_channel) };
			s_xactor.i_wr_resp.enq(resp);
		endrule
	endrules);

	//writeConfig gets highest priority since we do not want the core to stall 
	ruleset= rJoinDescendingUrgency(writeConfig,ruleset); 

	//writeConfig if more urgent than rl_send_chan_disabled_to_proc
	ruleset= rJoinDescendingUrgency(ruleset, rl_send_chan_disabled_to_proc); 

	// A rule for reading a configuration register
	//TODO need to add preempts with writeConfig? or mutually_exclusive? Because both these rules will never fire together.
	// If we do not put any attributes, won't two instances of selectReg get synthesized?
	rule readConfig;
		AXI4_Resp lv_rresp;
		let read_addr <- pop_o(s_xactor.o_rd_addr);
		// Select the register
		let lv1= selectReg(read_addr.araddr);
		let thisReg = tpl_1(lv1);

		//If read happens to a non defined register,
		//or if read size is not 32-bits return SLVERR.
		if(!tpl_2(lv1))
			lv_rresp= AXI4_SLVERR;
		else
			lv_rresp= AXI4_OKAY;

		Req_Data lv_data;
		if(read_addr.arsize=='b0)
			lv_data={thisReg[7:0], thisReg[7:0], thisReg[7:0], thisReg[7:0], thisReg[7:0], thisReg[7:0], thisReg[7:0], thisReg[7:0]};
		else if(read_addr.arsize=='b01)
			lv_data={thisReg[15:0], thisReg[15:0], thisReg[15:0], thisReg[15:0]};
		else if(read_addr.arsize=='b10)
			lv_data={thisReg[31:0], thisReg[31:0]};
		else
			lv_data= thisReg;
		// Now generate the response and enqueue
		let resp = AXI4_Rd_Data {rresp: lv_rresp, rdata: thisReg, rlast: True,
								 ruser: 0, rid: read_addr.arid};
		s_xactor.i_rd_data.enq(resp);
	endrule


	// Add the rules to this module
	addRules (ruleset);

	//Note that unknownConfig rule is not needed here because all the FIFOs will
	//be empty and hence neither writeConfig nor readConfig will fire.

	////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////
	//
	// Create the interfaces by connecting the axi side interfaces
	// of the transactors to it.

	interface cfg= s_xactor.axi_side;
	interface mmu= m_xactor.axi_side;
	
	//This method receives various interrupts from the peripheral devices and gives it to the DMA
	method Action interrupt_from_peripherals(Bit#(numPeripherals) pint);
		wr_peripheral_interrupt<= unpack(pint);
	endmethod

	//TODO should the interrupt be sent in the next cycle (as is implemented) or in the same cycle as generated (using Wires instead)?
	//This method returns the interrupt generated by the DMA corresponding to every channel
	//Raise the interrupt of a particular channel if any of the interrupts are active (in ISR) and are not masked(in CCR)
	method Bit#(numChannels) interrupt_to_processor();
		Bit#(numChannels) lv_interrupt_to_processor;
		for(Integer chanNum= 0; chanNum < valueof(numChannels); chanNum= chanNum + 1) begin
			let lv_dma_ccr= dma_ccr[chanNum];
			//The bits in CCR represent the interrupts that are enabled, whereas the ones in IFCR represent which need to be cleared
			let lv_intr_TE_HT_TC_enable= lv_dma_ccr[3:1];

			//The bits in ISR represent which interrupts are active right now
			Bit#(3) active_interrupts= {lv_intr_TE_HT_TC_enable} & dma_isr[chanNum][3:1];
			lv_interrupt_to_processor[chanNum]= |(active_interrupts);
		end
		return lv_interrupt_to_processor;
	endmethod
endmodule

endpackage
