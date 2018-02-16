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
package Soc;
	/*====== Package imports === */
	import FIFO::*;
	import FIFOF::*;
	import SpecialFIFOs::*;
	import GetPut::*;
	import ClientServer::*;
	import Vector::*;
	import Connectable::*;
	import Clocks::*;
	/*========================== */
	/*=== Project imports === */
	import ConcatReg::*;
	import AXI4_Types::*;
	import AXI4_Fabric::*;
	import defined_types::*;
	import MemoryMap		 :: *;
	import slow_peripherals::*;
	`include "defines.bsv"
	`include "defined_parameters.bsv"

		`ifdef DMA
			import DMA				 :: *;
		`endif
		`ifdef AXIEXP
			import axiexpansion	::*;
		`endif
		`ifdef QSPI0 
			import qspi				 :: *; 
		`endif
		`ifdef BOOTROM
			import BootRom			::*;
		`endif
		`ifdef SDRAM
			import sdr_top			 :: *;
		`else
			import Memory_AXI4	::*;
		`endif
		`ifdef I2C0
			import I2C_top			 :: *;
		`endif
		`ifdef TCMemory
			import TCM::*;
		`endif
		`ifdef Debug
			import jtagdtm::*;
			import DebugModule::*;
		`else
			import core::*;
		`endif
	/*========================= */
	interface Ifc_Soc;
		interface SP_ios slow_ios;
		(*always_ready,always_enabled*)
		method Action boot_sequence(Bit#(1) bootseq);
		
		`ifdef SDRAM 
			(*always_ready*) interface Ifc_sdram_out sdram_out; 
		`endif
		`ifdef QSPI0 
			interface QSPI_out qspi0_out; 
		`endif
      `ifdef QSPI1 
			interface QSPI_out qspi1_out; 
		`endif
      `ifdef Debug
			(*always_ready,always_enabled*)
			method Action tms_i(Bit#(1) tms);
			(*always_ready,always_enabled*)
			method Action tdi_i(Bit#(1) tdi);
			(*always_ready,always_enabled*)
			method Action bs_chain_i(Bit#(1) bs_chain);
			(*always_ready,always_enabled*)
			method Bit#(1) shiftBscan2Edge;
			(*always_ready,always_enabled*)
			method Bit#(1) selectJtagInput;
			(*always_ready,always_enabled*)
			method Bit#(1) selectJtagOutput;
			(*always_ready,always_enabled*)
			method Bit#(1) updateBscan;
			(*always_ready,always_enabled*)
			method Bit#(1) bscan_in;
			(*always_ready,always_enabled*)
			method Bit#(1) scan_shift_en;
			(*always_ready,always_enabled*)
			method Bit#(1) tdo;
			(*always_ready,always_enabled*)
			method Bit#(1) tdo_oe;
		`endif
		`ifdef I2C0
			interface I2C_out i2c0_out;
		`endif
		`ifdef I2C1
			interface I2C_out i2c1_out;
		`endif
		`ifdef AXIEXP
//			method ActionValue#(Bit#(67)) axiexp1_out;
//			method Action axiexp1_in(Bit#(67) datain);
			interface Get#(Bit#(67)) axiexp1_out;
			// 1-bit indicating slave response or master request , 2 bits for slverror, 64-info signals, 
			interface Put#(Bit#(67)) axiexp1_in;
		`endif
		`ifdef HYPER
			(*always_ready,always_enabled*)	
		   interface Ifc_flash ifc_flash;
		`endif
	/*=============================================== */
	endinterface
	(*synthesize*)
	module mkSoc #(Bit#(`VADDR) reset_vector, Clock slow_clock, Clock uart_clock,  Clock clk0, Clock tck, Reset trst)(Ifc_Soc);
			Clock core_clock <-exposeCurrentClock; // slow peripheral clock
			Reset core_reset <-exposeCurrentReset; // slow peripheral reset
			Reset slow_reset <-mkAsyncResetFromCR(1,slow_clock);
         `ifdef Debug 
				Ifc_jtagdtm tap <-mkjtagdtm(clocked_by tck, reset_by trst);
            rule drive_tmp_scan_outs;
                tap.scan_out_1_i(1'b0);
                tap.scan_out_2_i(1'b0);
                tap.scan_out_3_i(1'b0);
                tap.scan_out_4_i(1'b0);
                tap.scan_out_5_i(1'b0);
            endrule
				Ifc_DebugModule core<-mkDebugModule(reset_vector);
			`else
				Ifc_core_AXI4 core <-mkcore_AXI4(reset_vector);
			`endif
			`ifdef BOOTROM
				BootRom_IFC bootrom <-mkBootRom;
			`endif
			`ifdef SDRAM
				Ifc_sdr_slave			sdram				<- mksdr_axi4_slave(clk0);
			`else
				Memory_IFC#(`SDRAMMemBase,`Addr_space) main_memory <- mkMemory("code.mem.MSB","code.mem.LSB","MainMEM");
			`endif
			`ifdef QSPI0 
				Ifc_qspi			qspi0				<-	mkqspi(); 
			`endif
			`ifdef QSPI1 
				Ifc_qspi			qspi1				<-	mkqspi(); 
			`endif
			`ifdef I2C0 
				I2C_IFC					i2c0				<- mkI2CController();
			`endif
			`ifdef I2C1
				I2C_IFC					i2c1				<- mkI2CController();
			`endif
			`ifdef TCMemory
				Ifc_TCM					tcm				<- mkTCM;	
			`endif
			`ifdef DMA
				DmaC#(7,12)				dma				<- mkDMA();
			`endif
			`ifdef AXIEXP
				Ifc_AxiExpansion		axiexp1			<- mkAxiExpansion();	
			`endif
		Ifc_slow_peripherals slow_peripherals <-mkslow_peripherals(core_clock, uart_clock, clocked_by slow_clock , reset_by slow_reset);	

   	// Fabric
   	AXI4_Fabric_IFC #(Num_Masters, Num_Slaves, `PADDR, `Reg_width,`USERSPACE)
		 		fabric <- mkAXI4_Fabric(fn_addr_to_slave_num);

   	// Connect traffic generators to fabric
   	mkConnection (core.dmem_master,	fabric.v_from_masters [fromInteger(valueOf(Dmem_master_num))]);
   	mkConnection (core.imem_master,	fabric.v_from_masters [fromInteger(valueOf(Imem_master_num))]);
    `ifdef Debug
		mkConnection (core.debug_master, fabric.v_from_masters [fromInteger(valueOf(Debug_master_num))]);
    `endif
    `ifdef DMA
            mkConnection (dma.mmu, fabric.v_from_masters[fromInteger(valueOf(DMA_master_num))]);
    `endif


		// Connect fabric to memory slaves
			`ifdef Debug
				mkConnection (fabric.v_to_slaves [fromInteger(valueOf(Debug_slave_num))],core.debug_slave);
			`endif
			`ifdef SDRAM	
				mkConnection (fabric.v_to_slaves [fromInteger(valueOf(Sdram_slave_num))],	sdram.axi4_slave_sdram); // 
	   		mkConnection (fabric.v_to_slaves [fromInteger(valueOf(Sdram_cfg_slave_num))],	sdram.axi4_slave_cntrl_reg); // 
			`else
				mkConnection(fabric.v_to_slaves[fromInteger(valueOf(Sdram_slave_num))],main_memory.axi_slave);
			`endif
  			`ifdef QSPI0 
				mkConnection (fabric.v_to_slaves [fromInteger(valueOf(Qspi0_slave_num))],	qspi0.slave); 
			`endif
  			`ifdef QSPI1 
				mkConnection (fabric.v_to_slaves [fromInteger(valueOf(Qspi1_slave_num))],	qspi1.slave); 
			`endif
			`ifdef BOOTROM
				mkConnection (fabric.v_to_slaves [fromInteger(valueOf(BootRom_slave_num))],bootrom.axi_slave);
			`endif
			`ifdef I2C0
   			mkConnection (fabric.v_to_slaves [fromInteger(valueOf(I2c0_slave_num))],		i2c0.slave_i2c_axi); 
			`endif
			`ifdef I2C1
   			mkConnection (fabric.v_to_slaves [fromInteger(valueOf(I2c1_slave_num))],		i2c1.slave_i2c_axi); // 
			`endif
			`ifdef DMA
   			mkConnection (fabric.v_to_slaves [fromInteger(valueOf(Dma_slave_num))],	dma.cfg); //DMA slave
			`endif
			`ifdef AXIEXP
   			mkConnection (fabric.v_to_slaves [fromInteger(valueOf(AxiExp1_slave_num))],	axiexp1.axi_slave); //
			`endif
			`ifdef TCMemory
				mkConnection (fabric.v_to_slaves [fromInteger(valueOf(TCM_slave_num))],tcm.axi_slave);
			`endif
			mkConnection(fabric.v_to_slaves [fromInteger(valueOf(SlowPeripheral_slave_num))],slow_peripherals.axi_slave);

			`ifdef DMA
			//rule to connect all interrupt lines to the DMA
			//All the interrupt lines to DMA are active HIGH. For peripherals that are not connected, or those which do not
			//generate an interrupt (like TCM), drive a constant 1 on the corresponding interrupt line.
				rule rl_connect_interrupt_to_DMA;
					Bit#(12) lv_interrupt_to_DMA= {'d-1, 
															`ifdef I2C1 i2c1.isint `else 1'b1 `endif , 
															`ifdef I2C0 i2c0.isint `else 1'b1 `endif , 
															`ifdef QSPI1 qspi1.interrupts[5] `else 1'b1 `endif ,
															1'b1, 
															`ifdef QSPI0 qspi0.interrupts[5] `else 1'b1 `endif , 
															1'b1,1'b0, 
															1'b1 /*TODO: Bring UART0 interrupt here */ };
					dma.interrupt_from_peripherals(lv_interrupt_to_DMA);
				endrule
			`endif


		/*======= Synchornization between the JTAG and the Debug Module ========= */
		`ifdef Debug
			SyncFIFOIfc#(Bit#(40)) sync_request_to_dm <-mkSyncFIFOToCC(1,tck,trst);
			SyncFIFOIfc#(Bit#(34)) sync_response_from_dm <-mkSyncFIFOFromCC(1,tck);
			rule connect_tap_request_to_syncfifo;
				let x<-tap.request_to_dm;
				sync_request_to_dm.enq(x);
			endrule
			rule read_synced_request_to_dm;
				sync_request_to_dm.deq;
				core.request_from_dtm(sync_request_to_dm.first);
			endrule

			rule connect_debug_response_to_syncfifo;
				let x<-core.response_to_dtm;
				sync_response_from_dm.enq(x);
			endrule
			rule read_synced_response_from_dm;
				sync_response_from_dm.deq;
				tap.response_from_dm(sync_response_from_dm.first);
			endrule
		`endif
		/*======================================================================= */
	
		`ifdef CLINT
			SyncBitIfc#(Bit#(1)) clint_mtip_int <-mkSyncBitToCC(slow_clock,slow_reset);
			SyncBitIfc#(Bit#(1)) clint_msip_int <-mkSyncBitToCC(slow_clock,slow_reset);
			Reg#(Bit#(`Reg_width)) clint_mtime_value <-mkSyncRegToCC(0,slow_clock,slow_reset);
			rule synchronize_clint_data;
				clint_mtip_int.send(slow_peripherals.mtip_int);
				clint_msip_int.send(slow_peripherals.msip_int);
				clint_mtime_value<=slow_peripherals.mtime;
			endrule
			rule connect_msip_mtip_from_clint;
				core.clint_msip(clint_msip_int.read);
				core.clint_mtip(clint_mtip_int.read);
            core.clint_mtime(clint_mtime_value);
			endrule
		`endif
		`ifdef PLIC
			Reg#(Tuple2#(Bool,Bool)) plic_interrupt_note <-mkSyncRegToCC(tuple2(False,False),slow_clock,slow_reset);
			rule synchronize_interrupts;
				let note <- slow_peripherals.intrpt_note;
				plic_interrupt_note<=note;
			endrule
         rule rl_send_external_interrupt_to_csr;
				core.set_external_interrupt(plic_interrupt_note);
			endrule
		`endif
      method Action boot_sequence(Bit#(1) bootseq) = core.boot_sequence(bootseq);
		`ifdef QSPI0 interface qspi0_out = qspi0.out; `endif
      `ifdef QSPI1 interface qspi1_out = qspi1.out; `endif

		`ifdef SDRAM
			interface sdram_out=sdram.ifc_sdram_out;
		`endif
		`ifdef Debug
			method Action tms_i(Bit#(1) tms);
				tap.tms_i(tms);
			endmethod
			method Action tdi_i(Bit#(1) tdi);
				tap.tdi_i(tdi);
			endmethod
			method Action bs_chain_i(Bit#(1) bs_chain);
				tap.bs_chain_i(bs_chain);
			endmethod
//			method Action tck_i(Bit#(1) tck);
//				tap.tck_i(tck);
//			endmethod
//			method Action trst_i(Bit#(1) trst);
//				tap.trst_i(trst);
//			endmethod
			method Bit#(1) shiftBscan2Edge=tap.shiftBscan2Edge;
			method Bit#(1) selectJtagInput=tap.selectJtagInput;
			method Bit#(1) selectJtagOutput=tap.selectJtagOutput;
			method Bit#(1) updateBscan=tap.updateBscan;
			method Bit#(1) bscan_in=tap.bscan_in;
            method Bit#(1) scan_shift_en=tap.scan_shift_en;
			method Bit#(1) tdo=tap.tdo;
			method Bit#(1) tdo_oe=tap.tdo_oe;
		`endif
		`ifdef I2C0
			interface i2c0_out=i2c0.out;
		`endif
		`ifdef I2C1
			interface i2c1_out=i2c1.out;
		`endif
		`ifdef AXIEXP
			interface axiexp1_out=axiexp1.slave_out;
			interface axiexp1_in=axiexp1.slave_in;
		`endif
		interface slow_ios=slow_peripherals.slow_ios;

	endmodule
endpackage
