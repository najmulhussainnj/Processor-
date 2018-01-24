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
	`include "defines.bsv"
	`include "defined_parameters.bsv"

		`ifdef DMA
			import DMA				 :: *;
		`endif
		`ifdef AXIEXP
			import axiexpansion	::*;
		`endif
		`ifdef HYPER
			import hyperflash_bsv_wrapper ::*;
		`endif
		`ifdef QSPI0 
			import qspi				 :: *; 
		`endif
		`ifdef PLIC
			import gpio				::*;
			import plic				::*;
		`endif
		`ifdef UART0
			import Uart16550		 :: *;
		`endif
		`ifdef UART1
			import Uart_bs::*;
			import RS232_modified::*;
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
		`ifdef CLINT
			import clint::*;
		`endif
		import DebugModule::*;
		import jtagdtm::*;
	/*========================= */
	interface Ifc_Soc;
		(*always_ready,always_enabled*)
		method Action boot_sequence(Bit#(1) bootseq);
		`ifdef UART0
			interface RS232_PHY_Ifc uart0_coe;
		`endif
		`ifdef UART1
			interface RS232 uart1_coe;
		`endif
		
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
		`ifdef PLIC
			(*always_ready,always_enabled*)
			method Action gpio_in (Vector#(`IONum,Bit#(1)) inp);
			(*always_ready,always_enabled*)
			method Vector#(`IONum,Bit#(1))   gpio_out;
			(*always_ready,always_enabled*)
			method Vector#(`IONum,Bit#(1))   gpio_out_en;
			(*always_ready,always_enabled*)
			method Vector#(`IONum,Bit#(1))   gpio_DRV0;
			(*always_ready,always_enabled*)
			method Vector#(`IONum,Bit#(1))   gpio_DRV1;
			(*always_ready,always_enabled*)
			method Vector#(`IONum,Bit#(1))   gpio_DRV2;
			(*always_ready,always_enabled*)
			method Vector#(`IONum,Bit#(1))   gpio_PD;
			(*always_ready,always_enabled*)
			method Vector#(`IONum,Bit#(1))   gpio_PPEN;
			(*always_ready,always_enabled*)
			method Vector#(`IONum,Bit#(1))   gpio_PRG_SLEW;
			(*always_ready,always_enabled*)
			method Vector#(`IONum,Bit#(1))   gpio_PUQ;
			(*always_ready,always_enabled*)
			method Vector#(`IONum,Bit#(1))   gpio_PWRUPZHL;
			(*always_ready,always_enabled*)
			method Vector#(`IONum,Bit#(1))   gpio_PWRUP_PULL_EN;
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
	module mkSoc #(Bit#(`VADDR) reset_vector, Clock clk0, Clock clk90, Clock clk180, Clock clk270, Clock tck, Reset trst)(Ifc_Soc);

			Ifc_jtagdtm tap <-mkjtagdtm(clocked_by tck, reset_by trst);
            Wire#(Bit#(TLog#(`INTERRUPT_PINS))) interrupt_id <- mkWire();
            rule drive_tmp_scan_outs;
                tap.scan_out_1_i(1'b0);
                tap.scan_out_2_i(1'b0);
                tap.scan_out_3_i(1'b0);
                tap.scan_out_4_i(1'b0);
                tap.scan_out_5_i(1'b0);
            endrule
            
			Ifc_DebugModule core<-mkDebugModule(reset_vector);
			`ifdef BOOTROM
				BootRom_IFC bootrom <-mkBootRom;
			`endif
			`ifdef SDRAM
				Ifc_sdr_slave			sdram				<- mksdr_axi4_slave(clk0);
			`else
				Memory_IFC#(`SDRAMMemBase,`Addr_space) main_memory <- mkMemory("code.mem.MSB","code.mem.LSB","MainMEM");
			`endif
			`ifdef UART0
				Uart16550_AXI4_Ifc uart0 <- mkUart16550();
			`endif
			`ifdef UART1
				Ifc_Uart_bs uart1 <- mkUart_bs();
			`endif
			`ifdef PLIC
				Ifc_PLIC_AXI	plic <- mkplicperipheral();
				Vector#(`INTERRUPT_PINS, FIFO#(bit)) ff_gateway_queue <- replicateM(mkFIFO);
				GPIO						gpio				<- mkgpio;
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
			`ifdef HYPER			
				Ifc_hyperflash			hyperflash		<- mkhyperflash(clk0,clk90,clk180,clk270);
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
			`ifdef CLINT
				Ifc_clint				clint				<- mkclint();
			`endif
				

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
			mkConnection (fabric.v_to_slaves [fromInteger(valueOf(Debug_slave_num))],core.debug_slave);
			`ifdef SDRAM	
				mkConnection (fabric.v_to_slaves [fromInteger(valueOf(Sdram_slave_num))],	sdram.axi4_slave_sdram); // 
	   		mkConnection (fabric.v_to_slaves [fromInteger(valueOf(Sdram_cfg_slave_num))],	sdram.axi4_slave_cntrl_reg); // 
			`else
				mkConnection(fabric.v_to_slaves[fromInteger(valueOf(Sdram_slave_num))],main_memory.axi_slave);
			`endif
			`ifdef UART0
				mkConnection (fabric.v_to_slaves [fromInteger(valueOf(Uart0_slave_num))],	uart0.slave_axi_uart);  
			`endif
			`ifdef UART1
	   		mkConnection (fabric.v_to_slaves [fromInteger(valueOf(Uart1_slave_num))],	uart1.slave_axi_uart); 
			`endif
  			`ifdef QSPI0 
				mkConnection (fabric.v_to_slaves [fromInteger(valueOf(Qspi0_slave_num))],	qspi0.slave); 
			`endif
  			`ifdef QSPI1 
				mkConnection (fabric.v_to_slaves [fromInteger(valueOf(Qspi1_slave_num))],	qspi1.slave); 
			`endif
			`ifdef PLIC
				mkConnection (fabric.v_to_slaves [fromInteger(valueOf(Plic_slave_num))],	plic.axi4_slave_plic); //
				mkConnection (fabric.v_to_slaves [fromInteger(valueOf(GPIO_slave_num))],	gpio.axi_slave); //
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
			`ifdef HYPER
   			mkConnection (fabric.v_to_slaves [fromInteger(valueOf(Hyperflash_mem_slave_num))],	hyperflash.axi4_slave_m); // 
   			mkConnection (fabric.v_to_slaves [fromInteger(valueOf(Hyperflash_reg_slave_num))],	hyperflash.axi4_slave_r); //
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
			`ifdef CLINT
				mkConnection (fabric.v_to_slaves [fromInteger(valueOf(CLINT_slave_num))],clint.axi4_slave);
			`endif

			`ifdef DMA
			//rule to connect all interrupt lines to the DMA
			//All the interrupt lines to DMA are active HIGH. For peripherals that are not connected, or those which do not
			//generate an interrupt (like TCM), drive a constant 1 on the corresponding interrupt line.
				rule rl_connect_interrupt_to_DMA;
					Bit#(12) lv_interrupt_to_DMA= {'d-1, 
															`ifdef HYPER pack(hyperflash.ifc_control_reg.oIENOn) `else 1'b1 `endif ,
															`ifdef I2C1 i2c1.isint `else 1'b1 `endif , 
															`ifdef I2C0 i2c0.isint `else 1'b1 `endif , 
															`ifdef QSPI1 qspi1.interrupts[5] `else 1'b1 `endif ,
															1'b1, 
															`ifdef QSPI0 qspi0.interrupts[5] `else 1'b1 `endif , 
															1'b1,1'b0, 
															`ifdef UART0 uart0.irq `else 1'b1 `endif };
					dma.interrupt_from_peripherals(lv_interrupt_to_DMA);
				endrule
			`endif

			/*=================== PLIC Connections ==================== */
			`ifdef PLIC
                for(Integer i=1; i<8; i=i+1) begin
                `ifdef DMA
                    rule rl_connect_dma_interrupts_to_plic;
                    if(dma.interrupt_to_processor[i-1]==1'b1) begin
                        ff_gateway_queue[i].enq(1);
						plic.ifc_external_irq[i].irq_frm_gateway(True);
                    end
                    endrule
                 `else
                    rule rl_connect_dma_interrupts_to_plic;
                        ff_gateway_queue[i].enq(0);
                    endrule
                 `endif
                end

                    rule rl_connect_i2c0_to_plic;
                    `ifdef I2C0
                    if(i2c0.isint()==1'b1) begin
                        ff_gateway_queue[8].enq(1);
						plic.ifc_external_irq[8].irq_frm_gateway(True);
                    end
                    `else
                        ff_gateway_queue[8].enq(0);
                    `endif
                    endrule

                    rule rl_connect_i2c1_to_plic;
                    `ifdef I2C1
                    if(i2c1.isint()==1'b1) begin
                        ff_gateway_queue[9].enq(1);
						plic.ifc_external_irq[9].irq_frm_gateway(True);
                    end
                    `else
                        ff_gateway_queue[9].enq(0);
                    `endif
                    endrule

                    rule rl_connect_i2c0_timerint_to_plic;
                    `ifdef I2C0
                    if(i2c0.timerint()==1'b1) begin
                        ff_gateway_queue[10].enq(1);
						plic.ifc_external_irq[10].irq_frm_gateway(True);
                    end
                    `else
                        ff_gateway_queue[10].enq(0);
                    `endif
                    endrule

                    rule rl_connect_i2c1_timerint_to_plic;
                    `ifdef I2C1
                    if(i2c1.timerint()==1'b1) begin
                        ff_gateway_queue[11].enq(1);
						plic.ifc_external_irq[11].irq_frm_gateway(True);
                    end
                    `else
                        ff_gateway_queue[11].enq(0);
                    `endif
                    endrule

                    rule rl_connect_i2c0_isber_to_plic;
                    `ifdef I2C0
                    if(i2c0.isber()==1'b1) begin
                        ff_gateway_queue[12].enq(1);
						plic.ifc_external_irq[12].irq_frm_gateway(True);
                    end
                    `else
                        ff_gateway_queue[12].enq(0);
                    `endif
                    endrule

                    rule rl_connect_i2c1_isber_to_plic;
                    `ifdef I2C1
                        if(i2c1.isber()==1'b1) begin
                            ff_gateway_queue[13].enq(1);
						    plic.ifc_external_irq[13].irq_frm_gateway(True);
                        end
                    `else
                        ff_gateway_queue[13].enq(0);
                    `endif
                    endrule

                    for(Integer i = 14; i < 20; i=i+1) begin
                        rule rl_connect_qspi0_to_plic;
                        `ifdef QSPI0
                        if(qspi0.interrupts()[i-14]==1'b1) begin
                            ff_gateway_queue[i].enq(1);
						    plic.ifc_external_irq[i].irq_frm_gateway(True);
                        end
                        `else
                            ff_gateway_queue[i].enq(0);
                        `endif
                        endrule
                    end

                    for(Integer i = 20; i<26; i=i+1) begin
                        rule rl_connect_qspi1_to_plic;
                        `ifdef QSPI1
                        if(qspi1.interrupts()[i-20]==1'b1) begin
                            ff_gateway_queue[i].enq(1);
						    plic.ifc_external_irq[i].irq_frm_gateway(True);
                        end
                        `else
                            ff_gateway_queue[i].enq(0);
                        `endif
                        endrule
                    end
                
                    rule rl_connect_hyperflash_to_plic;
                    `ifdef HYPER
                       if(hyperflash.ifc_control_reg.oIENOn()) begin
                            ff_gateway_queue[26].enq(1);
						    plic.ifc_external_irq[26].irq_frm_gateway(True);
                        end
                    `else
                        ff_gateway_queue[26].enq(0);
                    `endif
                    endrule
        
                    rule rl_connect_uart_to_plic;
                    `ifdef UART0
                        if(uart0.irq==1'b1) begin
                            ff_gateway_queue[27].enq(1);
						    plic.ifc_external_irq[27].irq_frm_gateway(True);
                        end
                    `else
                        ff_gateway_queue[27].enq(0);
                    `endif
                    endrule
                
				for(Integer i = 28; i<`INTERRUPT_PINS; i=i+1) begin
					rule rl_raise_interrupts;
						if((i-28)<`IONum) begin	//Peripheral interrupts
							if(gpio.to_plic[i-28]==1'b1) begin
								plic.ifc_external_irq[i].irq_frm_gateway(True);
									ff_gateway_queue[i].enq(1);	
                     end
						end
					endrule
				end

				//for(Integer i=0; i<`INTERRUPT_PINS; i=i+1) begin
				//	rule rl_connect_interrupts_to_plic;
				//		plic.ifc_external_irq[i].irq_frm_gateway(unpack(ff_gateway_queue[i].first));
                //        $display("PLIC taking Interrupt id: %d value: %b ",i, ff_gateway_queue[i].first);
				//	endrule
				//end

			
                rule rl_completion_msg_from_plic;
					let id <- plic.intrpt_completion;
                    interrupt_id <= id;
                    $display("Dequeing the FIFO -- PLIC Interrupt Serviced id: %d",id);
					//ff_gateway_queue[id].deq;
				endrule

                for(Integer i=0; i <`INTERRUPT_PINS; i=i+1) begin
                    rule deq_gateway_queue;
                        if(interrupt_id==fromInteger(i)) begin
                            ff_gateway_queue[i].deq;
                            $display($time,"Dequeing the Interrupt request for ID: %d",i);
                        end
                    endrule
                end

				
                rule rl_send_external_interrupt_to_csr;
					let note <- plic.intrpt_note;
					core.set_external_interrupt(note);
				endrule
			`endif
			/*======================================================= */

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
			rule connect_msip_mtip_from_clint;
				core.clint_msip(clint.msip_int);
				core.clint_mtip(clint.mtip_int);
                 core.clint_mtime(clint.mtime);
			endrule
		`endif

      method Action boot_sequence(Bit#(1) bootseq) = core.boot_sequence(bootseq);
		`ifdef UART0
			interface uart0_coe=uart0.coe_rs232;
		`endif
		`ifdef UART1
			interface uart1_coe=uart1.coe_rs232;
		`endif
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
		`ifdef PLIC
			method Action gpio_in (Vector#(`IONum,Bit#(1)) inp)=gpio.gpio_in(inp);
			method Vector#(`IONum,Bit#(1))   gpio_out=gpio.gpio_out;
			method Vector#(`IONum,Bit#(1))   gpio_out_en=gpio.gpio_out_en;
			method Vector#(`IONum,Bit#(1))   gpio_DRV0=gpio.gpio_DRV0;
			method Vector#(`IONum,Bit#(1))   gpio_DRV1=gpio.gpio_DRV1;
			method Vector#(`IONum,Bit#(1))   gpio_DRV2=gpio.gpio_DRV2;
			method Vector#(`IONum,Bit#(1))   gpio_PD=gpio.gpio_PD;
			method Vector#(`IONum,Bit#(1))   gpio_PPEN=gpio.gpio_PPEN;
			method Vector#(`IONum,Bit#(1))   gpio_PRG_SLEW=gpio.gpio_PRG_SLEW;
			method Vector#(`IONum,Bit#(1))   gpio_PUQ=gpio.gpio_PUQ;
			method Vector#(`IONum,Bit#(1))   gpio_PWRUPZHL=gpio.gpio_PWRUPZHL;
			method Vector#(`IONum,Bit#(1))   gpio_PWRUP_PULL_EN=gpio.gpio_PWRUP_PULL_EN;
		`endif
		`ifdef HYPER
	      interface ifc_flash=hyperflash.ifc_flash;
		`endif
		`ifdef AXIEXP
			interface axiexp1_out=axiexp1.slave_out;
			interface axiexp1_in=axiexp1.slave_in;
		`endif

	endmodule
endpackage
