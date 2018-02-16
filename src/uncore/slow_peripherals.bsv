package slow_peripherals;
	/*===== Project imports =====*/
	import defined_types::*;
	import AXI4_Lite_Fabric::*;
	import AXI4_Lite_Types::*;
	import AXI4_Fabric::*;
	import AXI4_Types::*;
	import Semi_FIFOF::*;
	import AXI4Lite_AXI4_Bridge::*;
	`include "defined_parameters.bsv"
	/*===========================*/
	/*=== package imports ===*/
	import Clocks::*;
	import GetPut::*;
	import ClientServer::*;
	import Connectable::*;
	import Vector::*;
	import FIFO::*;
	/*=======================*/
	/*===== Import the slow peripherals ====*/
	`ifdef UART0
		import Uart16550		 :: *;
	`endif
	`ifdef UART1
		import Uart_bs::*;
		import RS232_modified::*;
	`endif
	`ifdef CLINT
		import clint::*;
	`endif
	`ifdef PLIC
		import gpio				::*;
		import plic				::*;
	`endif
	/*=====================================*/
	
	/*===== interface declaration =====*/
	interface SP_ios;
		`ifdef UART0
			interface RS232_PHY_Ifc uart0_coe;
		`endif
		`ifdef UART1
			interface RS232 uart1_coe;
		`endif
		`ifdef PLIC
			(*always_ready,always_enabled*)
			method Action gpio_in (Vector#(`IONum,Bit#(1)) inp);
			(*always_ready,always_enabled*)
			method Vector#(`IONum,Bit#(1))   gpio_out;
			(*always_ready,always_enabled*)
			method Vector#(`IONum,Bit#(1))   gpio_out_en;
		`endif
	endinterface
	interface Ifc_slow_peripherals;
		interface AXI4_Slave_IFC#(`PADDR,`Reg_width,`USERSPACE) axi_slave;
		interface SP_ios slow_ios;
		`ifdef CLINT
			method Bit#(1) msip_int;
			method Bit#(1) mtip_int;
			method Bit#(`Reg_width) mtime;
		`endif
		`ifdef PLIC method ActionValue#(Tuple2#(Bool,Bool)) intrpt_note; `endif
	endinterface
	/*================================*/

	function Tuple2#(Bool, Bit#(TLog#(Num_Slow_Slaves))) fn_address_mapping (Bit#(`PADDR) addr);
		`ifdef UART0
			if(addr>=`UART0Base && addr<=`UART0End)
				return tuple2(True,fromInteger(valueOf(Uart0_slave_num)));
			else
		`endif
		`ifdef UART1
			if(addr>=`UART1Base && addr<=`UART1End)
				return tuple2(True,fromInteger(valueOf(Uart1_slave_num)));
			else
		`endif
		`ifdef CLINT
			if(addr>=`ClintBase && addr<=`ClintEnd)
				return tuple2(True,fromInteger(valueOf(CLINT_slave_num)));
			else
		`endif
		`ifdef PLIC
			if(addr>=`PLICBase && addr<=`PLICEnd)
				return tuple2(True,fromInteger(valueOf(Plic_slave_num)));
			else if(addr>=`GPIOBase && addr<=`GPIOEnd)
				return tuple2(True,fromInteger(valueOf(GPIO_slave_num)));
			else
		`endif
		return tuple2(False,?);
	endfunction

	(*synthesize*)
	module mkslow_peripherals#(Clock fast_clock, Clock uart_clock)(Ifc_slow_peripherals);
		Clock sp_clock <-exposeCurrentClock; // slow peripheral clock
		Reset sp_reset <-exposeCurrentReset; // slow peripheral reset
		Reset uart_reset <-mkSyncResetFromCR(1,uart_clock); // reset synchronization for uart clock

		/*======= Module declarations for each peripheral =======*/
		`ifdef UART0
			Uart16550_AXI4_Lite_Ifc uart0 <- mkUart16550(clocked_by uart_clock, reset_by uart_reset, sp_clock, sp_reset);
		`endif
		`ifdef UART1
			Ifc_Uart_bs uart1 <- mkUart_bs(clocked_by uart_clock, reset_by uart_reset,sp_clock, sp_reset);
		`endif
		`ifdef CLINT
			Ifc_clint				clint				<- mkclint();
		`endif
		`ifdef PLIC
			Ifc_PLIC_AXI	plic <- mkplicperipheral();
         Wire#(Bit#(TLog#(`INTERRUPT_PINS))) interrupt_id <- mkWire();
			Vector#(`INTERRUPT_PINS, FIFO#(bit)) ff_gateway_queue <- replicateM(mkFIFO);
			GPIO						gpio				<- mkgpio;
		`endif
		/*=======================================================*/

   	AXI4_Lite_Fabric_IFC #(1, Num_Slow_Slaves, `PADDR, `Reg_width,`USERSPACE)	slow_fabric <- mkAXI4_Lite_Fabric(fn_address_mapping);
		Ifc_AXI4Lite_AXI4_Bridge bridge	<-mkAXI4Lite_AXI4_Bridge(fast_clock);
   	
		mkConnection (bridge.axi4_lite_master,	slow_fabric.v_from_masters [0]);
		/*======= Slave connections to AXI4Lite fabric =========*/
		`ifdef UART0
			mkConnection (slow_fabric.v_to_slaves [fromInteger(valueOf(Uart0_slave_num))],	uart0.slave_axi_uart);  
		`endif
		`ifdef UART1
	   	mkConnection (slow_fabric.v_to_slaves [fromInteger(valueOf(Uart1_slave_num))],	uart1.slave_axi_uart); 
		`endif
		`ifdef CLINT
			mkConnection (slow_fabric.v_to_slaves [fromInteger(valueOf(CLINT_slave_num))],clint.axi4_slave);
		`endif
		`ifdef PLIC
			mkConnection (slow_fabric.v_to_slaves [fromInteger(valueOf(Plic_slave_num))],	plic.axi4_slave_plic); //
			mkConnection (slow_fabric.v_to_slaves [fromInteger(valueOf(GPIO_slave_num))],	gpio.axi_slave); //
		`endif
		/*=======================================================*/
		/*=================== PLIC Connections ==================== */
		`ifdef PLIC
			/*TODO DMA interrupt need to be connected to the plic
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
			*/
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
        
			`ifdef UART0 
				SyncBitIfc#(Bit#(1)) uart0_interrupt <-mkSyncBitToCC(uart_clock,uart_reset); 
				rule synchronize_the_uart0_interrupt;
					uart0_interrupt.send(uart0.irq);		
				endrule
			rule rl_connect_uart_to_plic;
				`ifdef UART0
					if(uart0_interrupt.read==1'b1) begin
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
			
         rule rl_completion_msg_from_plic;
				let id <- plic.intrpt_completion;
            interrupt_id <= id;
            `ifdef verbose $display("Dequeing the FIFO -- PLIC Interrupt Serviced id: %d",id); `endif
			endrule

         for(Integer i=0; i <`INTERRUPT_PINS; i=i+1) begin
				rule deq_gateway_queue;
					if(interrupt_id==fromInteger(i)) begin
						ff_gateway_queue[i].deq;
                  `ifdef $display($time,"Dequeing the Interrupt request for ID: %d",i); `endif
               end
            endrule
         end

				
		`endif
			/*======================================================= */

		/* ===== interface definition =======*/
		interface axi_slave=bridge.axi_slave;
		`ifdef PLIC method intrpt_note = plic.intrpt_note; `endif
		`ifdef CLINT
			method msip_int=clint.msip_int;
			method mtip_int=clint.mtip_int;
			method mtime=clint.mtime;
		`endif
		interface SP_ios slow_ios;
			`ifdef UART0
				interface uart0_coe=uart0.coe_rs232;
			`endif
			`ifdef UART1
				interface uart1_coe=uart1.coe_rs232;
			`endif
			`ifdef PLIC
				method Action gpio_in (Vector#(`IONum,Bit#(1)) inp)=gpio.gpio_in(inp);
				method Vector#(`IONum,Bit#(1))   gpio_out=gpio.gpio_out;
				method Vector#(`IONum,Bit#(1))   gpio_out_en=gpio.gpio_out_en;
			`endif
		endinterface
		/*===================================*/
	endmodule
endpackage