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
	/*=======================*/
	/*===== Import the slow peripherals ====*/
	`ifdef UART0
		import Uart16550		 :: *;
	`endif
	`ifdef UART1
		import Uart_bs::*;
		import RS232_modified::*;
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
	endinterface
	interface Ifc_slow_peripherals;
		interface AXI4_Slave_IFC#(`PADDR,`Reg_width,`USERSPACE) axi_slave;
		interface SP_ios slow_ios;
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
		/*=======================================================*/

		/* ===== interface definition =======*/
		interface axi_slave=bridge.axi_slave;
		interface SP_ios slow_ios;
			`ifdef UART0
				interface uart0_coe=uart0.coe_rs232;
			`endif
			`ifdef UART1
				interface uart1_coe=uart1.coe_rs232;
			`endif
		endinterface
		/*===================================*/
	endmodule
endpackage
