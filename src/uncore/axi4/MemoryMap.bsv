package MemoryMap;
	/*=== Project imports ==== */
	import defined_types::*;
	`include "defined_parameters.bsv"
	/*========================= */


function Tuple2 #(Bool, Bit#(TLog#(Num_Slaves))) fn_addr_to_slave_num  (Bit#(`PADDR) addr);

		if(addr>=`SDRAMMemBase && addr<=`SDRAMMemEnd)
			return tuple2(True,fromInteger(valueOf(Sdram_slave_num)));
		else if(addr>=`DebugBase && addr<=`DebugEnd)
			return tuple2(True,fromInteger(valueOf(Debug_slave_num)));
		`ifdef SDRAM
			else if(addr>=`SDRAMCfgBase && addr<=`SDRAMCfgEnd )
				return tuple2(True,fromInteger(valueOf(Sdram_cfg_slave_num)));
		`endif
		`ifdef BOOTROM
			else if(addr>=`BootRomBase && addr<=`BootRomEnd)
				return tuple2(True,fromInteger(valueOf(BootRom_slave_num)));
		`endif
		`ifdef UART0
			else if(addr>=`UART0Base && addr<=`UART0End)
				return tuple2(True,fromInteger(valueOf(Uart0_slave_num)));
		`endif
		`ifdef UART1
			else if(addr>=`UART1Base && addr<=`UART1End)
				return tuple2(True,fromInteger(valueOf(Uart1_slave_num)));
		`endif
		`ifdef PLIC
			else if(addr>=`PLICBase && addr<=`PLICEnd)
				return tuple2(True,fromInteger(valueOf(Plic_slave_num)));
			else if(addr>=`GPIOBase && addr<=`GPIOEnd)
				return tuple2(True,fromInteger(valueOf(GPIO_slave_num)));
		`endif
		`ifdef QSPI0
			else if(addr>=`QSPI0CfgBase && addr<=`QSPI0CfgEnd)
				return tuple2(True,fromInteger(valueOf(Qspi0_slave_num)));
			else if(addr>=`QSPI0MemBase && addr<=`QSPI0MemEnd)
				return tuple2(True,fromInteger(valueOf(Qspi0_slave_num)));
		`endif
		`ifdef QSPI1
			else if(addr>=`QSPI1CfgBase && addr<=`QSPI1CfgEnd)
				return tuple2(True,fromInteger(valueOf(Qspi1_slave_num)));
			else if(addr>=`QSPI1MemBase && addr<=`QSPI1MemEnd)
				return tuple2(True,fromInteger(valueOf(Qspi1_slave_num)));
		`endif
		`ifdef I2C0
			else if(addr>=`I2C0Base && addr<=`I2C0End)	
				return tuple2(True,fromInteger(valueOf(I2c0_slave_num)));
		`endif
		`ifdef I2C1
			else if(addr>=`I2C1Base && addr<=`I2C1End)
				return tuple2(True,fromInteger(valueOf(I2c1_slave_num)));
		`endif
		`ifdef HYPER
			else if(addr>=`HyperCfgBase && addr<=`HyperCfgEnd)
				return tuple2(True,fromInteger(valueOf(Hyperflash_reg_slave_num)));
			else if(addr>=`HyperMemBase && addr<=`HyperMemEnd)
				return tuple2(True,fromInteger(valueOf(Hyperflash_mem_slave_num)));
		`endif
		`ifdef AXIEXP
			else if(addr>=`AxiExp1Base && addr<=`AxiExp1End)
				return tuple2(True,fromInteger(valueOf(AxiExp1_slave_num)));
		`endif
		`ifdef DMA
			else if(addr>=`DMABase && addr<=`DMAEnd)
				return tuple2(True,fromInteger(valueOf(Dma_slave_num)));
		`endif
		`ifdef TCMemory
			else if(addr>=`TCMBase && addr<=`TCMEnd)
				return tuple2(True,fromInteger(valueOf(TCM_slave_num)));
		`endif
		`ifdef CLINT
			else if(addr>=`ClintBase && addr<=`ClintEnd)
				return tuple2(True,fromInteger(valueOf(CLINT_slave_num)));
		`endif
	else
		return tuple2(False,?);
endfunction

function Bool is_IO_Addr(Bit#(`PADDR) addr); // TODO Shuold be PADDR
		if(addr>=`DebugBase && addr<=`DebugEnd)
			return (True);
		else if(addr>=`SDRAMMemBase && addr<=`SDRAMMemEnd)
			return (False);
		`ifdef BOOTROM
			else if(addr>=`BootRomBase && addr<=`BootRomEnd)
				return (False);
		`endif
		`ifdef TCMemory
			else if(addr>=`TCMBase && addr<=`TCMEnd)
				return (False);
		`endif
		else
			return True;
endfunction

	
endpackage
