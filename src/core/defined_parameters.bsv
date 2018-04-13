`define RegFileSize 32 // describes the size of ht register file in the processor.
`ifdef spfpu
	`define FLEN 32
`endif
`ifdef dpfpu
	`define FLEN 64
`endif
//`define fpu_hierarchical //Define this if you want hierarchical modules in verilog

//`define MMU
`define PRFDEPTH 6
`define USERSPACE 0
`ifdef RV64
	`define Burst_length_bits 8
	`define byte_offset 2
	`define Reg_width 64 // the register data width of the processor.
	`define ADDR 64 // the address width
	`define DCACHE_ADDR 64
	`define DCACHE_BLOCK_SIZE 4
	`define DCACHE_WORD_SIZE 8
/////////////////////////////MMU parameters///////////////////////////////////
`define VADDR 	39	
`define PADDR   32
`define OFFSET	12
`define ASID		8
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
	// TLM2 Request Response definitions for Processor to Bus connection
	`define TLM_PRM_CPU_REQ 4, 64, 64, 5, Bit #(0)
	`define TLM_PRM_CPU_RSP 4, 64, 64, 5, Bit #(0)

	// TLM2 Request Response definitions for Memory to Bus connection
	`define TLM_PRM_MEM_REQ 4, 64, 64, 5, Bit #(0)
	`define TLM_PRM_MEM_RSP 4, 64, 64, 5, Bit #(0)

	// Axi Request Response definitions for Processor as a Master
	`define AXI_PRM_CPU	4, 64, 64, 5, Bit #(0)	// Fabric Interface
	`define AXI_XTR_CPU TLMRequest #(`TLM_PRM_CPU_REQ), TLMResponse #(`TLM_PRM_CPU_RSP), `AXI_PRM_CPU // Transactor Interface

	// Axi Request Response definitions for Memory as a Slave
	`define AXI_PRM_MEM	4, 64, 64, 5, Bit #(0)	// Fabric Interface
	`define AXI_XTR_MEM TLMRequest #(`TLM_PRM_MEM_REQ), TLMResponse #(`TLM_PRM_MEM_RSP), `AXI_PRM_MEM // Transactor Interface
///////////////////////////////////////////////////////////////////////////////
`else
	`define byte_offset 1
	`define Reg_width 32 // the register data width of the processor.
	`define Addr_width 32 // the address width
	`define DCACHE_ADDR 32
	`define DCACHE_BLOCK_SIZE 8
	`define DCACHE_WORD_SIZE 4
///////////////////////////////////////////////////////////////////////////////
	// TLM2 Request Response definitions for Processor to Bus connection
	`define TLM_PRM_CPU_REQ 4, 32, 32, 5, Bit #(0)
	`define TLM_PRM_CPU_RSP 4, 32, 32, 5, Bit #(0)

	// TLM2 Request Response definitions for Memory to Bus connection
	`define TLM_PRM_MEM_REQ 4, 32, 32, 5, Bit #(0)
	`define TLM_PRM_MEM_RSP 4, 32, 32, 5, Bit #(0)

	// Axi Request Response definitions for Processor as a Master
	`define AXI_PRM_CPU	4, 32, 32, 5, Bit #(0)	// Fabric Interface
	`define AXI_XTR_CPU TLMRequest #(`TLM_PRM_CPU_REQ), TLMResponse #(`TLM_PRM_CPU_RSP), `AXI_PRM_CPU // Transactor Interface

	// Axi Request Response definitions for Memory as a Slave
	`define AXI_PRM_MEM	4, 32, 32, 5, Bit #(0)	// Fabric Interface
	`define AXI_XTR_MEM TLMRequest #(`TLM_PRM_MEM_REQ), TLMResponse #(`TLM_PRM_MEM_RSP), `AXI_PRM_MEM // Transactor Interface
///////////////////////////////////////////////////////////////////////////////
`endif

`define Loop 1
`define BAUD_RATE 130
`ifdef verilog
	`define Addr_space 22	//since we are leaving off the lower 2 bits of address(byte addressable memory), we have to 
`else
	`define Addr_space 30
`endif
`ifdef simulate
  `define BAUD_RATE 5 //130 //
`endif
`define INTERRUPT_PINS 64 

// Branch_predictor_paramters
/////////////////////////// CACHE RELATED PARAMETERS ////////////////////////////////
`define DCACHE_WAYS 4
`define DCACHE_SETS 512

`define ICACHE_WAYS 4			// way_bits =2
`define ICACHE_BLOCK_SIZE 8	// word_bits = 3
`define ICACHE_SETS 512			// set_bits	=7
`define ICACHE_WORD_SIZE 4		// byte_bits=2
`define ICACHE_TAG_BITS 20		// tag_bits = 52
`define DCACHE_TAG_BITS 20		// tag_bits = 52
`define BTB_DEPTH			256
`define RAS_DEPTH			8
/////////////////////////////////////////////////////////////////////////////////////
`ifdef RV64
	`define MISA_BITS   'h141129 //'h082C849//// 'h40101121 // A + F + I + M + U 
	`define MXL_BITS		'h2
`else
	`define MISA_BITS   'h082C849 // 'h40101121 // A + F + I + M + U 
	`define MXL_BITS		'h1
`endif
`define MTVEC_DEFAULT       'h00000000
`define STVEC_DEFAULT       'h00000000
`define UTVEC_DEFAULT       'h00000000
/////////////////////////// Register Mapping for Machine Mode Regs /////////////////
`define MSTATUS			'h00 //'h300 // Machine Status register                                
`define MISA				'h01 //'h301 // ISA and extensions                                     
`define MEDELEG			'h02 //'h302 // Machine exception delegation                               
`define MIDELEG			'h03 //'h303 // Machine interrupt delegation                               
`define MIE					'h04 //'h304 // Machine interrupt enable                                   
`define MTVEC				'h05 //'h305 // Machine trap-handler base address                          
`define MCOUNTEREN		'h06 //'h306 // Machine counter setup register                                  
`define MHPMEVENTSTART	'h23 //'h323 // statr of event selectors
`define MHPMEVENTEND		'h26 //'h326 // end of event selectors
`define MSCRATCH			'h40 //'h340 // Scratch rgister for machine trap hanglers                  
`define MEPC				'h41 //'h341 // Machine exception program counter                          
`define MCAUSE				'h42 //'h342 // Machine trap cause                                         
`define MTVAL				'h43 //'h343 // Machine bad address                                        
`define MIP					'h44 //'h344 // Machine interrupt pending
`define MPOWERCONTROL	'h45 //'h345 // 2 bits to control the power switches.
`define PMPCFG0			'hA0 //'h3A0 // 
`ifndef RV64
	`define PMPCFG1			'hA1 //'h3A1 // Physical Memory Protection Configuration Registers 
`endif
`define PMPCFG2			'hA2 //'h3A2 // Physical Memory Protection Configuration Registers 
`ifndef RV64
	`define PMPCFG3			'hA3 //'h3A3 // Physical Memory Protection Configuration Registers 
`endif
`define PMPADDRSTART 'hB0 //PMP Address array start 
`define PMPADDREND   'hB7 //PMP Address array end
`define MCYCLE				'h00 //'hB00 // Machine cycle counter                                      
`define MTIME				'h01 //'hB01	// mtime register (Non-standard r/w)
`define MINSTRET			'h02 //'hB02 // Machine instructions retired.                              
`define MHPMCOUNTSTART 	'h03 //'hB03 // start address for performance counters
`define MHPMCOUNTEND 	'h05 //'hB05 // end address for performance counters
`define MTIMECMP			'h20 //'hB20 //  time compare register (Non-standard r/w)
`define MCYCLEH			'h80 //'hB80 // Upper 32 bits of mcycle                                   
`define MTIMEH				'h81 //'hB81	// mtime hi-register (Non-standard r/w)
`define MINSTRETH			'h82 //'hB82 // Upper 32 bits of minstret.                                 
`define MHPMCOUNTHSTART 'h83 //'hB83 // start address for performance counters higher bits
`define MHPMCOUNTHEND 	'h85 //'hB86 // end address for performance counters higher bits
`define MTIMECMPH			'hA0 //'hBA0 //  time compare hi-register (Non-standard r/w)
`define MHPMTHRESSTART	'hA3 //'hBA3 // start of counter-thresholds
`define MHPMTHRESEND		'hA5 //'hBA6 // end of counter-thresholds
`define MVENDORID			'h11 //'hF11 // Vendor ID                                                  
`define MARCHID			'h12 //'hF12 // Architecture ID                                           
`define MIMPID				'h13 //'hF13 // Implementation ID                                        
`define MHARTID			'h14 //'hF14 // Hardware Thread ID                                      
`define MBOOTSEQ			'h15 //'hF15 // Hardware Thread ID                                      

`define MHPMTHRESHSTART	'hE0 //'h7E0 // start of counter-thresholds
`define MHPMTHRESHEND	'hE2 //'h7FC // end of counter-thresholds
/////////////////////////// Register Mapping for Supervisor Mode Regs /////////////////
`define SSTATUS     'h00 //'h100 // Supervisor Status register                                
`define SEDELEG     'h02 //'h102 // Supervisor exception delegation                               
`define SIDELEG     'h03 //'h103 // Supervisor interrupt delegation                               
`define SIE         'h04 //'h104 // Supervisor interrupt enable                                   
`define STVEC       'h05 //'h105 // Supervisor trap-handler base address                          
`define SCOUNTEREN  'h06 //'h106 // Supervisor counter setup register                                  
`define SSCRATCH    'h40 //'h140 // Scratch register for supervisor trap hanglers                  
`define SEPC        'h41 //'h141 // Supervisor exception program counter                          
`define SCAUSE      'h42 //'h142 // Supervisor trap cause                                         
`define STVAL		  'h43 //'h143 // Supervisor bad address or illegal instruction                                  
`define SIP         'h44 //'h144 // Supervisor interrupt pending                                  
`define SATP        'h80 //'h180 // Supervisor interrupt pending                                  

/////////////////////////// Register Mapping for User Mode Regs /////////////////
`define USTATUS		'h00 //'h000 // User status register
`define FFLAGS			'h01 //'h001 // FP Accrued exceptions
`define FRM				'h02 //'h002 // FP Dynamic rounding mode
`define FCSR			'h03 //'h003 // FP Control and status register
`define UIE				'h04 //'h004 // User interrupt enable register
`define UTVEC			'h05 //'h005 // User trap handler base address
`define USCRATCH		'h40 //'h040 // Scratch register for user trap handlers
`define UEPC			'h41 //'h041 // User exception program counter
`define UCAUSE			'h42 //'h042 // User trap cause
`define UTVAL			'h43 //'h043 // User bad address or illegal instruction
`define UIP				'h44 //'h044 // User interrupt pending
`define UMEMSE			'h45 //'h045 // Machine Memory Structures enable
`define UCYCLE			'h00 //'hC00 // cycle counter for RDCYCLE instruction.
`define UTIME			'h01 //'hC01 // Tiemr for RDTIME instruction
`define UINSTRET		'h02 //'hC02 // Instruction retired counter for RDINSTRET
`define UCYCLEH		'h80 //'hC80 // Upper 32bits of UCYCLE
`define UTIMEH			'h81 //'hC81 // Upper 32bits of UTIME
`define UINSTRETH		'h82 //'hC82 // Upper 32bits of UINSTRET
`define HPMCOUNTSTART	'h03 	//'hC03 // start address for performance counters
`define HPMCOUNTEND		'h05  //'hC06 // start address for performance counters
`define HPMCOUNTHSTART	'h83 	//'hC83 // start address for performance counters higher bits
`define HPMCOUNTHEND		'h85  //'hC86 // end address for performance counters higher bits

////////////////////////////////////////////////////////////////////////////////////
/////////// Debug registers //////////////////////////
`define DCSR		'hb0 //'h7b0
`define DPC			'hb1 //'h7b1
`define DSCRATCH0	'hb2 //'h7b2
`define DSCRATCH1	'hb3 //'h7b3
`define DENTRY		'hb4 //'h7b4 // holds the address of the debug entry for self loop
`define TSELECT	'ha0 // 'h7a0 // holds the tselect information
`define TDATA1		'ha1 // 'h7a1 // holds the first trigger data
`define TDATA2		'ha2 // 'h7a2 // holds the first trigger data
////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////// funct3 defintions for ISA ////////////////////
`define JALR_f3 'b000
`define BEQ_f3	'b000
`define BNE_f3 	'b001
`define BLT_f3	'b100
`define BGE_f3	'b101
`define BLTU_f3	'b110
`define BGEU_f3	'b111
`define LB_f3		'b000
`define Lh_f3		'b001
`define LW_f3		'b010
`define LBU_f3	'b100
`define LHU_f3	'b101
`define LWU_f3	'b110
`define SB_f3		'b000
`define SH_f3		'b001
`define SW_f3		'b010
`define ADD_SUB_f3	'b000
`define SLT_SLTI_f3	'b010
`define SLTU_SLTIU_f3	'b011
`define XOR_XORI_f3	'b100
`define OR_ORI_f3	'b110
`define AND_ANDI_f3	'b111
`define SLL_SLLI_f3	'b001
`define SR_SRI_f3	'b101
`define ECALL_f3	'b000
`define EBREAK_f3	'b000
`define CSRRW_f3	'b001
`define CSRRS_f3	'b010
`define CSRRC_f3	'b011
`define CSRRWI_f3	'b101
`define CSRRSI_f3	'b110
`define CSRRCI_f3	'b111
`define MUL_f3		'b000
`define MULH_f3		'b001
`define MULHSU_f3	'b010
`define MULHU_f3	'b011
`define DIV_f3		'b100
`define DIVU_f3		'b101
`define REM_f3		'b110
`define REMU_f3		'b111
`define ATOMIC_f3	'b010
`define FENCE_f3	'b000
`define	FENCEI_f3	'b001
/////////////////////////////////////////////////////////////////////////
////////////////////// opcode definitions of ISA ////////////////////////
`define LUI_op				'b01101
`define AUIPC_op			'b00101
`define JAL_op				'b11011
`define JALR_op				'b11001
`define BRANCH_op			'b11000
`define LOAD_op				'b00000
`define FLOAD_op			'b00001
`define STORE_op			'b01000
`define FSTORE_op			'b01001
`define IMM_ARITH_op	'b00100
`define	ARITH_op			'b01100
`ifdef RV64
	`define IMM_ARITHW_op	'b00110
	`define	ARITHW_op			'b01110
	`define MULDIVW_op		'b01110
`endif
`define	CSR_op				'b11100
`define	MULDIV_op			'b01100
`define	ATOMIC_op			'b01011
`define	FMADD_op  		'b10000
`define	FMSUB_op			'b10001
`define	FNMSUB_op			'b10010
`define	FNMADD_op			'b10011
`define	FLOAT_op		'b10100
`define	FENCE_op			'b00011
//////////////////////////////////////////////////////////////////////////
/////////////// funct7 deifnition of ISA /////////////////////////////////
`define	SLLI_f7		'b0000000
`define	LOGIC_SHIFT_f7		'b0000000
`define	ARITH_SHIFT_f7		'b0100000
`define	ARITH_f7		'b0000000
`define	SUB_f7			'b0100000
`define	MULDIV_f7		'b0000001
`define	SFENCE_VMA	'b0001001
`define	LR_f5				'b00010
`define	SC_f5				'b00011
`define	AMOSWAP_f5	'b00001
`define	AMOADD_f5		'b00000
`define	AMOXOR_f5		'b00100
`define	AMOAND_f5		'b01100
`define	AMOOR_f5		'b01000
`define	AMOMIN_f5		'b10000
`define	AMOMAX_f5		'b10100
`define	AMOMINU_f5	'b11000
`define	AMOMAXU_f5	'b11100
`define	AMOMIN_f5	'b10000
`define	AMOMAX_f5	'b10100


`define FADD_f5         'b00000
`define FSUB_f5         'b00001
`define FMUL_f5         'b00010
`define FDIV_f5         'b00011
`define FSQRT_f5        'b01011
`define FP_OPCODE       'b0100
`define FCMP_f5         'b10100
`define FMMAX_f5        'b00101
`define FCVT_F_I_f5     'b11010
`define FCVT_I_F_f5     'b11000
`define FSGNJN_f5       'b00100
`define FCLASS_f5       'b11100
`define FCVT_S_D_f5     'b01000
`define FMV_X_S_f7      'b1110000
`define FMV_S_X_f7      'b1111000
`define FMV_X_D_f7      'b1110001
`define FMV_D_X_f7      'b1111001




///////////////////////////////////////////////////////////////////////////
///////////////// Event Values of Performance Counters ////////////////////
`define PERFMONITORS							64
`define	CYCLECOUNTERS						13
`define ICACHE_MISS							0
`define ICACHE_CACHEABLE	 				`ICACHE_MISS+1	
`define ICACHE_LINEREPLACE 				`ICACHE_CACHEABLE+1
`define ICACHE_TLBMISS						`ICACHE_LINEREPLACE+1
`define ICACHE_MISALIGNED					`ICACHE_TLBMISS+1
`define ICACHE_PREFETCHMISS				`ICACHE_MISALIGNED+1
`define COND_BRANCH							`ICACHE_PREFETCHMISS+1
`define COND_BRANCH_TAKEN					`COND_BRANCH+1
`define COND_BRANCH_MISPREDICTED			`COND_BRANCH_TAKEN+1
`define TAKEN_BRANCH_MISPREDICTED		`COND_BRANCH_MISPREDICTED+1
`define UNCOND_JUMPS							`TAKEN_BRANCH_MISPREDICTED+1
`define SPFPU_INST							`UNCOND_JUMPS+1
`define DPFPU_INST							`SPFPU_INST+1
`define DCACHE_TLBMISS						`DPFPU_INST+1
`define TOTAL_LOADS							`DCACHE_TLBMISS+1
`define TOTAL_STORES							`TOTAL_LOADS+1
`define TOTAL_ATOMIC							`TOTAL_STORES+1
`define DCACHE_LOAD_MISS					`TOTAL_ATOMIC+1
`define DCACHE_STORE_MISS					`DCACHE_LOAD_MISS+1
`define DCACHE_ATOMIC_MISS					`DCACHE_STORE_MISS+1
`define DCACHE_CACHEABLE_LOAD				`DCACHE_ATOMIC_MISS+1
`define DCACHE_CACHEABLE_STORE			`DCACHE_CACHEABLE_LOAD+1
`define DCACHE_CACHEABLE_ATOMIC			`DCACHE_CACHEABLE_STORE+1
`define DCACHE_WRITEBACKS					`DCACHE_CACHEABLE_ATOMIC+1
`define DCACHE_LINEREPLACE					`DCACHE_WRITEBACKS+1
`define DCACHE_MISALIGNED					`DCACHE_LINEREPLACE+1
`define EXCEPTIONS_TAKEN					`DCACHE_MISALIGNED+1
`define INTERRUPTS_TAKEN					`EXCEPTIONS_TAKEN+1
`define MULDIV_INSTRUCTIONS				`INTERRUPTS_TAKEN+1
`define MEMORY_INSTRUCTIONS				`MULDIV_INSTRUCTIONS+1
`define EXEC_FLUSHES							`MEMORY_INSTRUCTIONS+1
`define WB_FLUSHES							`EXEC_FLUSHES+1

`define USERMODE_CYCLES						30
`define SUPERVISORMODE_CYCLES				31
`define MACHINEMODE_CYLES					32
`define MISPREDICTION_STALLS				33
`define INTERRUPT_STALLS					34
`define DFENCE_CYCLES						35	
`define IFENCE_CYCLES						36
`define DCACHE_MISS_CYCLES					37
`define ICACHE_MISS_CYCLES					38
`define FPBUSY_CYCLES						39
`define DIVISIONBUSY_CYCLES				40
`define TOTAL_STALL_CYCLES					41
`define PAGEWALK_CYCLES						42
`define COREBUS_CYCLES						43

///////////////////////////////////////////////////////////////////////////////////
/*====== Define the slave number of each peripheral=== */
//`ifdef simulate
//	`define Sdram_slave_num					0 
//	`define Sdram_cfg_slave_num		`Sdram_slave_num 
//	`define BootRom_slave_num			`Sdram_cfg_slave_num+1
//	`define Uart0_slave_num				`BootRom_slave_num+1
//	`define Uart1_slave_num				`Uart0_slave_num+1
//	`define Debug_slave_num				`Uart1_slave_num+1
//	`define Plic_slave_num				`Debug_slave_num+1
//	`define Qspi0_slave_num          `Plic_slave_num 
//	`define Qspi1_slave_num          `Qspi0_slave_num
//	`define Num_Slaves					`Qspi1_slave_num
//`else
//	`define Uart0_slave_num           0
//	`define Uart1_slave_num           1
//	`define Qspi0_slave_num           2
//	`define Qspi1_slave_num           3
//	`define I2c0_slave_num				 4
//	`define I2c1_slave_num            5
//	`define Sdram_slave_num           6
//	`define Sdram_cfg_slave_num		 7
//	`define Dma_slave_num				 8
//	`define Hyperflash_mem_slave_num  9
//	`define Hyperflash_reg_slave_num  10
//	`define Debug_slave_num				11
//	`define AxiExp1_slave_num			12
//	`define GPIO_slave_num				13
//	`define BootRom_slave_num			14
//	`define PLIC_slave_num				15
//	`define TCM_slave_num				16
//`endif
/*=================================================== */

/*===== Define the base address of each peripheral === */
	`define DebugBase		'h00000000
	`define DebugEnd		'h000000FF
	`define BootRomBase  'h00001000
	`define BootRomEnd   'h00010FFF
	`define GPIOBase		'h00011100
	`define GPIOEnd		'h000111FF // 2 32-bit registers
	`define UART0Base		'h00011200
	`define UART0End		'h000112FF // 8 32-bit registers
	`define UART1Base		'h00011300
	`define UART1End		'h000113FF // 2 32-bit registers
	`define I2C0Base		'h00011400
	`define I2C0End		'h000114FF // 8 32-bit registers
	`define I2C1Base		'h00011500
	`define I2C1End		'h000115FF // 8 32-bit registers
	`define DMABase		'h00011600
	`define DMAEnd			'h000116FF // TODO
	`define SDRAMCfgBase	'h00011700
	`define SDRAMCfgEnd  'h000117FF // 12 32-bit registers
	`define QSPI0CfgBase	'h00011800
	`define QSPI0CfgEnd  'h000118FF // 13 32-bit registers
	`define QSPI1CfgBase	'h00011900
	`define QSPI1CfgEnd  'h000119FF // 13 32-bit registers
  `define PWMBase      'h00011A00
  `define PWMEnd       'h00011A0C // 4 32-bit registers
	`define TCMBase		'h00020000 // 
	`define TCMEnd			'h00040000 // 128KB
  `define VMEBase	'h40000000	
	`define  VMEEnd  'h4FFFFFFF // 1GB
    `ifdef FlexBus_verify
        `define FlexBusBase 'h80000000
        `define FlexBusEnd 'h8FFFFFFF
    `else
        `define FlexBusBase 'h50000000
        `define FlexBusEnd 'h5FFFFFFF
    `endif
	`define ClintBase		'h02000000
	`define ClintEnd		'h020BFFFF 
    `ifdef FlexBus_verify
	    `define SDRAMMemBase	'h50000000	
	    `define SDRAMMemEnd  'h5FFFFFFF // 1GB
    `else
	    `define SDRAMMemBase	'h80000000	
	    `define SDRAMMemEnd  'h8FFFFFFF // 1GB
    `endif
	`define QSPI0MemBase	'h90000000 
	`define QSPI0MemEnd  'h9FFFFFFF // 256 MB
	`define QSPI1MemBase	'hA0000000
	`define QSPI1MemEnd  'hAFFFFFFF // 256 MB
	`define PLICBase		'h0c000000
	`define PLICEnd		'h10000000
	`define AxiExp1Base	'hC0000000
	`define AxiExp1End	'hFFFFFFFF
/*=================================================== */
/*== Define the range of bytes per peripheral==== 
`ifdef simulate
	`define MemCRange		'h7FFFFFFF
	`define ConfigMRange	'h3000
	`define DebugRange		'h44  
`else
    `define BootRomRange       'hFFF         //4KB for Now
	 `define GPIORange			 'h8				//2 registers for GPIO
    `define UART0Range         'h38          //8 Registers
    `define UART1Range         'h38          //2 registers
    `define I2C0Range          'h38          //6 Registers. Adding +2 to just have a backup, if at all it might be required 
    `define I2C1Range          'h38          //8 registers
    `define DMARange           'hFF          
    `define SDRAMCfgRange      'h58          //12 registers
    `define QSPI0CfgRange      'h60          //13 registers
    `define QSPI1CfgRange      'h60          //13 registers
    `define HyperCfgRange      'h70          //15 registers
    `define SDRAMMemRange      'hFFFFFFF     //512 MB
    `define HyperMemRange      'hFFFFFFF     //512 MB
	 `define QSPI0MemRange	    'h7FFFFFFF    //2 GB
    `define QSPI1MemRange	    'h7FFFFFFF    //2 GB
    `define AxiExpRange			 'hFFFFFFFF    //512 MB for now!
    `define DebugRange         'h44          //16 Registers -- 32 bit
`endif
	/*=================================================== */
`define IONum 32

