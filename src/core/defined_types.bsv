/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author Names : Neel Gala, Arjun Menon
Email ID : neelgala@gmail.com

Description : 

This files contains all the types and structures that are used in any of the modules.
*/
package defined_types;

`include "defined_parameters.bsv"

	typedef enum {Load, Store, Atomic, Fence} Access_type deriving (Bits,Eq,FShow);
	typedef enum {User=2'd0,Supervisor=2'd1,Machine=2'd3} Privilege_mode deriving (Bits,Eq,FShow);
	typedef enum {Idle,Stall,Handling_Request,Handling_Memory_Read, Handling_Memory_Write, Handle_Fence} Cache_State deriving (Bits,Eq,FShow);
	typedef enum {Check_vd,Update_vd} Fence_state deriving (Bits,Eq);
	typedef enum {AccessFlush,Fence,None} Flush_type deriving (Bits,Eq,FShow);
	
	typedef union tagged{
		Bit#(width) Present;
		Bit#(4) Absent;
	} RFType#(numeric type width) deriving(Bits,Eq,FShow);

	typedef union tagged{
		void All;
		void None;
		Bit#(TLog#(`PRFDEPTH)) Specific;
	}PRFFlush deriving(Bits, Eq, FShow);
	
	typedef union tagged{
		void All;
		void None;
		Tuple2#(Register_type, Bit#(5)) Specific;
	}RFFlush deriving(Bits, Eq, FShow);

  
  typedef struct{
    Bit#(addr_width) address;
  }From_Cpu#(numeric type addr_width) deriving(Bits,Eq);
  
  typedef struct{
    Bit#(TMul#(word_size,8)) data_word;
    Bit#(1) bus_error;
    Bit#(1) misaligned_error;
    Bit#(addr_width) address;
  }To_Cpu#(numeric type addr_width,numeric type word_size) deriving(Bits,Eq);

  typedef struct{
    Bit#(addr_width) address;
    Bit#(8) burst_length; 
    Access_type ld_st;
	 Bit#(3) transfer_size;
  }To_Memory#(numeric type addr_width) deriving(Bits,Eq);

  typedef struct{
    Bit#(TMul#(word_size,8)) data_line;
    Bit#(1) bus_error;
	 Bool last_word;
  }From_Memory#(numeric type word_size) deriving(Bits,Eq);

  typedef struct{
    //Bit#(addr_width) address;
    Access_type load_store;
    Bit#(TMul#(word_size,8)) data;
    Bit#(TLog#(TDiv#(addr_width, 8))) transfer_size; // 0 -8 bits, 1- 16 bits, 2 -32 bits 3 - 64-bits;
	`ifdef atomic Bit#(5) atomic_op;`endif
	Bool signextend;
  }From_Cpu_D#(numeric type addr_width, numeric type word_size) deriving(Bits,Eq,FShow);


  typedef struct{
    Bit#(TMul#(8,TMul#(word_size,block_size))) line;
    Bit#(addr_width) address;
    Bit#(TLog#(ways)) replace_block;
  } Current_Store#(numeric type ways, numeric type addr_width, numeric type block_size, numeric type word_size) deriving(Bits,Eq);
  
  typedef struct{
    Bit#(TMul#(word_size,8)) data_word;
    Bit#(1) bus_error;
    Bit#(1) misaligned_error;
    Bit#(addr_width) address;
    Access_type load_store;
  }To_Cpu_D#(numeric type addr_width,numeric type word_size) deriving(Bits,Eq);

  typedef struct{
    Bit#(`PADDR) address;
    Bit#(8) burst_length; 
    Access_type ld_st;
    Bit#(TLog#(TDiv#(`VADDR, 8))) transfer_size; // 0 -8 bits, 1- 16 bits, 2 -32 bits 3 - 64-bits;
    Bit#(TMul#(`DCACHE_BLOCK_SIZE,TMul#(`DCACHE_WORD_SIZE,8))) data_line;
  } To_Memory_Write deriving(Bits,Eq);

  typedef struct{
    Bit#(TMul#(word_size,8)) data_line;
    Bit#(1) bus_error;
    Bit#(1) misaligned_error;
    Bit#(addr_width) address;
  }From_Memory_D#(numeric type addr_width,numeric type word_size, numeric type block_size) deriving(Bits,Eq);


typedef enum{
	Taken, Notaken
}Actual_jump deriving (Eq,Bits,FShow); // actual branch condition used in the branch execution unit.


// enum defining the prediction of the branch predictor for the current PC.
typedef enum{
	Predicted_taken,Predicted_notaken
}Prediction_type deriving (Eq,Bits,FShow); // output from the branch prediction unit.

// A typedef defining , if the prediction by the branch predictor was correct or wrong. 
typedef union tagged{
	Bit#(`VADDR) Mispredicted;
	Bit#(`VADDR) Correct_prediction;
}Prediction_result deriving (Eq,Bits,FShow); // result of prediuction from the branch execution unit.

typedef struct{
	Bit#(addr_width) prog_counter_;
  Prediction_type prediction_;
  Bool jump;
} Predictor_output#(numeric type addr_width) deriving(Bits, Eq);    // the program counter from the branch prediction unit.

typedef struct{
	Bit#(`Reg_width) data_forward;
	Bit#(TLog#(`PRFDEPTH)) pid;
}Operand_forwading_type deriving (Bits,Eq);	// the data structure for operand forwarding from any stage

typedef union tagged{
	Bit#(`Reg_width) Data;
	Bit#(TLog#(`PRFDEPTH)) Pid;
} FromRf deriving (Bits,Eq,FShow);

typedef struct{
	Bit#(`Reg_width) rs1;
	Bit#(`Reg_width) rs2;
  `ifdef spfpu Bit#(`Reg_width) rs3;`endif
} Output_for_operand_fetch deriving (Bits,Eq); // output from the register file to the decode stage

typedef enum {
	ALU,MUL,DIV,MEMORY,BRANCH,JAL,JALR `ifdef spfpu ,DFLOATING,FLOATING `endif ,FENCE,FENCEI,SYSTEM_INSTR,NOP
}Instruction_type deriving(Bits, Eq,FShow); // the type of the decoded instruction.

// to distuingish between integer and floating point RF
typedef enum {IntegerRF `ifdef spfpu ,FloatingRF `endif } Register_type deriving(Bits,Eq,FShow);
typedef enum {IntegerRF `ifdef spfpu ,FloatingRF `endif , Immediate, PC} Operand_type deriving(Bits,Eq,FShow);


typedef union tagged{
	void None;
	Bit#(`Reg_width) Address;
	Bit#(`Reg_width) Data;} TriggerType deriving(Bits,Eq,FShow);

typedef struct{
	TriggerType ttype;
	Bit#(4) matchscheme;} TriggerData deriving(Bits,Eq,FShow);

// the data stucture for the pipeline FIFO between fetch and decode.
typedef struct{
	Bit#(`VADDR) program_counter;
	Bit#(32) instruction;
	Trap_type exception;
	Bit#(`VADDR) nextpc;
	Bit#(2) prediction;
	Bit#(`PERFMONITORS) perfmonitors;
	Bit#(2) epochs;
}IF_ID_type deriving (Bits,Eq);

typedef struct{
	Bit#(`Reg_width) rs1;
	Bit#(`Reg_width) rs2;
	Bit#(5) rs1_addr;
	Bit#(12) csr_address;
	Bit#(3) funct3;
	}CSRInputs deriving(Bits,Eq,FShow);

typedef struct{
	Instruction_type inst_type;
	Operand_type rdtype;
	Bit#(`Reg_width) rs1;
	Bit#(`Reg_width) rs2;
	Bit#(`Reg_width) rs3_imm;
	Bit#(5) rs1addr;
	Bit#(5) rs2addr;
	Bit#(5) rs3addr;
	Operand_type rs1_type;
	Operand_type rs2_type;
	Operand_type rs3_type;
	Bit#(`VADDR) program_counter;
	Bool word32;
	Access_type mem_access;	
	Bit#(4) fn; // TODO Check is this can suffices for memaccess also
	Trap_type exception;
	Bit#(5) destination;
	Bit#(`VADDR) nextpc;
	Bit#(3) funct3;
	`ifdef spfpu Bool fcsr_rm; `endif
	`ifdef simulate Bit#(32) instruction ;`endif 
	Bit#(3) debugcause;
	Bit#(2) prediction;
	Bit#(`PERFMONITORS) perfmonitors;
	Bit#(2) epochs;
}ID_IE_type deriving (Bits,Eq);

typedef struct{
	Bit#(`Reg_width) address;
	Bit#(`Reg_width) memory_data; // data to be written in the memory
	Bit#(TLog#(TDiv#(`VADDR, 8))) transfer_size; // 0 -8 bits, 1- 16 bits, 2 -32 bits 3 - 64-bits;
	Bit#(1) signextend; // whether the loaded value has to be signextended
	Access_type mem_type; // STORE or AMO or LOAD or FENCE
	`ifdef atomic Bit#(5) atomic_op;`endif
	Bit#(1) epochs;
	}Memout deriving(Bits,Eq,FShow);

typedef union tagged{
	Arithout RESULT;	// 64+5
	Memout MEMORY;		// 64+64+3+1+3+5 = 140
	CSRInputs SYSTEM; // 64+64+5+12+3 = 148
	void Busy;
} Execution_output deriving(Bits,Eq,FShow);

//typedef struct{
//	Bit#(`Reg_width) result_addr_rs1;
//	Bit#(`Reg_width) data_rs2;
//	Bit#(20) csr_mem;
//} Execution_output deriving(Bits,Eq,FShow);

typedef struct{
	Execution_output execresult;
	Bit#(`VADDR) program_counter;
	Trap_type exception;
	Bit#(5) destination;
	Operand_type rd_type;
	Bit#(TLog#(`PRFDEPTH)) index;
	Bit#(4) pid;
  `ifdef simulate Bit#(32) instruction ;`endif 
	Bit#(3) debugcause;
	Bit#(`PERFMONITORS) perfmonitors;
	Bit#(2) epochs;
}IE_IMEM_type deriving (Bits,Eq);

typedef struct{
	Bit#(`Reg_width) aluresult;
	Bit#(5) fflags;
	} Arithout deriving(Bits,Eq,FShow); // output struct from the alu.

typedef struct{
	Bit#(`Reg_width) address;
	Bit#(3) transfer_size;
	Access_type load_store;
	}MemoryResponse deriving(Bits,Eq,FShow);

typedef union tagged{
	Arithout RESULT;
	CSRInputs SYSTEM;
} WriteBackType deriving(Bits,Eq);

typedef struct{
	WriteBackType commit_data;
	Bit#(5) destination;
	Operand_type rd_type;
	Bit#(TLog#(`PRFDEPTH)) index;
	Bit#(4) pid;
	Bit#(`VADDR) program_counter;
	Trap_type exception;
  `ifdef simulate Bit#(32) instruction ;`endif 
	Bit#(3) debugcause;
	Bit#(`PERFMONITORS) perfmonitors;
	Bit#(2) epochs;
}IMEM_IWB_type deriving(Bits,Eq);

typedef struct {
	Bit#(paddr) 			 address;
	Bit#(TLog#(TDiv#(paddr,8)))  transfer_size;	
	Bit#(1)					 u_signed;
	Bit#(3)						byte_offset;
	Bit#(TMul#(8, word_size)) write_data;
	Access_type			 ld_st;
} UncachedMemReq#(numeric type paddr, numeric type word_size) deriving(Bits, Eq);

/************************** Interfaces in PLIC ******************************/

interface Ifc_global_interrupt;
	method Action irq_frm_gateway(Bool ir);
endinterface

interface Ifc_program_registers#(numeric type addr_width,numeric type word_size);
	method ActionValue#(Bit#(TMul#(8,word_size))) prog_reg(UncachedMemReq#(addr_width, word_size) mem_req);
endinterface
/****************************************************************************/

typedef struct {
	Bit#(addr_width) pc;
	Bit#(addr_width) branch_address;
	Bit#(2) state;} Training_data#(numeric type addr_width) deriving (Bits, Eq);

typedef enum {SWAP,ADD,XOR,AND,OR,MINU,MAXU,MIN,MAX} Atomic_funct deriving(Bits,Eq,FShow);

typedef struct{
		Bit#(width) final_result;					// the final result for the operation
		Bit#(5) fflags; 					// indicates if any exception is generated.
	}Floating_output#(numeric type width) deriving(Bits,Eq);				// data structure of the output FIFO.

typedef enum {
	Inst_addr_misaligned=0,
	Inst_access_fault=1,
	Illegal_inst=2,
	Breakpoint=3,
	Load_addr_misaligned=4,
	Load_access_fault=5,
	Store_addr_misaligned=6,
	Store_access_fault=7,
	Ecall_from_user=8,
	Ecall_from_supervisor=9,
	Ecall_from_machine=11,
	Inst_pagefault=12,
	Load_pagefault=13,
	Store_pagefault=15
	`ifdef simulate ,Endsimulation =16 `endif
} Exception_cause deriving (Bits,Eq,FShow);

typedef struct {
	Bit#(TSub#(`VADDR,TAdd#(TLog#(`DCACHE_BLOCK_SIZE), TLog#(`DCACHE_WORD_SIZE)))) vtag;
	Bit#(`DCACHE_TAG_BITS) ptag;
	Bit#(TLog#(`DCACHE_WAYS)) writeblock;
	Bit#(1) dirty;
	Bit#(1) valid;
} Linebuff_tag deriving (Bits, Eq, FShow);
typedef enum{
	/*==== Standard =============== */
	User_soft_int=0,
	Supervisor_soft_int=1,
	Machine_soft_int=3,
	User_timer_int=4,
	Supervisor_timer_int=5,
	Machine_timer_int=7,
	User_external_int=8,
	Supervisor_external_int=9,
	Machine_external_int=11,
	/*=============================*/
	/*===== Non Standard========= */
	DebugInterrupt =12,
	DebugResume=13,
	DebugReset=14
//	Icache_miss 							=12,	
//	Icache_cacheable	 				=13,
//	Icache_linereplace 				=14,
//	Icache_tlbmiss 						=15,
//	Icache_misaligned 				=16,
//	Cond_branch 							=17,
//	Cond_branch_taken 				=18,
//	Cond_branch_mispredicted	=19,
//	Taken_branch_mispredicted	=20,
//	Uncond_jumps							=21,
//	Spfpu_inst								=22,
//	Dpfpu_inst								=23,
//	Dcache_tlbmiss						=24,
//	Total_loads								=25,
//	Total_stores							=26,
//	Total_atomic							=27,
//	Dcache_load_miss					=28,
//	Dcache_store_miss					=29,
//	Dcache_atomic_miss				=30,
//	Dcache_cacheable_load			=31,
//	Dcache_cacheable_store		=32,
//	Dcache_cacheable_atomic		=33,
//	Dcache_writebacks					=34,
//	Dcache_linereplace				=35,
//	Dcache_misaligned					=36,
//	Exceptions_taken					=37,
//	Interrupts_taken					=38,
//	Muldiv_instructions				=39,
//	System_instructions				=40,
//	Usermode_cycles						=41,
//	Supervisormode_cycles			=42,
//	Machinemode_cyles					=43,
//	Misprediction_stalls			=44,
//	Interrupt_stalls					=45,
//	Dfence_cycles							=46,
//	Ifence_cycles             =47,
//	Dcache_miss_cycles        =48,
//	Icache_miss_cycles        =49,
//	Fpbusy_cycles             =50,
//	Divisionbusy_cycles				=51,
//	Total_stall_cycles				=52,
//	Pagewalk_cycles						=53,
//	Corebus_cycles						=54
} Interrupt_cause deriving (Bits,Eq,FShow);

typedef union tagged{
  Exception_cause Exception;
  Interrupt_cause Interrupt;
  void None;
} Trap_type deriving(Bits,Eq,FShow);

function String event_name(Bit#(64) eventnum);
	case (eventnum)
		'h0000000000000001: return 	"ICACHE_MISS 						";
		'h0000000000000002: return 	"ICACHE_CACHEABLE	 				";
		'h0000000000000004: return 	"ICACHE_LINEREPLACE 				";
		'h0000000000000008: return 	"ICACHE_TLBMISS 					";
		'h0000000000000010: return 	"ICACHE_MISALIGNED					";
		'h0000000000000020: return 	"ICACHE_PREFETCHMISS				";
		'h0000000000000040: return 	"COND_BRANCH 						";
		'h0000000000000080: return 	"COND_BRANCH_TAKEN					";
		'h0000000000000100: return 	"COND_BRANCH_MISPREDICTED		";
		'h0000000000000200: return		"TAKEN_BRANCH_MISPREDICTED		";
		'h0000000000000400: return		"UNCOND_JUMPS						";
		'h0000000000000800: return		"SPFPU_INST							";
		'h0000000000001000: return		"DPFPU_INST							";
		'h0000000000002000: return		"DCACHE_TLBMISS						";
		'h0000000000004000: return		"TOTAL_LOADS							";
		'h0000000000008000: return		"TOTAL_STORES						";
		'h0000000000010000: return		"TOTAL_ATOMIC						";
		'h0000000000020000: return		"DCACHE_LOAD_MISS					";
		'h0000000000040000: return		"DCACHE_STORE_MISS					";
		'h0000000000080000: return		"DCACHE_ATOMIC_MISS				";
		'h0000000000100000: return		"DCACHE_CACHEABLE_LOAD			";
		'h0000000000200000: return		"DCACHE_CACHEABLE_STORE			";
		'h0000000000400000: return		"DCACHE_CACHEABLE_ATOMIC			";
		'h0000000000800000: return		"DCACHE_WRITEBACKS					";
		'h0000000001000000: return		"DCACHE_LINEREPLACE				";
		'h0000000002000000: return		"DCACHE_MISALIGNED					";
		'h0000000004000000: return		"EXCEPTIONS_TAKEN					";
		'h0000000008000000: return		"INTERRUPTS_TAKEN					";
		'h0000000010000000: return		"MULDIV_INSTRUCTIONS				";
		'h0000000020000000: return 	"MEMORY_INSTRUCTIONS				";
		'h0000000040000000: return 	"EXEC_FLUSHES						";
		'h0000000080000000: return 	"WB_FLUSHES							";
		default: return "NO EVENT";
	endcase
endfunction

/****************************** MMU TYPES *******************************/

typedef struct {
	bit			mprv;
	bit			sum;
	bit			mxr;
	Privilege_mode mpp;
	Privilege_mode prv;
} Chmod deriving(Bits, Eq);
	
typedef struct {
	bit v;					//valid
	bit r;					//allow reads
	bit w;					//allow writes
	bit x;					//allow execute(instruction read)
	bit u;					//allow supervisor
	bit g;					//global page
	bit a;					//accessed already
	bit d;					//dirty
} TLB_permissions deriving(Bits, Eq, FShow);

typedef struct {
	Bit#(TSub#(paddr,page_size)) ppn;
	TLB_permissions 				tlb_perm;
	Bit#(asid_width)						asid;
	Bit#(2)										levels;
} To_TLB#(numeric type paddr, numeric type page_size, numeric type asid_width) deriving(Bits,Eq); 
	
typedef struct {
	Bit#(data_width) vaddr;
	Access_type  ld_st_atomic;
} DTLB_access#(numeric type data_width) deriving(Bits, Eq);

typedef enum {
		PTW_ready, Handling_PTW, Wait_for_memory, PTW_done, Send_to_memory} PTW_state deriving(Bits, Eq);
	
typedef enum {
		Load, Store, Execution} Translation_type deriving(Bits, Eq);

typedef struct {
	Translation_type 	page_type;
	Bit#(TSub#(vaddr_width,page_offset)) 	vpn;
} Request_PPN_PTW#(numeric type vaddr_width, numeric type page_offset) deriving (Bits,Eq);

typedef struct {
	Translation_type 	page_type;
	To_TLB#(paddr_width,page_offset,asid_width) 	tlb_packet;
} Response_PPN_TLB#(numeric type paddr_width, numeric type page_offset, numeric type asid_width) deriving (Bits,Eq);

typedef struct {
	Bool ptwdone;
	Translation_type 	page_type;
	Bit#(data_width) 	address;
} Request_PTE_memory#(numeric type data_width) deriving (Bits,Eq);

typedef struct {
	Trap_type exception;
	Bit#(data_width) address;	
	Bool 						 cacheable;
} From_TLB#(numeric type data_width) deriving (Bits, Eq);

typedef struct {
	Bit#(vaddr_width) rs1;
	Bit#(vaddr_width) rs2;
} Fence_VMA_type#(numeric type vaddr_width) deriving (Bits, Eq);

typedef enum {
	Store_pf, Load_pf, Instruction_pf, None} Pf_exception_type deriving (Bits, Eq);	
/*=============================================================================== */

/* =============================== Debug related types ========================== */
typedef enum {CPU_CONTINUE,CPU_STOPPED} CPU_State deriving(Bits,Eq,FShow);
typedef enum {
	      GDB_INTERRUPT,
	      GDB_HUP,
	      GDB_INT,
	      GDB_QUIT,
	      GDB_ILL,
	      GDB_BREAK = 5,
              CPU_BUSY
   } GdbStopCondition
deriving (Bits ,Eq, FShow);

/*======= AXI4 master/slave numbers ======*/
typedef 0 Sdram_slave_num;
typedef  TAdd#(Sdram_slave_num	 ,`ifdef SDRAM		1 `else 0 `endif )		Sdram_cfg_slave_num;
typedef	TAdd#(Sdram_cfg_slave_num,`ifdef BOOTROM	1 `else 0 `endif )		BootRom_slave_num	;
typedef	TAdd#(BootRom_slave_num  ,`ifdef Debug		1 `else 0 `endif )		Debug_slave_num	;
typedef  TAdd#(Debug_slave_num	 , `ifdef TCMemory	1 `else 0 `endif )		TCM_slave_num;
typedef  TAdd#(TCM_slave_num	 ,`ifdef DMA			1 `else 0 `endif )	Dma_slave_num;
typedef  TAdd#(Dma_slave_num	  ,1 )		SlowPeripheral_slave_num;
typedef  TAdd#(SlowPeripheral_slave_num,`ifdef VME	1 `else 0 `endif )       VME_slave_num;
typedef  TAdd#(VME_slave_num,`ifdef FlexBus	1 `else 0 `endif )              FlexBus_slave_num;
typedef	TAdd#(FlexBus_slave_num,1)						 Num_Slaves;
typedef 0 Dmem_master_num;
typedef 1 Imem_master_num;
typedef TAdd#(Imem_master_num , `ifdef Debug 1 `else 0 `endif ) Debug_master_num;
typedef TAdd#(Debug_master_num, `ifdef DMA 1 `else 0 `endif ) DMA_master_num;
typedef TAdd#(DMA_master_num,1) Num_Masters;

/*=============================================================================== */
/*====== AXI4 Lite slave declarations =======*/
typedef  0	SlowMaster; 
typedef	0	Uart0_slave_num	;
typedef	 TAdd#(Uart0_slave_num		,`ifdef UART1 		1 `else 0 `endif )		Uart1_slave_num	;
typedef  TAdd#(Uart1_slave_num		,`ifdef CLINT		1 `else 0 `endif )		CLINT_slave_num;
typedef	 TAdd#(CLINT_slave_num		,`ifdef PLIC		1 `else 0 `endif )		Plic_slave_num		;
typedef  TAdd#(Plic_slave_num			,`ifdef PLIC 		1 `else 0 `endif )		GPIO_slave_num   ;
typedef  TAdd#(GPIO_slave_num			,`ifdef I2C0 		1 `else 0 `endif )		I2c0_slave_num   ;
typedef  TAdd#(I2c0_slave_num			,`ifdef I2C1 		1 `else 0 `endif )		I2c1_slave_num   ;
typedef  TAdd#(I2c1_slave_num			,`ifdef QSPI0 		1 `else 0 `endif )		Qspi0_slave_num   ;
typedef  TAdd#(Qspi0_slave_num		,`ifdef QSPI1 		1 `else 0 `endif )		Qspi1_slave_num   ;
typedef  TAdd#(Qspi1_slave_num		,`ifdef AXIEXP		1 `else 0 `endif )		AxiExp1_slave_num;
typedef  TAdd#(AxiExp1_slave_num  ,`ifdef PWM_AXI4Lite 1 `else 0 `endif ) Pwm_slave_num;
typedef	TAdd#(Pwm_slave_num,1)									Num_Slow_Slaves			;
/*===========================================*/

endpackage
