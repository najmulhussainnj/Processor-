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
package decoder; 
   `include "defined_parameters.bsv" 
   import defined_types::*; 
 
   typedef struct { 
      Bit#(4) fn;  
      Bit#(5) rs1;  
      Bit#(5) rs2;  
      Bit#(5) rs3;  
      Bit#(5) rd;  
      Operand_type rs1type;  
      Operand_type rs2type; 
      Operand_type rdtype; 
      Instruction_type inst_type; 
      Bit#(`Reg_width) immediate_value; 
      Bool word32; 
      Access_type mem_access;    
      Trap_type exception; 
      Bit#(3) funct3; 
      Bit#(`PERFMONITORS) perf;  
      } Decoded_data deriving(Bits,Eq,FShow); 
 
(*noinline*) 
function Decoded_data fn_decode(Bit#(32) instruction, Trap_type exception, Bit#(`Reg_width) misa, Bit#(`PERFMONITORS) perfmonitors); 
   Bit#(5) rs1=instruction[19:15]; 
   Bit#(5) rs2=instruction[24:20]; 
   Bit#(5) rs3=instruction[31:27]; 
   Bit#(5) rd=instruction[11:7]; 
   Bit#(5) opcode = instruction[6:2]; 
    Bit#(7) funct7 = instruction[31:25]; 
   Bit#(3) funct3 = instruction[14:12]; 
   Bool word32 =False; 
 
   Access_type mem_access=Load; 
	`ifdef atomic
	   if(opcode[3]=='b1 && opcode[1]=='b1) 
		   mem_access=Atomic; 
		else 
	`endif
	if(opcode[3]=='b1 && opcode[1]==0) 
      mem_access=Store; 
 
   Operand_type rs1type=IntegerRF; 
   Operand_type rs2type=IntegerRF; 
   Operand_type rdtype=IntegerRF; 
    
   Bit#(`Reg_width) immediate_value=signExtend(instruction[31:20]); 
   if(opcode==`LUI_op|| opcode==`AUIPC_op) 
      immediate_value=signExtend({instruction[31:12],12'd0}); 
   else if(opcode==`JAL_op) 
      immediate_value=signExtend({instruction[31],instruction[19:12],instruction[20],instruction[30:21],1'b0}); 
   else if(opcode==`JALR_op) 
      immediate_value=signExtend({instruction[31:21],1'b0}); 
   else if (opcode==`BRANCH_op) // Branch instructions 
      immediate_value=signExtend({instruction[31],instruction[7],instruction[30:25],instruction[11:8],1'b0}); 
   else if (opcode==`STORE_op `ifdef spfpu || opcode==`FSTORE_op `endif ) // Store operations 
      immediate_value=signExtend({instruction[31:25],instruction[11:7]}); 
   else if(opcode==`CSR_op) 
      immediate_value[16:12]=instruction[19:15]; 
	else if(opcode==`ATOMIC_op)
		immediate_value=0;
 
   if(opcode==`LUI_op || opcode==`JAL_op || opcode==`AUIPC_op || (opcode==`CSR_op && funct3[2]==1)) 
      rs1=0; 
   if(opcode==`CSR_op || opcode[4:2]=='b000                           // CSR or ( (F)Load or FENCE ) 
      || opcode==`LUI_op || opcode==`JAL_op || opcode[4:2]=='b001      // LUI or JAL or (AUIPC or IMMediate Arith) 
      || opcode==`JALR_op || (opcode[4:2]=='b101 && funct7[5]==1))   // JALR or Floating conversion operations. 
      rs2=0; 
   if(opcode==`BRANCH_op || opcode[4:1]=='b0100) 
      rd=0; 
 
   if(opcode==`JAL_op || opcode==`AUIPC_op) 
      rs1type=PC; 
`ifdef spfpu
	   else if(opcode[4:2]=='b100 || (opcode[4:2]=='b101 &&               // (F(N)MADD or F(N)SUB)  
		   (funct7[6:3]!='b1101 && funct7[6:3]!='b1111)))                  // some of the conversion operations 
			rs1type=FloatingRF; 
`endif
 
   if(opcode==`JAL_op || opcode==`JALR_op || opcode==`LUI_op|| opcode[4:2]=='b001 // JAL or JALR or (AUIPC or IMM Arith) 
      || opcode[4:1]==0)                                                // (F)Load or  
      rs2type=Immediate; 
`ifdef spfpu
   else if((opcode[4:2]=='b101 && funct7[5]!='b1) || opcode==`FSTORE_op || opcode[4:2]=='b100)                  // All convert + FSQRToperations do not need rs2 
	   rs2type=FloatingRF; 
 
   if(opcode==`FLOAD_op || (opcode[4:2]=='b101 &&  
         funct7[6:3]!='b1010 && funct7[6:3]!='b1100 && funct7[6:3]!='b1110 ) || opcode[4:2]=='b100) 
      rdtype=FloatingRF; 
`endif
    
   if(opcode==`IMM_ARITHW_op || opcode==`MULDIVW_op || opcode==`ARITHW_op || (opcode[4:3]=='b10 && funct7[0]==0) 
         || (opcode[4:1]=='b0101 && funct3[0]==0)) 
      word32=True; 
    
   Instruction_type inst_type=NOP; 
	`ifdef spfpu
   if(opcode[4:3]=='b10)begin 
      inst_type=funct7[0]==0?FLOATING:DFLOATING; 
   end 
   else `endif
	if(opcode[4:3]=='b11)begin 
      case (opcode[2:0]) 
         'b011:inst_type=JAL; 
         'b001:inst_type=JALR; 
         'b000:inst_type=BRANCH; 
         'b100:inst_type=SYSTEM_INSTR; 
      endcase 
   end 
   else if(opcode[4:3]=='b01)begin 
      case (opcode[2:0])  
         'b000,'b011,'b001:inst_type=MEMORY; // STORE or FSTORE or ATOMIC 
         'b101:inst_type=ALU;      // LUI 
         'b100,'b110:inst_type=(funct7[0]==1)?(funct3[2]==0)?MUL:DIV:ALU; 
      endcase 
   end 
   else if(opcode[4:3]=='b00)begin 
      case(opcode[2:0]) 
         'b000,'b001:inst_type=MEMORY;   // 
         'b101,'b100,'b110:inst_type=ALU;      //AUIPC IMM WORD 
         'b011:inst_type=(funct3[0]==0)?FENCE:FENCEI; 
      endcase 
   end 
    
   Trap_type ex=tagged None; 
   if(exception matches tagged None)begin 
      if( `ifdef spfpu (inst_type==FLOATING && misa[5]==0) `ifdef dpfpu || (inst_type==DFLOATING && misa[3]==0)  `endif || `endif
          (inst_type==MUL && misa[12]==0)    || (inst_type==DIV && misa[12]==0) 
            `ifdef atomic || (inst_type==MEMORY && mem_access==Atomic && misa[0]==0) `endif ) 
         ex=tagged Exception Illegal_inst; 
	`ifdef simulate
		if(inst_type==JAL && immediate_value==0)
			ex=tagged Exception Endsimulation;
	`endif
		if(instruction[1:0]!='b11)
			ex=tagged Exception Illegal_inst;
		if(inst_type==NOP)
			ex=tagged Exception Illegal_inst;
   end 
	else 
	   ex=exception; 
		
		Bit#(4) fn=0;
		if(opcode==`ATOMIC_op)begin
			if((instruction[27] | instruction[28]) == 1)
				fn={instruction[29:27],1'b1};
			else
				fn={instruction[31:29],instruction[27]};	
		end
		else if(opcode==`BRANCH_op)begin
			if(funct3[2]==0)
				fn={2'b0,1,funct3[0]};
			else
				fn={1'b1,funct3};
		end
		else if(opcode==`JAL_op || opcode==`JALR_op || opcode==`LOAD_op `ifdef spfpu || opcode==`FLOAD_op `endif
				|| opcode==`STORE_op `ifdef spfpu || opcode==`FSTORE_op `endif || opcode==`AUIPC_op || opcode==`LUI_op)
			fn=0;
		else if(opcode==`IMM_ARITHW_op || opcode==`IMM_ARITH_op)begin
			fn=case(funct3)
				'b010: 'b1100;
				'b011: 'b1110;
				'b101: if(funct7[5]==1) 'b1011; else 'b0101;
				default:{1'b0,funct3};
			endcase;
		end
		else if(opcode==`ARITHW_op || opcode==`ARITH_op)begin
			fn=case(funct3)
				'b000:if(funct7[5]==1) 'b1010; else 'b0000;
				'b010:'b1100;
				'b011:'b1110;
				'b101:if (funct7[5]==1) 'b1011;else 'b0101;
				default:{1'b0,funct3};
			endcase;
		end
		else if(opcode[4:3]=='b10)
			fn=opcode[3:0];
	if(inst_type==BRANCH)
		perfmonitors[`COND_BRANCH]=1;
	`ifdef spfpu
		if(inst_type==FLOATING)
			perfmonitors[`SPFPU_INST]=1;
	`endif
	`ifdef dpfpu
	if(inst_type==DFLOATING)
		perfmonitors[`DPFPU_INST]=1;
	`endif
	if(inst_type==JAL || inst_type==JALR)
		perfmonitors[`UNCOND_JUMPS]=1;
	if(inst_type==MEMORY)
		perfmonitors[`MEMORY_INSTRUCTIONS]=1;
	if(inst_type==MUL || inst_type==DIV)
		perfmonitors[`MULDIV_INSTRUCTIONS]=1;
    
   return (Decoded_data{fn:fn,rs1:rs1,rs2:rs2,rs3:rs3,rd:rd, 
                        rs1type:rs1type,rs2type:rs2type,rdtype:rdtype, 
                        inst_type:inst_type,immediate_value:immediate_value, 
                        word32:word32,mem_access:mem_access,exception:ex, 
                        funct3:funct3,perf:perfmonitors}); 
			
endfunction        
endpackage        
