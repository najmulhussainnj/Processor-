package plic;
	import Vector::*;
	import defined_types::*;
	import ConfigReg::*;
	import Semi_FIFOF::*;
	import AXI4_Types::*;
	import BUtils ::*;
	import ConcatReg ::*;
	`include "defined_parameters.bsv"
  //	import ConfigReg::*;
/*Platform level interrupt controller:
	Refer to RISC-V privilege spec-v-1.10 chapter 7
	Memory maps of Registers
		Interrupt enable registers :
							rg_ie_0 :0C002000 
							rg_ie_1 :0C002000 
							rg_ie_2 :0C002000 
							.
							.
							.
							rg_ie_7 :0C002000 
							rg_ie_8 :0C002001 
							rg_ie_9 :0C002001 
							.
							.
		Interrupt priority registers :
							rg_priority_0 : 0C000000
							rg_priority_1 : 0C000002
							rg_priority_2 : 0C000004
							.
							.
							.
		Priority Threshold register : 
							rg_priority_threshold : 0C200000
		Claim register : 
							rg_interrupt_id : 0C200004
*/


interface Ifc_PLIC#(numeric type addr_width,numeric type word_size,numeric type no_of_ir_pins);
	interface Vector#(no_of_ir_pins,Ifc_global_interrupt) ifc_external_irq;
	interface Ifc_program_registers#(addr_width,word_size) ifc_prog_reg;
	method ActionValue#(Tuple2#(Bool,Bool)) intrpt_note;
	method ActionValue#(Bit#(TLog#(no_of_ir_pins))) intrpt_completion;
endinterface

//(*conflict_free = "rl_prioritise, prog_reg"*)
module mkplic(Ifc_PLIC#(addr_width,word_size,no_of_ir_pins))
	provisos(
	Log#(no_of_ir_pins, priority_bits),
	Mul#(8,word_size,data_width),
	Add#(1,priority_bits,x_priority_bits),
	Add#(msb_priority,1,priority_bits),
	Add#(msb_priority_bits,1,no_of_ir_pins),
	Add#(b__, no_of_ir_pins, data_width),
	Add#(c__, priority_bits, data_width),
	Add#(a__, 8, no_of_ir_pins),
	Add#(e__, 32, data_width),
	Add#(g__, 32, no_of_ir_pins),
	Add#(f__, 3, priority_bits),
	Add#(d__, 5, priority_bits)
	);
	let v_no_of_ir_pins = valueOf(no_of_ir_pins);
	let v_priority_bits = valueOf(priority_bits);
	let v_msb_priority_bits = valueOf(msb_priority_bits);
	let v_msb_priority = valueOf(msb_priority);
	let v_data_width = valueOf(data_width);

	//(* noinline *)

	/* This function defines the working of priority encoder with 4 bit inputs */
	function Bit#(8) priority_encoder(Bit#(8) inp, Bool alu_free);
	   Bit#(8) outp = 0;
	   if(alu_free) begin
		if(inp[0]==1)
		   outp[0] = 1'b1;
		else if(inp[1]==1)
		   outp[1] = 1'b1;
		else if(inp[2]==1)
		   outp[2] = 1'b1;
		else if(inp[3]==1)
		   outp[3] = 1'b1;
		else if(inp[4]==1)
		   outp[4] = 1'b1;
		else if(inp[5]==1)
		   outp[5] = 1'b1;
		else if(inp[6]==1)
		   outp[6] = 1'b1;
		else if(inp[7]==1)
		   outp[7] = 1'b1;
	   end
	
	   return outp;
	endfunction
	
	function bit any_req(Bit#(8) inp);
	   return inp[0] | inp[1] | inp[2] | inp[3] | inp[4] | inp[5] | inp[6] | inp[7];
	endfunction
	
	/* Encodes the grant vector */
	function Bit#(x_priority_bits) encoder(Bit#(no_of_ir_pins) inp);
	   Bit#(priority_bits) outp = 0;
	   bit outp_valid = 1'b1;
	   for(Integer i = 0; i < v_no_of_ir_pins; i = i+1) begin
			if(inp[i]==1)
				outp = fromInteger(i);
	   end
	   return {outp_valid,outp};
	endfunction
	
  function Reg#(t) readOnlyReg(t r);
    return (interface Reg;
       method t _read = r;
       method Action _write(t x) = noAction;
    endinterface);
  endfunction

	/* Request vectors are passed down the tree and the grants are given back */
	function Bit#(no_of_ir_pins) encoder_tree(Bit#(no_of_ir_pins) inp);
		Bit#(no_of_ir_pins) outp = 0;
		//request to root
		Bit#(8) root_reqs;
		
		//grant from root
		Bit#(8) root_grants;
		
		for(Integer i=0;i<8;i=i+1)
		   root_reqs[i] = any_req(inp[8*fromInteger(i)+7:8*fromInteger(i)]);
		
		root_grants = priority_encoder(root_reqs, True);
		
		//grants are passed back to leaves
		for(Integer i=0;i<8;i=i+1)
			outp[8*fromInteger(i)+7:8*fromInteger(i)] = priority_encoder(inp[8*fromInteger(i)+7:8*fromInteger(i)], unpack(root_grants[i]));
		return outp;
	endfunction			

	Vector#(no_of_ir_pins,Array#(Reg#(Bool))) rg_ip <- replicateM(mkCReg(2,False));
	Reg#(Bool) rg_ie[v_no_of_ir_pins];
	for(Integer i = 0; i < v_no_of_ir_pins;i=i+1)
		if(i==28 || i == 29)
			rg_ie[i] = readOnlyReg(True); 
		else
			rg_ie[i] <- mkReg(False); 
	Reg#(Bit#(32)) rg_priority_low[v_no_of_ir_pins];
	for(Integer i =0; i < v_no_of_ir_pins; i=i+1)
		if(i==28 || i == 29)
			rg_priority_low[i] = readOnlyReg(32'h00000001);
		else
			rg_priority_low[i] <- mkConfigReg(0);
	Reg#(Bit#(no_of_ir_pins)) rg_priority[v_no_of_ir_pins];
	for(Integer i=0;i < v_no_of_ir_pins;i=i+1)
		rg_priority[i] = concatReg2(readOnlyReg(0), rg_priority_low[i]);
	Reg#(Bit#(5)) rg_priority_threshold_low <- mkReg(0);
	Reg#(Bit#(priority_bits))	 rg_priority_threshold = concatReg2(readOnlyReg(0),rg_priority_threshold_low);
	Reg#(Bit#(priority_bits))	 rg_interrupt_id <- mkConfigReg(0);
	Reg#(Bool)	 rg_interrupt_valid <- mkConfigReg(False);
	Reg#(Maybe#(Bit#(priority_bits))) rg_completion_id <- mkReg(tagged Invalid);

	rule rl_prioritise;
		Bit#(priority_bits) winner_priority = 0;
		Bit#(priority_bits) winner_interrupts = 0;
		Bit#(x_priority_bits) ir_id_valid = 0;
		Bit#(no_of_ir_pins) lv_priority = 0;
		Bit#(no_of_ir_pins) lv_total_priority = 0;
		for(Integer i = 0; i < v_no_of_ir_pins; i = i + 1)
		 begin
			
			if(rg_ip[i][1] && rg_ie[i]) begin
				lv_priority = lv_priority | rg_priority[i];
				winner_interrupts = fromInteger(i);
				`ifdef verbose $display($time,"\tInterrupt id %d and priority is %d", i, lv_priority);`endif
			end
		end
		winner_priority = encoder(encoder_tree(lv_priority))[v_msb_priority:0];
		`ifdef verbose $display($time,"\t winner priority is  %d", winner_priority);`endif
		for(Integer i = 0; i < v_no_of_ir_pins; i = i + 1) begin
			if(rg_priority[i][winner_priority] == 1 && rg_ip[i][1] && rg_ie[i])
				lv_total_priority[i] = 1;
		end
		if(lv_total_priority!=0)
		winner_interrupts = encoder(encoder_tree(lv_total_priority))[v_msb_priority:0];
		if(winner_interrupts!=0) begin
			ir_id_valid = encoder(rg_priority[winner_interrupts]);
			if(winner_priority <= rg_priority_threshold)
			 begin
				
				`ifdef verbose $display("Interrupt valid");`endif
				rg_interrupt_id <= winner_interrupts;
				rg_interrupt_valid <= True;
				$display($time,"\t The highest priority interrupt is  %d and the priority is ", winner_interrupts, winner_priority);
			end
		end
	endrule

	Vector#(no_of_ir_pins, Ifc_global_interrupt) temp_ifc_irq;

	for(Integer i = 0; i < v_no_of_ir_pins; i = i + 1) begin

		temp_ifc_irq[i] = interface Ifc_global_interrupt

							method Action irq_frm_gateway(Bool ir);
								`ifdef verbose $display("Interrupt id %d is pending", i);`endif
								rg_ip[i][0] <= True;
							endmethod

						  endinterface;
	end

	interface ifc_external_irq = temp_ifc_irq;

interface ifc_prog_reg = interface Ifc_program_registers;

							method ActionValue#(Bit#(data_width)) prog_reg(UncachedMemReq#(addr_width, word_size) mem_req);
								//update memory mapped registers
								`ifdef verbose $display($time,"\tPLIC : programming registers for address %h", mem_req.address);`endif
								let address = mem_req.address;
								Bit#(priority_bits) source_id=0;
								Bit#(data_width) data_return = 0;
								if(address < 'h0C001000) begin
									address = address >> 2;
									if(mem_req.ld_st == Load) begin
										source_id = address[v_msb_priority:0];
										`ifdef verbose $display($time,"\tPLIC : source %d Priority set to %h", source_id, mem_req.write_data);`endif
										data_return = zeroExtend(rg_priority[source_id]);
									end
									else if(mem_req.ld_st == Store) begin
										Bit#(no_of_ir_pins) store_data;
										if(mem_req.byte_offset==0)
											store_data=mem_req.write_data[v_msb_priority_bits:0];
										else
											store_data=mem_req.write_data[v_data_width-1:v_data_width-v_no_of_ir_pins];
										mem_req.byte_offset = mem_req.byte_offset >> 2;
										source_id = address[v_msb_priority:0] | zeroExtend(mem_req.byte_offset);
										$display($time,"\tPLIC : source %d Priority set to %h", source_id, store_data);
										rg_priority[source_id] <= store_data;
									end
								end
								else if(address < 'h0C002000) begin
									if(mem_req.ld_st == Load) begin
										source_id = address[v_msb_priority:0];
										source_id = source_id << 3;
										for(Integer i = 0; i < 8; i = i+1)
											data_return[i] = pack(rg_ip[source_id + fromInteger(i)][1]);
									end
									else if(mem_req.ld_st == Store) begin
										source_id = zeroExtend(mem_req.byte_offset);
										source_id = source_id << 3;
										for(Integer i = 0; i < 8; i = i+1) begin
											`ifdef verbose $display($time,"\tPLIC : pending interrupt  %b id %d", mem_req.write_data[i], source_id);`endif
											rg_ip[source_id + fromInteger(i)][1] <= unpack(mem_req.write_data[i]); 
										end
									end
								end
								else if(address < 'h0C020000) begin
									if(mem_req.ld_st == Load) begin
										source_id = address[v_msb_priority:0];
										source_id = source_id << 3;
										for(Integer i = 0; i < 8; i = i+1)
											data_return[i] = pack(rg_ie[source_id + fromInteger(i)]);
                                            `ifdef verbose $display($time,"PLIC: Printing Source Enable Interrupt: %h data_return: %h",source_id,data_return); `endif
									end
									else if(mem_req.ld_st == Store) begin
										source_id = zeroExtend(mem_req.byte_offset);
										source_id = source_id << 3;
										for(Integer i = 0; i < 8; i = i+1) begin
											`ifdef verbose $display($time,"\tPLIC : enabled interrupt  %b id %d", mem_req.write_data[i], source_id);`endif
											rg_ie[source_id + fromInteger(i)] <= unpack(mem_req.write_data[i]); 
										end
									end
								end
								else if(address == 'hC200000) begin
									if(mem_req.ld_st == Load) begin
										data_return = zeroExtend(rg_priority_threshold); 
									end
									else if(mem_req.ld_st == Store)
										rg_priority_threshold <= mem_req.write_data[v_msb_priority:0];
								end
								else if(address == 'hC200004) begin
									if(mem_req.ld_st == Load) begin
										data_return = zeroExtend(rg_interrupt_id); 
										rg_ip[rg_interrupt_id][1] <= False;
                                       `ifdef verbose $display($time,"rg_ip is made false here"); `endif
									end
									else if(mem_req.ld_st == Store) begin
										source_id = mem_req.write_data[v_msb_priority:0];
										rg_completion_id <= tagged Valid source_id;
                                        `ifdef verbose $display("rg_completion_id is made tagged valid and completion is signaled-- source_id: %d",source_id); `endif
									end
								end
								return data_return;
							endmethod

						endinterface;

							method ActionValue#(Bit#(TLog#(no_of_ir_pins))) intrpt_completion if(isValid(rg_completion_id)); 
								let completion_msg = validValue(rg_completion_id);
								rg_completion_id <= tagged Invalid;
                                `ifdef verbose $display("Sending Completion to SoC"); `endif
								return completion_msg;
							endmethod

							method ActionValue#(Tuple2#(Bool,Bool)) intrpt_note;
								Bool if_nmi = (rg_interrupt_id == 28 || rg_interrupt_id == 29);
								Bool valid_interrupt = rg_interrupt_valid;
								rg_interrupt_valid <= False;
								return tuple2(valid_interrupt, if_nmi);
							endmethod
endmodule

interface Ifc_PLIC_AXI;
	interface AXI4_Slave_IFC#(`PADDR, `Reg_width,`USERSPACE) axi4_slave_plic;
	interface Vector#(`INTERRUPT_PINS,Ifc_global_interrupt) ifc_external_irq;
	method ActionValue#(Tuple2#(Bool,Bool)) intrpt_note;
	method ActionValue#(Bit#(TLog#(`INTERRUPT_PINS))) intrpt_completion;
endinterface

(*synthesize*)
//(*conflict_free="rl_config_plic_reg_write,intrpt_completion"*)
module mkplicperipheral(Ifc_PLIC_AXI);

AXI4_Slave_Xactor_IFC #(`PADDR, `Reg_width, `USERSPACE)  s_xactor_plic <- mkAXI4_Slave_Xactor;
Ifc_PLIC#(`PADDR, `DCACHE_WORD_SIZE, `INTERRUPT_PINS) plic <- mkplic();

(*preempts="rl_config_plic_reg_read, rl_config_plic_reg_write"*)
	rule rl_config_plic_reg_write;
		let aw <- pop_o(s_xactor_plic.o_wr_addr);
		let w <- pop_o(s_xactor_plic.o_wr_data);
		let w_strobe = w.wstrb;
		Bit#(3) byte_offset=0;
		for(Integer i=7; i >= 0; i=i-1) begin 
			if(w_strobe[i]==1)
				byte_offset=fromInteger(i);
		end
		let x <- plic.ifc_prog_reg.prog_reg(UncachedMemReq{address : aw.awaddr, transfer_size : 'd3, 
																							u_signed : 0, byte_offset : byte_offset, write_data : w.wdata, ld_st : Store}); 
		let w_resp = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: 0, bid : aw.awid}; //TODO user value is null
		s_xactor_plic.i_wr_resp.enq(w_resp);
	endrule

	rule rl_config_plic_reg_read;

		let ar <- pop_o(s_xactor_plic.o_rd_addr);
		let x <- plic.ifc_prog_reg.prog_reg(UncachedMemReq{address : ar.araddr, transfer_size : 'd3, 
	    																				u_signed : 0, byte_offset : 0, ld_st : Load}); 
        if(ar.arsize==3'd0)
            x = duplicate(x[7:0]);
        else if(ar.arsize==3'd1)
            x = duplicate(x[15:0]);
        else if(ar.arsize==3'd2)
            x = duplicate(x[31:0]);

		let r = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: x, rlast: True, ruser: 0, rid: ar.arid};
		s_xactor_plic.i_rd_data.enq(r);
	endrule

	interface axi4_slave_plic = s_xactor_plic.axi_side;
	interface ifc_external_irq = plic.ifc_external_irq;
	method ActionValue#(Tuple2#(Bool,Bool)) intrpt_note = plic.intrpt_note;
	method ActionValue#(Bit#(TLog#(`INTERRUPT_PINS))) intrpt_completion = plic.intrpt_completion;
	
endmodule
endpackage 
