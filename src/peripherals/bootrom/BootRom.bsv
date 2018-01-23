package BootRom;
	import defined_types::*;
	`include "defined_parameters.bsv"
  	import BRAMCore :: *;
	import DReg::*;
	import Semi_FIFOF        :: *;
	import AXI4_Types   :: *;
	import AXI4_Fabric  :: *;
	import BUtils::*;

interface BootRom_IFC;
	interface AXI4_Slave_IFC#(`PADDR,`Reg_width,`USERSPACE) axi_slave;
endinterface
typedef enum{Send_req,Get_resp} Mem_state deriving(Bits,Eq);
(*synthesize*)
module mkBootRom (BootRom_IFC);
	
	BRAM_PORT#(Bit#(13),Bit#(32)) dmemMSB <- mkBRAMCore1Load(valueOf(TExp#(13)),False,"boot.MSB",False);
	BRAM_PORT#(Bit#(13),Bit#(32)) dmemLSB <- mkBRAMCore1Load(valueOf(TExp#(13)),False,"boot.LSB",False);

	AXI4_Slave_Xactor_IFC #(`PADDR, `Reg_width, `USERSPACE)  s_xactor <- mkAXI4_Slave_Xactor;
	 Reg#(Mem_state) rg_state[2] <-mkCReg(2,Send_req);
	 Reg#(Bit#(8)) rg_readburst_counter<-mkReg(0);
	 Reg#(Bit#(8)) rg_readburst_value<-mkReg(0);
	 Reg#(Bit#(8)) rg_writeburst_counter<-mkReg(0);
	 Reg#(Bit#(4)) rg_id<-mkReg(0);
	 Reg#(Bit#(`PADDR)) rg_address<-mkReg(0);
	 Reg#(Bit#(3)) rg_transfer_size<-mkReg(0);

	rule rl_wr_respond;
      let aw <- pop_o (s_xactor.o_wr_addr);
      let w  <- pop_o (s_xactor.o_wr_data);
	   let b = AXI4_Wr_Resp {bresp: AXI4_SLVERR, buser: aw.awuser, bid:aw.awid};
     	s_xactor.i_wr_resp.enq (b);
		$display($time,"\tBootROM: Illegal Write operation on BootROM");
	endrule

	rule rl_rd_request(rg_state[1]==Send_req);
		  let ar<- pop_o(s_xactor.o_rd_addr);
			Bit#(13) index_address=(ar.araddr-`BootRomBase)[15:3];
			rg_address<=ar.araddr;
			rg_transfer_size<=ar.arsize;
			rg_readburst_value<=ar.arlen;
			rg_id<=ar.arid;
			dmemLSB.put(False,index_address,?);
			dmemMSB.put(False,index_address,?);
			rg_state[1]<=Get_resp;
			`ifdef verbose $display($time,"\tBootROM: Recieved Read Request for Address: %h Index Address: %h",ar.araddr,index_address);  `endif
	 endrule

	rule rl_rd_response(rg_state[0]==Get_resp);
	   Bit#(`Reg_width) data0 = {dmemMSB.read(),dmemLSB.read()};
      AXI4_Rd_Data#(`Reg_width,`USERSPACE) r = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: data0 ,rlast:rg_readburst_counter==rg_readburst_value, ruser: 0, rid:rg_id};
		if(rg_transfer_size==2)begin // 32 bit
			if(rg_address[2:0]==0)
				r.rdata=duplicate(data0[31:0]);
			else
				r.rdata=duplicate(data0[63:32]);
		end
      else if (rg_transfer_size=='d1)begin // half_word
			if(rg_address[2:0] ==0)
				r.rdata = duplicate(data0[15:0]);
			else if(rg_address[2:0] ==2)
				r.rdata = duplicate(data0[31:16]);
			else if(rg_address[2:0] ==4)
				r.rdata = duplicate(data0[47:32]);
			else if(rg_address[2:0] ==6)
				r.rdata = duplicate(data0[63:48]);
      end
      else if (rg_transfer_size=='d0) begin// one byte
			if(rg_address[2:0] ==0)
        	  r.rdata = duplicate(data0[7:0]);
        	else if(rg_address[2:0] ==1)
        	  r.rdata = duplicate(data0[15:8]);
        	else if(rg_address[2:0] ==2)
        	  r.rdata = duplicate(data0[23:16]);
        	else if(rg_address[2:0] ==3)
        	  r.rdata = duplicate(data0[31:24]);
        	else if(rg_address[2:0] ==4)
				r.rdata = duplicate(data0[39:32]);
        	else if(rg_address[2:0] ==5)
				r.rdata = duplicate(data0[47:40]);
        	else if(rg_address[2:0] ==6)
				r.rdata = duplicate(data0[55:48]);
        	else if(rg_address[2:0] ==7)
				r.rdata = duplicate(data0[63:56]);
      end
      s_xactor.i_rd_data.enq(r);
		rg_state[0]<=Send_req;
		if(rg_readburst_counter==rg_readburst_value)
			rg_readburst_counter<=0;
		else
			rg_readburst_counter<=rg_readburst_counter+1;
		Bit#(64) new_data=r.rdata;
		`ifdef verbose $display($time,"\tBootROM : Responding Read Request with Data: %8h BurstCounter: %d BurstValue: %d",new_data,rg_readburst_counter,rg_readburst_value);  `endif
   endrule

   interface axi_slave= s_xactor.axi_side;
endmodule
endpackage
