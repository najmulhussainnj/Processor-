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
/*
Transalates  AXI master request to VME master request
*/
`define PADDR   56
package vme_top;

	/* ======== Package imports ======= */
	import Vector						:: *;
	import FIFO						:: *;
	import ConfigReg					::*;
	import  AXI4_Types  ::*;
	import  AXI4_Fabric ::*;
	import  Semi_FIFOF ::*;
    	import BUtils::*;
	/*================================== */

	/*========= Project imports ======== */
	`include "vme_parameters.bsv"
	`include "defined_parameters.bsv"
	import defined_types	::*;
	import FIFOF					::*;
	import vme_master :: *;
        import vme_defines :: *;
	/*================================== */


	interface Ifc_vme_top;
        interface Vme_out proc_ifc;
	interface  Data_bus_inf proc_dbus;
	interface AXI4_Slave_IFC#(`PADDR,`Reg_width,`USERSPACE) slave_axi_vme;
	method Action rd_ipl(Bit#(3) ip);
		/*-============================================================================= */
	endinterface

	typedef enum {DATA,INST} Priority_cache deriving (Bits, Eq, FShow);
	typedef enum {DATA_MODE_8_READ,DATA_MODE_16_READ,DATA_MODE_32_READ,DATA_MODE_8_WRITE,DATA_MODE_16_WRITE,DATA_MODE_32_WRITE,INST_MODE} Data_mode deriving (Bits, Eq, FShow);

      function Bit#(2) modeconv_vme(Bit#(3) transfer_size );
         if(transfer_size==0)//8 bit transfer
           	return 2'b01;
         else if (transfer_size==1)//16 bit transfer
	   	return 2'b10;
          else
		return 2'b00;//32 bit transfer
       endfunction
          
	(*synthesize*)
	module mkvme_top(Ifc_vme_top);
		
		AXI4_Slave_Xactor_IFC #(`PADDR,`Reg_width,`USERSPACE) s_xactor <- mkAXI4_Slave_Xactor;
	        Vme_master proc_master <-mkvmemaster;
		Reg#(Bit#(`Reg_width_vme_slave)) response_buff  <-mkReg(0);//To buffer multiple cycle transfers
                FIFOF#(Bit#(4)) ff_id <-mkSizedFIFOF(2);//To store request address of instruction
                FIFOF#(Bit#(`Reg_width_vme_slave)) ff_address <-mkSizedFIFOF(2);//To store request address of instruction
		FIFOF#(Data_mode) ff_req<-mkFIFOF;//To keep track of last pending request
                Reg#(Bit#(2)) port_count <- mkReg(0);
		Reg#(Bit#(32)) inst_rcvd <- mkReg(0);
//.....................................................SEND_REQUEST_TO_MEMORY.........................................................................................................................//
//....................................................................................................................................................................................................// 

	
		rule check_wr_request_to_memory;
    		let info<-pop_o(s_xactor.o_wr_addr);
		let data<-pop_o(s_xactor.o_wr_data);
		let request=Req_vme{addr:truncate(info.awaddr),wr_data:truncate(data.wdata),mode:modeconv_vme(info.awsize),fun_code:3'b010,rd_req:0};
		if(info.awaddr[27:0]==28'hFFFF_FFF)
		request.fun_code=3'b111;	
		proc_master.get_req(request);
    		ff_id.enq(info.awid);
		$display("Enqueing request onto VME");
		ff_address.enq(info.awaddr);
			case(request.mode)
			2'b00 :ff_req.enq(DATA_MODE_32_WRITE);
			2'b01 :ff_req.enq(DATA_MODE_8_WRITE);
			2'b10 :ff_req.enq(DATA_MODE_16_WRITE);
			endcase

       		endrule


		rule check_read_request_to_memory;
    		let info<- pop_o(s_xactor.o_rd_addr);
    		ff_id.enq(info.arid);
		ff_address.enq(info.araddr);
		let request=Req_vme{addr:truncate(info.araddr),wr_data:?,mode:modeconv_vme(info.arsize),fun_code:3'b010,rd_req:1};
	
		case(request.mode)
		2'b00 :ff_req.enq(DATA_MODE_32_READ);
		2'b01 :ff_req.enq(DATA_MODE_8_READ);
		2'b10 :ff_req.enq(DATA_MODE_16_READ);
		endcase

		proc_master.get_req(request);
			
        	endrule

	(* preempts = "check_read_request_to_memory,check_wr_request_to_memory"*)

//...............................................................SEND RESPONSE TO MEMORY................................................................................................................//
//
// Data is retreived from a Big-Endian memory and sent back in little Endian format

	(* mutually_exclusive="send_read_response_from_memory_to_mem_stage_8,send_read_response_from_memory_to_mem_stage_16,send_read_response_from_memory_to_mem_stage_32,send_write_response_from_memory_to_mem_stage_8,send_write_response_from_memory_to_mem_stage_16,send_write_response_from_memory_to_mem_stage_32"*)


             	rule send_read_response_from_memory_to_mem_stage_8(ff_req.first==DATA_MODE_8_READ); 
			let response <-proc_master.resp();
			ff_req.deq();
                        let r = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata:duplicate(response.data[7:0]),rlast:True, ruser: 0, rid: ff_id.first };
    			if(response.berr==1'b1)
			r.rresp = AXI4_SLVERR;
		//	if(ff_address.first[4]==1)
		//	r.rdata={response.data,32'b0};
			ff_address.deq();
			s_xactor.i_rd_data.enq(r);
			$display("Data received %h from address %h to mem_stage",response.data,ff_id.first());
		       	ff_id.deq();
		endrule
                
             	rule send_read_response_from_memory_to_mem_stage_16(ff_req.first==DATA_MODE_16_READ);
			let response <-proc_master.resp();
			if(response.port_type==2'b10 )
				begin
					ff_req.deq();
                        		let r = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata:duplicate(response.data[15:0]),rlast:True, ruser: 0, rid: ff_id.first };
    					if(response.berr==1'b1)
					r.rresp = AXI4_SLVERR;
 
				//	if(ff_address.first[4]==1)
				//	r.rdata={response.data,32'b0};
					ff_address.deq();
					s_xactor.i_rd_data.enq(r);
					//let resp = To_Cpu_Mem {data_word:zeroExtend(response.data),bus_error:response.berr==1'b1,misaligned_error:1'b0,address:ff_id.first(),load_store:Load};
		       			ff_id.deq();
					$display("Data received %h from address %h to mem_stage",response.data,ff_id.first());
				end						
			else if(response.port_type==2'b01)
				if(port_count==0)
				begin
					response_buff<=response.data;
					port_count<=port_count+1;
				end
				else
				begin	
					
					ff_req.deq();
                        		let r = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata:duplicate({response.data[7:0],response_buff[7:0]}),rlast:True,ruser: 0,rid: ff_id.first };
    					if(response.berr==1'b1)
					r.rresp = AXI4_SLVERR; 
					port_count<=0;
				//	if(ff_address.first[4]==1)
				//	r.rdata={16'b0,{response.data[7:0],response_buff[7:0]},32'b0};
					ff_address.deq();
					s_xactor.i_rd_data.enq(r);
					response_buff<=0;
					//let resp = To_Cpu_Mem {data_word:zeroExtend({response.data[7:0],response_buff[7:0]}),bus_error:response.berr==1'b1,misaligned_error:1'b0,address:ff_id.first(						),load_store:Load};
		       			ff_id.deq();
					$display("Data received %h from address %h to mem_stage",{response.data[7:0],response_buff[7:0]},ff_id.first());

				end

			else
					
				begin
				
					ff_req.deq();
					port_count<=0;
					response_buff<=0;
                        		let r = AXI4_Rd_Data{rresp: AXI4_OKAY, rdata:duplicate(response.data[15:0]),rlast:True,ruser: 0,rid: ff_id.first };
    					if(response.berr==1'b1)
					r.rresp = AXI4_SLVERR;
	//				if(ff_address.first[4]==1)
	//				r.rdata={response.data,32'b0};
					ff_address.deq();
        				s_xactor.i_rd_data.enq(r);
			       		ff_id.deq();
					$display("Data received %h from address %h mem_stage",{response.data[15:0]},ff_id.first());
				end
		endrule


             	rule send_read_response_from_memory_to_mem_stage_32(ff_req.first==DATA_MODE_32_READ); 
			let response <-proc_master.resp();
			if(response.port_type==2'b00)
			begin		
				ff_req.deq();
                        	let r = AXI4_Rd_Data{rresp: AXI4_OKAY, rdata:duplicate({response.data}),rlast:True,ruser: 0,rid: ff_id.first };
    				if(response.berr==1'b1)
				r.rresp = AXI4_SLVERR;
	//			if(ff_address.first[4]==1)
	//			r.rdata={response.data,32'b0};
				ff_address.deq;
            			s_xactor.i_rd_data.enq(r);
		       		ff_id.deq();
				$display("Data received %h from address %h from mem_stage",response.data,ff_id.first());
			end


			else if(response.port_type==2'b10)
				
			if(port_count==0)
			begin
				response_buff<=response.data;
				port_count<=port_count+1;
			end
			else

			begin	
			
		       		ff_id.deq();
				ff_req.deq();
				port_count<=0;
				response_buff<=0;
                        	let r = AXI4_Rd_Data{rresp: AXI4_OKAY, rdata:duplicate({response.data[15:0],response_buff[15:0]}),rlast:True,ruser: 0,rid: ff_id.first };
    				if(response.berr==1'b1)
				r.rresp = AXI4_SLVERR;
	//			if(ff_address.first[4]==1)
	//			r.rdata={{response.data[15:0],response_buff[15:0]},32'b0};
				ff_address.deq;
            			s_xactor.i_rd_data.enq(r);
				$display("Data received %h from address %h from mem_stage",{response.data[15:0],response_buff[15:0]},ff_id.first());

			end

			else if(response.port_type==2'b01)
			begin

				if(port_count<3)
					begin
						response_buff<={response.data[7:0],response_buff[23:0]};
						port_count<=port_count+1;
					end
				else
					begin
					
					ff_req.deq();
					port_count<=0;
					response_buff<=0;
                        		let r = AXI4_Rd_Data{rresp: AXI4_OKAY, rdata:duplicate({response.data[7:0],response_buff[23:0]}),rlast:True,ruser: 0,rid: ff_id.first };
    					if(response.berr==1'b1)
					r.rresp = AXI4_SLVERR; 
	//				if(ff_address.first[4]==1)
	//				r.rdata={{response.data[15:0],response_buff[15:0]},32'b0};
					ff_address.deq;
					s_xactor.i_rd_data.enq(r);
		       			ff_id.deq();

		   		$display("Data received %h from address %h to mem_stage",{response.data[7:0],response_buff[23:0]},ff_id.first());
  		
					end

			end
		endrule




	/*	rule send_read_response_from_memory_to_fetch(ff_req.first==INST_MODE); //MEMORY READ RESP TO ICACHE
			let response <-proc_master.resp();
			if(response.port_type==2'b00)
			begin		
				ff_req.deq();
		       		ff_id_imem.deq();
				inst_rcvd <= response.data;
				let resp = To_Cpu{data_word:zeroExtend(response.data),bus_error:response.berr==1'b1,misaligned_error:1'b0,address:ff_id_imem.first()}; 	
				$display("Data received %h from address %h to fetch_stage",response.data,ff_id_imem.first());
			end


			else if(response.port_type==2'b10)
				
			if(port_count==0)
			begin
				response_buff<=response.data;
				port_count<=port_count+1;
			end
			else
			begin	
		       		ff_id_imem.deq();
				ff_req.deq();
				port_count<=0;
				response_buff<=0;
				inst_rcvd <= {response.data[15:0],response_buff[15:0]};
				let resp= To_Cpu{data_word:zeroExtend({response.data[15:0],response_buff[15:0]}),bus_error:response.berr==1'b1,misaligned_error:1'b0,address:ff_id_imem.first()}; 	
				$display("Data received %h from address %h to fetch_stage",{response.data[15:0],response_buff[15:0]},ff_id_imem.first());

			end
		endrule
*/

		rule send_write_response_from_memory_to_mem_stage_8(ff_req.first==DATA_MODE_8_WRITE);//MEMORY WRITE RESP TO DCACHE

			let response <-proc_master.resp();
			ff_req.deq();
		        let resp =  AXI4_Wr_Resp {bresp: AXI4_OKAY,  bid: ff_id.first};
			if (response.berr==1'b1)
			resp.bresp=AXI4_SLVERR;
			ff_id.deq();
			
			ff_address.deq();
	    		s_xactor.i_wr_resp.enq(resp);
			`ifdef verbose $display($time,"\t CORE: Received Write Response:"); `endif
		endrule
                
             	rule send_write_response_from_memory_to_mem_stage_16(ff_req.first==DATA_MODE_16_WRITE);
			let response <-proc_master.resp();
			$display("Received respons from port_type %h",response.port_type);
			if(response.port_type==2'b10 )
				begin
					ff_req.deq();
		       			ff_id.deq();
					`ifdef verbose $display($time,"CORE: Received Write Response:"); `endif
			//let resp = To_Cpu_Mem {data_word:?,bus_error:response.berr==1'b1,misaligned_error:1'b0,address:ff_id.first(),load_store:Store};
		        let resp =  AXI4_Wr_Resp {bresp: AXI4_OKAY, bid: ff_id.first};
			
			ff_address.deq();
			if (response.berr==1'b1)
			resp.bresp=AXI4_SLVERR;
	    		s_xactor.i_wr_resp.enq(resp);

				end						
			else if(response.port_type==2'b01)
				if(port_count==0)
				begin
					port_count<=port_count+1;
				end
				else

				begin	
			
				ff_address.deq();
		       	//		ff_id.deq();
					ff_req.deq();
					port_count<=0;
					`ifdef verbose $display($time,"CORE: Received Write Response:"); `endif
			///		let resp = To_Cpu_Mem {data_word:?,bus_error:response.berr==1'b1,misaligned_error:1'b0,address:ff_id.first(),load_store:Store};
		        let resp =  AXI4_Wr_Resp {bresp: AXI4_OKAY, bid: ff_id.first};
				if (response.berr==1'b1)
				resp.bresp=AXI4_SLVERR;
  					ff_id.deq();
	    				s_xactor.i_wr_resp.enq(resp);

				end
			
			else
					
				begin
					
					ff_address.deq();
		       	//		ff_id.deq();
					ff_req.deq();
					port_count<=0;
					response_buff<=0;
					
					`ifdef verbose $display($time,"CORE: Received Write Response:"); `endif
					//let resp = To_Cpu_Mem {data_word:?,bus_error:response.berr==1'b1,misaligned_error:1'b0,address:ff_id.first(),load_store:Store};
		         		let resp =  AXI4_Wr_Resp {bresp: AXI4_OKAY, bid: ff_id.first};
					if (response.berr==1'b1)
					resp.bresp=AXI4_SLVERR;
					ff_id.deq();
	    				s_xactor.i_wr_resp.enq(resp);

				end

		endrule

             	rule send_write_response_from_memory_to_mem_stage_32(ff_req.first==DATA_MODE_32_WRITE); 
			let response <-proc_master.resp();
			if(response.port_type==2'b00)
			begin		
				ff_req.deq();
		//       




				
				`ifdef verbose $display($time,"CORE: Received Write Response:"); `endif

				//let resp = To_Cpu_Mem {data_word:?,bus_error:response.berr==1'b1,misaligned_error:1'b0,address:ff_id.first(),load_store:Store};

		         		let resp =  AXI4_Wr_Resp {bresp: AXI4_OKAY, bid: ff_id.first};
					if (response.berr==1'b1)
					resp.bresp=AXI4_SLVERR;
					ff_id.deq();
					ff_address.deq();
	    			s_xactor.i_wr_resp.enq(resp);
			end

			else if(response.port_type==2'b10)
				
			if(port_count==0)
			begin
				port_count<=port_count+1;
			end
			else

			begin	
			
		      // 		ff_id.deq();
				ff_req.deq();
				ff_address.deq();
				port_count<=0;

			ff_id.deq();
		         		let resp =  AXI4_Wr_Resp {bresp: AXI4_OKAY, bid: ff_id.first};
					if (response.berr==1'b1)
					resp.bresp=AXI4_SLVERR;
	    			s_xactor.i_wr_resp.enq(resp);
			//	let resp = To_Cpu_Mem {data_word:?,bus_error:response.berr==1'b1,misaligned_error:1'b0,address:ff_id.first(),load_store:Store};
				
				`ifdef verbose $display($time,"CORE: Received Write Response:"); `endif

			end
			else if(response.port_type==2'b01)
			begin
				if(port_count<3)
					begin
						port_count<=port_count+1;
					end
				else
					begin
					
		     //  			ff_id.deq();
					ff_req.deq();
					ff_address.deq();
					port_count<=0;
		         		let resp =  AXI4_Wr_Resp {bresp: AXI4_OKAY, bid: ff_id.first};
					if (response.berr==1'b1)
					resp.bresp=AXI4_SLVERR;
					ff_id.deq();
//					let resp = To_Cpu_Mem {data_word:?,bus_error:response.berr==1'b1,misaligned_error:1'b0,address:ff_id.first(),load_store:Store};
	    				s_xactor.i_wr_resp.enq(resp);
  						
					`ifdef verbose $display($time,"CORE: Received Write Response:"); `endif
					end
			end
		endrule				
		interface proc_ifc  = proc_master.vme_interface;
		interface proc_dbus = proc_master.data_bus;
	        interface slave_axi_vme = s_xactor.axi_side;
endmodule
endpackage
