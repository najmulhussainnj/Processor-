
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

package Memory_vme_32;
	
	import defined_types::*;
       `include "defined_parameters.bsv"
  	import BRAMCore :: *;
	`include "vme_parameters.bsv"

//typedef enum{Send_req,Get_resp} Mem_state deriving(Bits,Eq);

typedef enum

{RCV_REQ,DET_DS,END_REQ
}State_slave deriving (Bits,Eq);
               
interface Vme_slave #(numeric type base_address,numeric type mem_size);
(*always_ready, always_enabled*)
method Action rd_as_l(Bit#(1) m_as_l);
(*always_ready, always_enabled*)
method Action rd_ds_l(Bit#(1)m_ds_l);
(*always_ready, always_enabled*)
method Action rd_siz0(Bit#(1)m_ds1_l);
(*always_ready, always_enabled*)
method Action rd_siz1(Bit#(1)m_siz1);
(*always_ready, always_enabled*)
method Action rd_addr(Bit#(32)m_addr);
(*always_ready, always_enabled*)
method Action rd_wr_l(Bit#(1)m_wr_l);
(*always_ready*)
method Bit#(1) wr_dsack_0_l();
(*always_ready*)
method Bit#(1) wr_dsack_1_l();
(*always_ready*)
method Bit#(1) wr_berr_l();
(*always_ready*)
method Bit#(1) wr_halt_l();

//.........Methods to write and read data when tristate is not enabled.........//


method Bit#(8) wr_byte_31_24();
method Bit#(8) wr_byte_23_16();
method Bit#(8) wr_byte_15_8();
method Bit#(8) wr_byte_7_0();

method Action rd_byte_31_24(Bit #(8) d3);
method Action rd_byte_23_16(Bit #(8) d2);
method Action rd_byte_15_8 (Bit #(8) d1);
method Action rd_byte_7_0  (Bit #(8) d0);

endinterface:Vme_slave

module mkMemory #(parameter String mem_init_file, parameter String module_name) (Vme_slave#(base_address,mem_size));
	
	BRAM_DUAL_PORT_BE#(Bit#(TSub#(mem_size,2)),Bit#(`Reg_width_vme_slave),TDiv#(`Reg_width_vme_slave,8)) dmemLSB <- mkBRAMCore2BELoad(valueOf(TExp#(TSub#(mem_size,2))),False,mem_init_file,False);

//Defining the slave interface lines

Reg #(Bit#(1)) s_dsack_0_l<-mkReg(1);
Reg #(Bit#(1)) s_dsack_1_l<-mkReg(1);
Reg #(Bit#(1)) s_berr_l<-mkReg(1);
Reg #(Bit#(1)) s_halt_l<-mkReg(1);
Wire #(Bit #(1)) s_as_l<-mkDWire(1);
Wire #(Bit #(1)) s_ds_l<-mkDWire(1);
Wire #(Bit #(1)) s_siz0<-mkDWire(0);
Wire #(Bit #(1)) s_siz1<-mkDWire(0);
Wire #(Bit #(32)) s_addr<-mkDWire(0);
Wire #(Bit #(1)) s_wr_l<-mkDWire(0);

Reg#(State_slave) slave_state <- mkReg (RCV_REQ);


//..........data_out registers of tristate buffers and their control......
//.......................................................................


Wire#(Bit#(8)) data_in_4<-mkDWire(0);
Wire#(Bit#(8)) data_in_3<-mkDWire(0);
Wire#(Bit#(8)) data_in_2<-mkDWire(0);
Wire#(Bit#(8)) data_in_1<-mkDWire(0);
Reg#(Bit#(8)) data_out_4<-mkReg(0);
Reg#(Bit#(8)) data_out_3<-mkReg(0);
Reg#(Bit#(8)) data_out_2<-mkReg(0);
Reg#(Bit#(8)) data_out_1<-mkReg(0);
Reg #(Bit#(4)) data_control <-mkReg(0);


/*In REQ_RCV State_slave
1.If read, sends acknowledge on detecting data strobe low
  puts the data into the data bus
2.If write,sends acknowledge on address strobe low
*/

Bool start_rd = (s_wr_l==1'b1)&&(s_ds_l==1'b0)&&(s_as_l==1'b0)&&(s_addr>=32'h40000000 && s_addr<=32'h4FFFFFFF);
Bool start_wr = (s_wr_l==1'b0)&&(s_as_l==1'b0)&&(s_addr>=32'h40000000 && s_addr <=32'h4FFFFFFF);
Bool store_data=(s_wr_l==1'b0)&&(s_ds_l==1'b0);

rule rcv_req((slave_state==RCV_REQ)&&(start_rd||start_wr));
        

        $display("............SELECTING SLAVE WITH PORT WIDTH 32...............");
       	if(start_wr)


	begin
		$display("Write_cycle");
		s_dsack_0_l<=1'b0;
		s_dsack_1_l<=1'b0;
                slave_state<=DET_DS;
	end
	else
		begin
 		$display("Starting read from address",$time);
		slave_state<=DET_DS;
		Bit#(TSub#(mem_size,2)) index_address=(s_addr-fromInteger(valueOf(base_address)))[valueOf(mem_size)-1:2];
                dmemLSB.b.put(0,index_address,?);
		$display("Index Address : %h",index_address);
		end
      
endrule

/*
1. Checks for data_strobe
2. Reads data if data_strobe is asserted
*/

 
rule send_ack(slave_state==DET_DS );
	if(s_wr_l==1'b1)
	begin
		s_dsack_0_l<=1'b0;
		s_dsack_1_l<=1'b0;		
		slave_state<=END_REQ;
    	Bit#(`Reg_width_vme_slave) data0 = dmemLSB.b.read();
	$display("32 bit data_read : %h",data0,$time);
                                case({s_siz1,s_siz0})

			 2'b00:
                               begin
					
					data_out_1 <=data0[31:24];
                               		data_out_2 <=data0[23:16];
                               		data_out_3 <=data0[15:8];
                               		data_out_4 <=data0[7:0];
					data_control<=4'b1111;
                               end
			2'b01 :
			       case({s_addr[1],s_addr[0]})
                               		2'b00:begin
						data_out_4<=data0[7:0];
						data_control<=4'b1111;
						$display("Reading data %h from %h addr",data0[7:0],s_addr);	
					      end
                               		2'b01:begin
						data_out_3<=data0[15:8];
						data_control<=4'b1111;
						$display("Reading data %h from %h addr",data0[15:8],s_addr);	
					      end
                               		2'b10:begin
						data_out_2<=data0[23:16];
					        data_control<=4'b1111;
						$display("Reading data %h from %h addr",data0[23:16],s_addr);	
						end
                               		2'b11:begin
						data_out_1<=data0[31:24];
					        data_control<=4'b1111;
						$display("Reading data %h from %h addr",data0[31:24],s_addr);	
                               			end

				endcase
			2'b10 :
			       if({s_addr[1],s_addr[0]}==2'b00)
                               begin
                               		data_out_3<=data0[15:8];
                               		data_out_4<=data0[7:0];
			       		data_control<=4'b1111;
			       end
			       else
                               begin 
                               		data_out_1<=data0[31:24];
                               		data_out_2<=data0[23:16];
			       		data_control<=4'b1111;
                               end                                  
                        endcase

	end
	  else	if(store_data)
		
		begin
		$display("Writing to addr %0d ",s_addr,$time); 
		slave_state<=END_REQ;
		Bit#(TSub#(mem_size,2)) index_address=(s_addr-fromInteger(valueOf(base_address)))[valueOf(mem_size)-1:2];
                $display("Index_address : %h",index_address); 	
			case({s_siz1,s_siz0})
                        2'b00 :begin
				dmemLSB.b.put(4'b1111,index_address,{data_in_1,data_in_2,data_in_3,data_in_4});
                               $display ("Data :%0h mode: quad_word",{data_in_1,data_in_2,data_in_3,data_in_4});
                               end
			2'b01 :begin
			       		case({s_addr[1],s_addr[0]})
						2'b00:begin
											

							dmemLSB.b.put(4'b0001,index_address,{24'b0,data_in_4});
							$display("Writing data %h",data_in_4);
							end 		
			       			2'b01:dmemLSB.b.put(4'b0010,index_address,{16'b0,data_in_3,8'b0});
			       			2'b10:dmemLSB.b.put(4'b0100,index_address,{8'b0,data_in_2,16'b0});
			       			2'b11:dmemLSB.b.put(4'b1000,index_address,{data_in_1,24'b0});
			       		endcase
                               end
			2'b10 :
                               if({s_addr[1],s_addr[0]}==2'b00)
			      	begin
					dmemLSB.b.put(4'b0011,index_address,{16'b0,data_in_3,data_in_4});
				end
			    	else 
				begin	
					dmemLSB.b.put(4'b1100,index_address,{data_in_1,data_in_2,16'b0});
                               end
                    endcase
                 end 

endrule

//Releases bus if data strobe is released//
rule end_req(slave_state==END_REQ);

if((s_ds_l==1) &&(s_as_l==1))
begin
s_dsack_0_l<=1;
s_dsack_1_l<=1;
s_berr_l<=1;
s_halt_l<=1;
data_control<=4'b0000;
$display("SLAVE_STATE:3 Releasing bus ",$time);
slave_state<=RCV_REQ;
end

endrule



method Action rd_as_l(Bit#(1) m_as_l);
s_as_l<=m_as_l;
endmethod

method Action rd_ds_l(Bit#(1)m_ds_l);
s_ds_l<=m_ds_l;
endmethod

method Action rd_siz0(Bit#(1)m_ds1_l);
s_siz0<=m_ds1_l;
endmethod

method Action rd_siz1(Bit#(1)m_siz1);
s_siz1<=m_siz1;
endmethod

method Action rd_addr(Bit #(32)m_addr);
s_addr<=m_addr;

endmethod

method Bit#(1) wr_dsack_0_l();
return s_dsack_0_l;
endmethod

method Bit#(1) wr_dsack_1_l();
return s_dsack_1_l;
endmethod

method Bit#(1)wr_berr_l();
return s_berr_l;
endmethod

method Bit#(1)wr_halt_l();
return s_halt_l;
endmethod


method Action rd_wr_l(m_wr_l);
s_wr_l<=m_wr_l;
endmethod

/*Methods to emulate tristate functionality*/


method Bit#(8) wr_byte_31_24()if(data_control[3]==1);
return data_out_4;
endmethod

method Bit#(8) wr_byte_23_16()if(data_control[2]==1);
return data_out_3;
endmethod

method Bit#(8) wr_byte_15_8()if(data_control[1]==1);
return data_out_2;
endmethod


method Bit#(8) wr_byte_7_0()if (data_control[0]==1);
return data_out_1;
endmethod

method Action rd_byte_31_24(Bit #(8) d4)if(data_control[3]==0);
data_in_4<=d4;
endmethod

method Action rd_byte_23_16(Bit #(8) d3)if(data_control[2]==0);
data_in_3<=d3;
endmethod

method Action rd_byte_15_8 (Bit #(8) d2)if(data_control[1]==0);
data_in_2<=d2;
endmethod

method Action rd_byte_7_0  (Bit #(8) d1)if(data_control[0]==0);
data_in_1<=d1;
endmethod


endmodule
endpackage
