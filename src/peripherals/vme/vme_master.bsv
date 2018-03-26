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
Generating VME master requests

*/

package vme_master;
//import TriState ::*;
import FIFOF ::*;
import vme_defines ::*;
//import Clocks ::*;



/*
 READ CYCLE


1.STATE 0  : receives req (function codes,r/w,siz1,siz0,addrbus) latched

2.STATE 1  : assert address strobe and data strobe

3. STATE 2 : Wait

4. STATE 3 : Check for acknowledgement

5. STATE 4 : latch data

6. STATE 5:  Release address strobe and data strobe(acknowledge lines will be released one cycle later)

 WRITE CYCLE


1.STATE 0: receives req (function codes,r/w,siz1,siz0,addrbus) latched

2.STATE 1 : assert address strobe

3. STATE 2 : place data on the bus and wait for acknowledgement

4. STATE 3 : assert data and strobe and enable required data lines

5. STATE 4 : No operation

6. STATE 5:Release address strobe and data strobe(acknowledge lines will be released one cycle later)READ CYCLE

Data_mode :01 byte
           10 Word
           00 Long word
*/

typedef enum


{RCV_REQ,PRC_REQ_1,PRC_REQ_2,PRC_REQ_3,LATCH_DATA,END_REQ,ADDR_INVALID,HALT
}State_master deriving (Bits,Eq);

typedef enum
{LW,BYTE,WORD,TRI_BYTE}Data_mode deriving (Bits,Eq);               

interface Vme_out;
	(*always_enabled,always_ready*)
	method Bit#(1) wr_as_l();
	(*always_enabled,always_ready*)
	method Bit#(1) wr_ds_l();
	(*always_enabled,always_ready*)
	method Bit#(1) wr_wr_l();
	(*always_ready*)
	method Action rd_dsack_0_l(Bit#(1) x);
	(*always_ready*)
	method Action rd_dsack_1_l(Bit#(1) y);
	(*always_ready*)
	method Action rd_berr_l(Bit#(1) z);
	(*always_ready*)
	method Action rd_halt_l(Bit#(1) u);
endinterface:Vme_out

interface Data_bus_inf;
 
	method Bit#(8) wr_byte_31_24();
	(*always_enabled,always_ready*)
	method Bit#(1) wr_siz1();
	(*always_enabled,always_ready*)
	method Bit#(1) wr_siz0();
	method Bool wr_mode_en();
	(*always_enabled,always_ready*)
	method Bit#(32) wr_addr();
	method Bool wr_addr_en();
	method Bit#(4)  wr_en();
	
	method Bit#(3) wr_fun_code();
	method Bool wr_fun_code_en();
	method Bit#(8) wr_byte_23_16();
	method Bit#(8) wr_byte_15_8();
	method Bit#(8) wr_byte_7_0();
	
	method Action rd_byte_31_24(Bit #(8) d3);
	method Action rd_byte_23_16(Bit #(8) d2);
	method Action rd_byte_15_8 (Bit #(8) d1);
	method Action rd_byte_7_0  (Bit #(8) d0);

endinterface

interface Vme_master ;
	method Action get_req (Req_vme req);
	interface Vme_out vme_interface;
	interface Data_bus_inf data_bus;
	method ActionValue #(Resp_vme) resp();
endinterface:Vme_master

(*synthesize*)

module mkvmemaster(Vme_master);


//Fifos to get request and send response
FIFOF #(Req_vme) cpu_req <- mkFIFOF;
FIFOF #(Resp_vme) cpu_resp <- mkFIFOF;
//............................................................//

Reg #(State_master) master_state <- mkReg (RCV_REQ);
Reg#(Bit#(2)) mode<-mkReg(0);
Reg#(Bool) mode_en<-mkReg(False);//Enable SIZ1,SIZ0
Reg#(Bit#(3)) fun_code<-mkReg(0);
Reg#(Bool) fun_code_en<-mkReg(False);
Reg#(Bit#(1))  as_l<-mkReg(1);//address_stop
Reg#(Bit#(1))  ds_l<-mkReg(1);//data_strobe
Reg#(Bit#(1))  stop<-mkReg(0);//Indicates processor stopped
Reg#(Bit#(1))  retry<-mkReg(0);//Indicates retry mode
Wire#(Bit#(1)) dsack_0_l<-mkDWire(1);//ack from VIC controller,indicates port size 0-32 bit port,1-8 bit port,2-16 bit port
Wire#(Bit#(1)) dsack_1_l<-mkDWire(1);
Wire#(Bit#(1)) berr_l <-mkDWire(1);//Bus error from slave
Wire#(Bit#(1)) halt_l<-mkDWire(1);//halt signal from the slave
//Wire#(Bit#(3)) ipl_l<-mkDWire(1);//interrupt prioritylevel

Reg #(Bit#(32)) addr<-mkReg(0);//addr_bus
Reg #(Bool) addr_en<-mkReg(False);
Reg #(Bit#(8)) data_out_4<-mkReg(0);//wr_data_4
Reg #(Bit#(8)) data_out_3<-mkReg(0);//wr_data_3
Reg #(Bit#(8)) data_out_2<-mkReg(0);//wr_data_2
Reg #(Bit#(8)) data_out_1<-mkReg(0);//wr_data_1

Wire #(Bit#(8)) data_in_4<-mkDWire(0);//rd_data_4
Wire #(Bit#(8)) data_in_3<-mkDWire(0);//rd_data_3
Wire #(Bit#(8)) data_in_2<-mkDWire(0);//rd_data_2
Wire #(Bit#(8)) data_in_1<-mkDWire(0);//rd_data_1


Reg #(Bit#(1))  wr_l <-mkReg(1'd1);//read : 1,write:0
Reg #(Bit#(4)) data_control <-mkReg(0);//Enable bits to data and control registers
Reg #(Bit#(2)) cntl_wd<-mkReg(0); //To synchronize if more than one cycle is required to r/w data
//.........Tristate signals...............//
/*

/*
*/
(* mutually_exclusive = "rcv_req_new,prc_req,prc_req_1,prc_req_2,req_wait,end_req" *)
  
/* REQ_RCV master_state....
1. Master receives request
2. address,FC2-FC0,Data,Write lines are driven
*/
rule rcv_req_new(master_state==RCV_REQ && cntl_wd==2'b00 && (halt_l==1'b1) && (berr_l==1'b1)) ;//To recieve new request

	let req =cpu_req.first();
 	$display("MASTER_STATE 1: receiving request to address %h req_type:%h",req.addr,req.rd_req,$time);
        cpu_req.deq();
 	addr<=req.addr;
	addr_en<=True;
	fun_code_en<=True;
	mode_en<=True;
        mode<=req.mode;
	$display("mode of operation %b",req.mode);
        fun_code<=req.fun_code;
        wr_l<=req.rd_req;
	master_state<=PRC_REQ_1;

//............Multiplexing logic to route data to data bus......................................................................................................
       
	 if (req.mode==2'b01)//For 8 bit data operations
        case({req.addr[1],req.addr[0]})
        
	2'b00:data_out_4<=req.wr_data[7:0];

	2'b01:begin
	      data_out_4<=req.wr_data[7:0];
	      data_out_3<=req.wr_data[7:0];
	      end	

        2'b10:begin
	      data_out_4<=req.wr_data[7:0];
	      data_out_2<=req.wr_data[7:0];
	      end	
        2'b11:begin 
		data_out_4<=req.wr_data[7:0];
	      	data_out_3<=req.wr_data[7:0];
	      	data_out_1<=req.wr_data[7:0];
		end
	endcase
        

	else  if (req.mode==2'b10)//For 16 data operations
        case({req.addr[1],req.addr[0]})
        
	2'b00:begin
	
	      data_out_4<=req.wr_data[7:0];
	      data_out_3<=req.wr_data[15:8];
		
		end

        2'b10: begin
		data_out_4<=req.wr_data[7:0];
		data_out_3<=req.wr_data[15:8];
	      	data_out_1<=req.wr_data[15:8];
	      	data_out_2<=req.wr_data[7:0];
	 	end
	endcase
        
       else  if (req.mode==2'b00)//for 32 bit operation
                
        case({req.addr[1],req.addr[0]})
        
	2'b00:
	      begin
		data_out_1<=req.wr_data[31:24];
		data_out_2<=req.wr_data[23:16];
		data_out_3<=req.wr_data[15:8];
		data_out_4<=req.wr_data[7:0];
		$display("Data to be written = %h",req.wr_data);
		end
         endcase
//..............................................................................................................................................................
endrule

rule prc_req(master_state==RCV_REQ && cntl_wd!=2'b00);//If a request takes multiple cycles

 if(cntl_wd==2'b01)//32 bit operation from 8 bit or 16 bit slave


	begin	
	
	data_out_4<=data_out_3;
	data_out_3<=data_out_2;
	data_out_2<=data_out_1;
	data_out_1<=8'b0;
        addr     <=addr+1;
        mode     <=mode-1;
	end
else
	begin//32 bit operation on a 16 bit slave

	data_out_4<=data_out_2;
	data_out_3<=data_out_1;
	data_out_2<=8'b0;
	data_out_1<=8'b0;
        addr     <= addr+2;
        mode     <=2'b10;
        end  
	master_state<=PRC_REQ_1;
	
endrule
/*
InPRC_REQ_1....

1.Address strobe is asserted

2.Data Strobe is asserted if it is a read cycle

3. Data lines are enabled for write cycle


*/

rule prc_req_1(master_state==PRC_REQ_1);
                
            
                 
		$display("MASTER_STATE 2: Activating address strobe",$time);
                 as_l<=0;
		master_state<=PRC_REQ_2; 
		ds_l<=0;
		if(wr_l==1'b0)
                data_control<=4'b1111;
	
endrule
/*
In PRC_REQ_2
1. Data strobe is asserted in write  cycle on receiving ack,halt or bus error cycle
*/
rule prc_req_2(master_state==PRC_REQ_2);		
	
	begin
		 $display("MASTER_STATE_3 ",$time);
	if (({dsack_0_l,dsack_1_l}!=2'b11)||(berr_l==1'b0)||(halt_l==1'b0))
                      begin
        	 	master_state<=PRC_REQ_3;
			end
                 
                 			
	end
endrule
/*In WAIT_RESP
1. Master waits for response
2. On receiving response,goes to next state
*/

rule req_wait((master_state==PRC_REQ_3)&&(({dsack_0_l,dsack_1_l}!=2'b11)||(berr_l==1'b0)||(halt_l==1'b0)));   
        
//	if (wr_l==1'b0)
        
//	ds_l<=0;

	master_state<=LATCH_DATA;
         $display(" MASTER_STATE_4:",$time); 
	if({dsack_0_l,dsack_1_l}==mode)   
        begin
//	        $display("detected response %h",$time);
//		master_state<=LATCH_DATA;
                cntl_wd<=2'b00;//Done can take the next request
	end
//if a 32 bit word r/w  fro/to  a 16 bit port      
	else if({dsack_0_l,dsack_1_l}==2'b10 && mode==2'b00)
        	
		begin	
			cntl_wd<=2'b10;
                        //  mode <=2'b10;
		end
		
// If a 32 bit word or 1 16 bit is  r/w  fro/to  a 8 bit port   	 	
	else if({dsack_0_l,dsack_1_l}==2'b01 && mode!=2'b01)

                  begin
                       cntl_wd<=2'b01;
                        //  mode<=mode-1;
		  end
                 
                 		 

endrule
//.......................Latch_data when not using tristatebuffers.............................................................................
//In LATCH DATA state
//Latches on-to data and response
rule latch_data(master_state==LATCH_DATA);

if(berr_l==1'b0 && halt_l==1'b0)
begin
retry<=1;
end

else if(berr_l!=1'b0 && halt_l==1'b0)
begin
stop<=1;
end

else if(berr_l==1'b0)
begin			


let resp_data = Resp_vme{ data:{data_in_4,
				      data_in_3,
				      data_in_2,8'b0},berr:1};
		  cpu_resp.enq(resp_data);
end


else
if (mode==2'b01 )//Receiving 8 bit data
	case({dsack_0_l,dsack_1_l})
//......................................SLAVE PORT..............................//:-32 byte
//.......................................................................................
          2'b00 :

	case({addr[1],addr[0]})//Position where byte read from depends on A1,A0          
        	2'b00 :begin
			$display("Master recieved data %h addr:00",data_in_4);
			let resp_data = Resp_vme{ data:{24'b0,
					data_in_4},berr:0,port_type:{dsack_0_l,dsack_1_l}};
			 cpu_resp.enq(resp_data); 
			end
        	2'b01 :begin
			let resp_data = Resp_vme{ data:{24'b0,

					data_in_3},berr:0,port_type:{dsack_0_l,dsack_1_l}};
			 cpu_resp.enq(resp_data);
			$display("Master recieved data %h addr 01",data_in_3);
			end
        	2'b10 :begin
			let resp_data = Resp_vme{ data:{24'b0,
					data_in_2},berr:0,port_type:{dsack_0_l,dsack_1_l}};
                        
			$display("Master recieved data %h addr :10",data_in_2);
			 cpu_resp.enq(resp_data);
		        end
        	2'b11 :begin
			let resp_data = Resp_vme{ data:{24'b0,
					data_in_1},berr:0,port_type:{dsack_0_l,dsack_1_l}};
			
			
			
			$display("Master recieved data %h addr: 11",data_in_1);
			 cpu_resp.enq(resp_data);
			end
        endcase
       //If slave is an 8 bit port		
        2'b01  : begin

		let resp_data = Resp_vme{data:{24'b0,data_in_4
						},berr:0,port_type:{dsack_0_l,dsack_1_l}};
                cpu_resp.enq(resp_data);

		end
        //if slave is a 16 bit port
        2'b10 :
		if (addr[0]==1'b0)
		begin
		let resp_data =Resp_vme{ data:{24'b0,data_in_4
					},berr:0,port_type:{dsack_0_l,dsack_1_l}};      
                 
	        cpu_resp.enq(resp_data);
                end
		else

		begin
		
		let resp_data =Resp_vme{ data:{24'b0,data_in_3
					},berr:0,port_type:{dsack_0_l,dsack_1_l}};      
                 
	        cpu_resp.enq(resp_data);
		end
endcase
else
 if(mode==2'b10)//Receiving 16 bit data

	case({dsack_0_l,dsack_1_l})
          //...........SLAVE PORT....................:-32 byte
          2'b00 :

	case({addr[1],addr[0]})//Position where byte read from depends on A1,A0          
        	2'b00 :begin
			let resp_data = Resp_vme{ data:{16'b0,data_in_3,
					data_in_4},berr:0,port_type:{dsack_0_l,dsack_1_l}};
			 cpu_resp.enq(resp_data); 
			end
        	
        	2'b10 :begin
			let resp_data = Resp_vme{ data:{16'b0,data_in_1,
					data_in_2},berr:0,port_type:{dsack_0_l,dsack_1_l}};
                        
			 cpu_resp.enq(resp_data);
		        end
        endcase
       //If slave is an 8 bit port		
        2'b01  :begin

		let resp_data = Resp_vme{data:{24'b0,data_in_4
						},berr:0,port_type:{dsack_0_l,dsack_1_l}};
                cpu_resp.enq(resp_data);

		end
        //if slave is a 16 bit port
        2'b10 :

		begin
		let resp_data =Resp_vme{ data:{16'b0,data_in_3,
					data_in_4},berr:0,port_type:{dsack_0_l,dsack_1_l}};      
                 
	        cpu_resp.enq(resp_data);
                end
 	endcase
else
	if(mode==2'b00)//Receiving 32 bit data

	case({dsack_0_l,dsack_1_l})
          //...........SLAVE PORT....................:-32 bit port
        	2'b00 :begin
		
		let resp_data = Resp_vme{ data:{data_in_1,
			data_in_2,data_in_3,data_in_4},berr:0,port_type:{dsack_0_l,dsack_1_l}};
		$display("Master recieved data %h",{data_in_1,data_in_2,data_in_3,data_in_4});
			 cpu_resp.enq(resp_data); 
			
		end
         //.......................................16 bit port	
        	2'b10 :begin
			let resp_data = Resp_vme{ data:{16'b0,data_in_3,
					data_in_4},berr:0,port_type:{dsack_0_l,dsack_1_l}};
                        
			 cpu_resp.enq(resp_data);
		        end
        
       //........................If slave is an 8 bit port		
       		 2'b01  : begin
 
		let resp_data = Resp_vme{data:{24'b0,data_in_4
						},berr:0,port_type:{dsack_0_l,dsack_1_l}};
                cpu_resp.enq(resp_data);

  //              $display("Data_latched",data_in_4);

		end
           
 
	endcase
else
begin

		let resp_data = Resp_vme{data:{24'b0,data_in_4
						},berr:0,port_type:{dsack_0_l,dsack_1_l}};
                cpu_resp.enq(resp_data);

   
end
	
	master_state<=END_REQ;


endrule
/*
1.Releases address and data strobe and data bus

2. DSACK lines are deasserted one cycle after

One more stage should be inserted here
*/

rule end_req(master_state==END_REQ);

	$display("\tMASTER_STATE_6:Releasing data strobes",$time);
       		ds_l<=1;
        	as_l<=1;
        	data_control<=4'b0000;
              master_state<=ADDR_INVALID; 
	endrule	

rule rel_addr(master_state==ADDR_INVALID);//Release the address and data bus when transfer is finished

	if(stop==0 && retry==0)
			begin
				if(((dsack_0_l!=0)&&(dsack_1_l!=0))||(berr_l!=1'b0))
				master_state<=RCV_REQ;
				if(cntl_wd==2'b00)
	
				begin
					$display("\tMASTER_STATE_7:Releasing address strobes",$time);
					addr_en<=False;
					mode_en<=False;
					fun_code_en<=False;
				end
	
                		$display("Ready to receive a new request",$time);
	
			end

                else
                		master_state<=HALT;

endrule
	

rule stop_wait(master_state==HALT);

//If HALT.....resume when halt signal is negated
if(stop==1)
	if (halt_l!=0)
	begin
		stop<=0;
		master_state<=RCV_REQ;
		$display("\tMASTER_STATE_7:Releasing address strobes",$time);
		addr_en<=False;
		mode_en<=False;
		fun_code_en<=False;

	end
	else 
		master_state<=HALT;
	

//If Retry.........start retry cycle when bus error and halt are negated
else
       if(halt_l!=0 && berr_l!=0)
       begin
	    retry<=0;
	    master_state<=PRC_REQ_1;
	end
	else
	
		master_state<=HALT;


endrule

//Methods for driving signals in and out of master


interface Vme_out vme_interface;
	

	method Bit#(1) wr_as_l();
	return as_l;
	endmethod
	
	method Bit#(1) wr_ds_l();
	return ds_l;
	endmethod
	
	
	method Bit#(1) wr_wr_l();
	return wr_l;
	endmethod
	
	
	method Action rd_dsack_0_l(Bit#(1) x);
	dsack_0_l<=x;
	endmethod
	
	method Action rd_dsack_1_l(Bit#(1) y);
	dsack_1_l<=y;
	endmethod
	
	method Action rd_berr_l(Bit#(1) z);
	berr_l<=z;
	endmethod
	
	method Action rd_halt_l(Bit#(1) u);
	halt_l<=u;
	endmethod
endinterface

/*Methods to emulate tristate functionality*/

interface Data_bus_inf data_bus;
	

	method Bit#(1) wr_siz0();
	return mode[0];
	endmethod
	
	method Bit#(1) wr_siz1();
	return mode[1];
	endmethod
	
	method Bit#(32) wr_addr();
	return addr;
	endmethod
	method Bit#(8) wr_byte_31_24();
	return data_out_4;
	endmethod
	
	method Bit#(8) wr_byte_23_16();
	return data_out_3;
	endmethod
	
	method Bit#(8) wr_byte_15_8();
	return data_out_2;
	endmethod
	
	method Bit#(3) wr_fun_code();
	return fun_code;
	endmethod

	method Bit#(8) wr_byte_7_0();
	return data_out_1;
	endmethod
	
	method Action rd_byte_31_24(Bit #(8) d4);
	data_in_4<=d4;
	endmethod
	
	method Action rd_byte_23_16(Bit #(8) d3);
	data_in_3<=d3;
	endmethod
	
	method Action rd_byte_15_8 (Bit #(8) d2);
	data_in_2<=d2;
	endmethod
	
	method Action rd_byte_7_0  (Bit #(8) d1);
	data_in_1<=d1;
	endmethod
	

	method Bit#(4) wr_en();
	return data_control;
	endmethod
				
	method Bool wr_mode_en();
	return mode_en;
	endmethod


	method Bool wr_addr_en();
	return addr_en;
	endmethod


	method Bool wr_fun_code_en();
	return fun_code_en;
	endmethod


endinterface

method Action get_req (Req_vme req) if(!(cpu_req.notEmpty()));
cpu_req.enq(req);
endmethod

method ActionValue #(Resp_vme) resp();
cpu_resp.deq();
return cpu_resp.first();
endmethod



endmodule

endpackage













 








