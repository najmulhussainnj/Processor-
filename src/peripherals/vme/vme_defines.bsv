

package vme_defines;


typedef struct
{
Bit #(32) addr;
Bit #(32) wr_data;
Bit #(2)  mode;
Bit #(3) fun_code;
Bit #(1) rd_req;//HIGH FOR READ,LOW FOR WRITE
}Req_vme deriving (Bits,Eq);//Request from cpu

typedef struct

{

Bit#(2) port_type;
Bit#(32) data;
Bit#(1)berr ; //r0 for bus_error 1 for no error 
}Resp_vme deriving (Bits,Eq);//Response to cpu






endpackage
